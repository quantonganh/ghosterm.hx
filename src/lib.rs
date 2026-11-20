use std::io::Write;
use std::sync::{Arc, atomic::AtomicBool, atomic::Ordering};

use abi_stable::std_types::{RBoxError, RResult, RVec};
use anyhow::{Context, Result};
use futures::future::Either;
use futures::lock::Mutex as AsyncMutex;
use libghostty_vt::render::{CellIterator, RenderState, RowIterator};
use libghostty_vt::style::Underline;
use libghostty_vt::terminal::{ScrollViewport, Terminal};
use portable_pty::{Child, CommandBuilder, MasterPty, NativePtySystem, PtySize, PtySystem};
use steel::declare_module;
use steel::rvals::Custom;
use steel::steel_vm::ffi::{
    FFIModule, FFIValue, FfiFuture, FfiFutureExt, IntoFFIVal, RegisterFFIFn,
};
use tokio::sync::mpsc;

const ATTR_BOLD: u16 = 1 << 0;
const ATTR_ITALIC: u16 = 1 << 1;
const ATTR_FAINT: u16 = 1 << 2;
const ATTR_BLINK: u16 = 1 << 3;
const ATTR_INVERSE: u16 = 1 << 4;
const ATTR_INVISIBLE: u16 = 1 << 5;
const ATTR_STRIKETHROUGH: u16 = 1 << 6;
const ATTR_OVERLINE: u16 = 1 << 7;
const ATTR_UNDERLINE: u16 = 1 << 8;
const SELECTION_BIAS: isize = 1_000_000;

fn to_rbox(err: impl std::fmt::Display) -> RBoxError {
    RBoxError::new(std::io::Error::other(err.to_string()))
}

fn pack_color(color: libghostty_vt::style::RgbColor) -> isize {
    (((color.r as isize) << 16) | ((color.g as isize) << 8) | (color.b as isize)) as isize
}

struct Pty {
    master: Box<dyn MasterPty + Send>,
    child: Box<dyn Child + Send + Sync>,
    receiver: Arc<AsyncMutex<mpsc::UnboundedReceiver<Vec<u8>>>>,
    writer: Arc<parking_lot::Mutex<Box<dyn Write + Send>>>,
    dead: Arc<AtomicBool>,
}

unsafe impl Send for Pty {}
unsafe impl Sync for Pty {}

impl Custom for Pty {}

impl Pty {
    fn write_bytes(&mut self, bytes: &[u8]) -> bool {
        let mut writer = self.writer.lock();
        match writer.write_all(bytes).and_then(|_| writer.flush()) {
            Ok(()) => true,
            Err(_) => false,
        }
    }

    fn send_command(&mut self, command: &str) -> bool {
        self.write_bytes(command.as_bytes())
    }

    fn send_command_char(&mut self, c: char) -> bool {
        let mut buf = [0u8; 4];
        let encoded = c.encode_utf8(&mut buf);
        self.write_bytes(encoded.as_bytes())
    }

    fn resize(&mut self, rows: usize, cols: usize) -> RResult<FFIValue, RBoxError> {
        let size = PtySize {
            rows: rows.max(1) as u16,
            cols: cols.max(1) as u16,
            pixel_width: 0,
            pixel_height: 0,
        };
        match self.master.resize(size) {
            Ok(()) => RResult::ROk(FFIValue::Void),
            Err(err) => RResult::RErr(to_rbox(err)),
        }
    }

    fn kill(&mut self) {
        self.dead.store(true, Ordering::SeqCst);
        self.child.kill().ok();
    }

    fn alive(&self) -> bool {
        !self.dead.load(Ordering::SeqCst)
    }

    fn async_read(&mut self) -> FfiFuture<RResult<FFIValue, RBoxError>> {
        let receiver = Arc::clone(&self.receiver);

        async move {
            let mut guard = receiver.lock().await;

            let mut buffer: Vec<u8> = Vec::new();
            while let Ok(chunk) = guard.try_recv() {
                buffer.extend_from_slice(&chunk);
            }

            if !buffer.is_empty() {
                return RResult::ROk(FFIValue::ByteVector(buffer.into()));
            }

            let next = guard.recv();
            let timeout = futures_time::task::sleep(futures_time::time::Duration::from_millis(4));

            futures::pin_mut!(next);

            match futures::future::select(next, timeout).await {
                Either::Left((Some(chunk), _)) => {
                    buffer.extend_from_slice(&chunk);
                    RResult::ROk(FFIValue::ByteVector(buffer.into()))
                }
                Either::Left((None, _)) => RResult::ROk(FFIValue::BoolV(false)),
                Either::Right((_, pending)) => match pending.await {
                    Some(chunk) => RResult::ROk(FFIValue::ByteVector(chunk.into())),
                    None => RResult::ROk(FFIValue::BoolV(false)),
                },
            }
        }
        .into_ffi()
    }

    #[cfg(unix)]
    fn foreground_pgid(&self) -> RResult<FFIValue, RBoxError> {
        let Some(fd) = self.master.as_raw_fd() else {
            return RResult::ROk(FFIValue::BoolV(false));
        };

        let pgid = unsafe { libc::tcgetpgrp(fd) };

        if pgid > 0 {
            RResult::ROk(FFIValue::IntV(pgid as isize))
        } else {
            RResult::ROk(FFIValue::BoolV(false))
        }
    }
}

impl Drop for Pty {
    fn drop(&mut self) {
        self.kill();
    }
}

fn default_shell() -> String {
    std::env::var("SHELL").unwrap_or_else(|_| "/bin/sh".to_string())
}

fn create_pty(shell: &str, rows: usize, cols: usize) -> RResult<FFIValue, RBoxError> {
    match create_pty_impl(shell, rows, cols) {
        Ok(pty) => RResult::ROk(pty.into_ffi_val().expect("Pty is always convertible")),
        Err(err) => RResult::RErr(to_rbox(err)),
    }
}

fn create_pty_impl(shell: &str, rows: usize, cols: usize) -> Result<Pty> {
    let pty_system = NativePtySystem::default();
    let pair = pty_system
        .openpty(PtySize {
            rows: rows.max(1) as u16,
            cols: cols.max(1) as u16,
            pixel_width: 0,
            pixel_height: 0,
        })
        .context("unable to open pty: {err}")?;

    let mut command = CommandBuilder::new(shell);
    command.env("TERM", "xterm-256color");
    command.env("COLORTERM", "truecolor");
    if let Ok(cwd) = std::env::current_dir() {
        command.cwd(cwd);
    }

    let child = pair
        .slave
        .spawn_command(command)
        .context("unable to spawn shell")?;

    let mut reader = pair
        .master
        .try_clone_reader()
        .context("unable to clone pty reader")?;
    let writer = pair
        .master
        .take_writer()
        .context("unable to take pty writer: {err}")?;

    let master = pair.master;
    drop(pair.slave);

    let (sender, receiver) = mpsc::unbounded_channel::<Vec<u8>>();
    let dead = Arc::new(AtomicBool::new(false));
    let reader_dead = Arc::clone(&dead);

    std::thread::spawn(move || {
        let mut buffer = [0u8; 65536];
        loop {
            match reader.read(&mut buffer) {
                Ok(0) => break,
                Ok(n) => {
                    if sender.send(buffer[..n].to_vec()).is_err() {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
        reader_dead.store(true, Ordering::SeqCst);
    });

    Ok(Pty {
        master,
        child,
        receiver: Arc::new(AsyncMutex::new(receiver)),
        writer: Arc::new(parking_lot::Mutex::new(writer)),
        dead,
    })
}

#[derive(Clone, Default)]
struct CellData {
    text: String,
    fg: isize,
    bg: isize,
    attrs: u16,
}

struct VTerm {
    term: Terminal<'static, 'static>,
    state: RenderState<'static>,
    cells: Vec<CellData>,
    rows: u16,
    cols: u16,
    cursor: Option<(u16, u16)>,
    cursor_visible: bool,
    default_fg: isize,
    default_bg: isize,
    iter: Option<usize>,
}

unsafe impl Send for VTerm {}
unsafe impl Sync for VTerm {}

impl Custom for VTerm {}

impl VTerm {
    fn new(pty: &Pty, rows: usize, cols: usize) -> Result<Self> {
        let cols = cols.max(1) as u16;
        let rows = rows.max(1) as u16;

        let mut term = Terminal::new(cols, rows).context("unable to create terminal")?;
        term.set_scrollback_max_lines(Some(10000))
            .context("unable to set scrollback")?;
        term.set_title("ghosterm").context("unable to set title")?;
        term.resize(cols, rows, 8, 16)
            .context("unable to resize terminal")?;

        let writer = Arc::clone(&pty.writer);
        term.on_pty_write(move |_term, data| {
            let mut writer = writer.lock();
            let _ = writer.write_all(data);
            let _ = writer.flush();
        })
        .context("unable to register pty write callback")?;

        let state = RenderState::new().context("unable to create render state")?;

        Ok(Self {
            term,
            state,
            cells: Vec::new(),
            rows,
            cols,
            cursor: None,
            cursor_visible: false,
            default_fg: -1,
            default_bg: -1,
            iter: None,
        })
    }

    fn refresh(&mut self) {
        let mut new_cells: Vec<CellData> = Vec::new();
        let mut cursor = None;
        let mut cursor_visible = false;
        let mut default_fg = self.default_fg;
        let mut default_bg = self.default_bg;
        let mut cols = self.cols;

        {
            let snapshot = match self.state.update(&self.term) {
                Ok(snapshot) => snapshot,
                Err(_) => return,
            };

            if let Ok(colors) = snapshot.colors() {
                default_fg = pack_color(colors.foreground);
                default_bg = pack_color(colors.background);
            }

            if let Ok(cur) = snapshot.cursor() {
                cursor_visible = cur.visible;
                if let Some(viewport) = cur.viewport {
                    cursor = Some((viewport.x, viewport.y));
                }
            }

            if let Ok(snapshot_cols) = snapshot.cols() {
                if snapshot_cols > 0 {
                    cols = snapshot_cols;
                }
            }
            let rows = snapshot.rows().unwrap_or(self.rows);

            let Ok(mut row_iterator) = RowIterator::new() else {
                return;
            };
            let Ok(mut cell_iterator) = CellIterator::new() else {
                return;
            };
            let Ok(mut rows_iteration) = row_iterator.update(&snapshot) else {
                return;
            };

            while let Some(row) = rows_iteration.next() {
                let mut in_row = 0u16;
                {
                    if let Ok(mut cells) = cell_iterator.update(row) {
                        while let Some(cell) = cells.next() {
                            let mut text = String::new();
                            let _ = cell.graphemes_utf8(&mut text);

                            let fg = cell.fg_color().ok().flatten().map(pack_color).unwrap_or(-1);
                            let bg = cell.bg_color().ok().flatten().map(pack_color).unwrap_or(-1);

                            let mut attrs = 0u16;
                            if let Ok(style) = cell.style() {
                                if style.bold {
                                    attrs |= ATTR_BOLD;
                                }
                                if style.italic {
                                    attrs |= ATTR_ITALIC;
                                }
                                if style.faint {
                                    attrs |= ATTR_FAINT;
                                }
                                if style.blink {
                                    attrs |= ATTR_BLINK;
                                }
                                if style.inverse {
                                    attrs |= ATTR_INVERSE;
                                }
                                if style.invisible {
                                    attrs |= ATTR_INVISIBLE;
                                }
                                if style.strikethrough {
                                    attrs |= ATTR_STRIKETHROUGH;
                                }
                                if style.overline {
                                    attrs |= ATTR_OVERLINE;
                                }
                                if !matches!(style.underline, Underline::None) {
                                    attrs |= ATTR_UNDERLINE;
                                }
                            }

                            new_cells.push(CellData {
                                text,
                                fg,
                                bg,
                                attrs,
                            });
                            in_row += 1;
                        }
                    }
                }

                while in_row < cols {
                    new_cells.push(CellData::default());
                    in_row += 1;
                }
            }

            let _ = rows;
        }

        self.default_fg = default_fg;
        self.default_bg = default_bg;
        self.cols = cols;
        self.cursor = cursor;
        self.cursor_visible = cursor_visible;
        self.cells = new_cells;
        self.iter = None;
    }

    fn advance_bytes(&mut self, data: RVec<u8>) -> bool {
        self.term.vt_write(&data);
        true
    }

    fn resize(&mut self, rows: usize, cols: usize) -> RResult<FFIValue, RBoxError> {
        let cols = cols.max(1) as u16;
        let rows = rows.max(1) as u16;
        match self.term.resize(cols, rows, 8, 16) {
            Ok(()) => {
                self.cols = cols;
                self.rows = rows;
                self.refresh();
                RResult::ROk(FFIValue::Void)
            }
            Err(err) => RResult::RErr(to_rbox(err)),
        }
    }

    fn reset_iterator(&mut self) {
        self.refresh();
        self.iter = None;
    }

    fn advance_iterator(&mut self) -> bool {
        let next = match self.iter {
            None => 0,
            Some(index) => index + 1,
        };

        if next < self.cells.len() {
            self.iter = Some(next);
            true
        } else {
            self.iter = Some(next);
            false
        }
    }

    fn iter_x(&self) -> isize {
        match self.iter {
            Some(index) if self.cols > 0 => (index % self.cols as usize) as isize,
            _ => -1,
        }
    }

    fn iter_y(&self) -> isize {
        match self.iter {
            Some(index) if self.cols > 0 => (index / self.cols as usize) as isize,
            _ => -1,
        }
    }

    fn iter_cell_str(&self) -> String {
        self.current()
            .map(|cell| cell.text.clone())
            .unwrap_or_default()
    }

    fn iter_cell_fg(&self) -> isize {
        self.current().map(|cell| cell.fg).unwrap_or(-1)
    }

    fn iter_cell_bg(&self) -> isize {
        self.current().map(|cell| cell.bg).unwrap_or(-1)
    }

    fn iter_cell_attrs(&self) -> isize {
        self.current().map(|cell| cell.attrs as isize).unwrap_or(0)
    }

    fn selection_contains(x: isize, y: isize, x1: isize, y1: isize, x2: isize, y2: isize) -> bool {
        if y < y1 || y > y2 {
            return false;
        }
        if y1 == y2 {
            x >= x1 && x < x2
        } else if y == y1 {
            x >= x1
        } else if y == y2 {
            x < x2
        } else {
            true
        }
    }

    fn render_runs(
        &mut self,
        has_selection: bool,
        x1: usize,
        y1: usize,
        x2: usize,
        y2: usize,
    ) -> Vec<FFIValue> {
        self.refresh();

        let x1 = x1 as isize - SELECTION_BIAS;
        let y1 = y1 as isize - SELECTION_BIAS;
        let x2 = x2 as isize - SELECTION_BIAS;
        let y2 = y2 as isize - SELECTION_BIAS;
        let cols = self.cols.max(1) as usize;
        let mut runs: Vec<(usize, usize, isize, isize, u16, String, bool, usize)> = Vec::new();

        for (index, cell) in self.cells.iter().enumerate() {
            if cell.text.is_empty() {
                continue;
            }

            let x = index % cols;
            let y = index / cols;
            let selected =
                has_selection && Self::selection_contains(x as isize, y as isize, x1, y1, x2, y2);

            if let Some(last) = runs.last_mut() {
                if last.1 == y
                    && last.0 + last.7 == x
                    && last.2 == cell.fg
                    && last.3 == cell.bg
                    && last.4 == cell.attrs
                    && last.6 == selected
                {
                    last.5.push_str(&cell.text);
                    last.7 += 1;
                    continue;
                }
            }

            runs.push((
                x,
                y,
                cell.fg,
                cell.bg,
                cell.attrs,
                cell.text.clone(),
                selected,
                1,
            ));
        }

        let mut result = Vec::with_capacity(runs.len() * 7);
        for (x, y, fg, bg, attrs, text, selected, _) in runs {
            result.push(FFIValue::IntV(x as isize));
            result.push(FFIValue::IntV(y as isize));
            result.push(FFIValue::IntV(fg));
            result.push(FFIValue::IntV(bg));
            result.push(FFIValue::IntV(attrs as isize));
            result.push(text.into());
            result.push(FFIValue::IntV(if selected { 1 } else { 0 }));
        }
        result
    }

    fn current(&self) -> Option<&CellData> {
        self.iter.and_then(|index| self.cells.get(index))
    }

    fn cursor_x(&self) -> isize {
        self.cursor.map(|(x, _)| x as isize).unwrap_or(-1)
    }

    fn cursor_y(&self) -> isize {
        self.cursor.map(|(_, y)| y as isize).unwrap_or(-1)
    }

    fn cursor_visible(&self) -> bool {
        self.cursor_visible
    }

    fn scroll_offset(&self) -> isize {
        self.term
            .scrollbar()
            .map(|scrollbar| scrollbar.offset as isize)
            .unwrap_or(0)
    }

    fn scroll_up(&mut self, amount: isize) {
        self.term
            .scroll_viewport(ScrollViewport::Delta(-amount.max(0)));
        self.refresh();
    }

    fn scroll_down(&mut self, amount: isize) {
        self.term
            .scroll_viewport(ScrollViewport::Delta(amount.max(0)));
        self.refresh();
    }

    fn scroll_top(&mut self) {
        self.term.scroll_viewport(ScrollViewport::Top);
        self.refresh();
    }

    fn scroll_bottom(&mut self) {
        self.term.scroll_viewport(ScrollViewport::Bottom);
        self.refresh();
    }
}

fn virtual_terminal(pty: &Pty, rows: usize, cols: usize) -> RResult<FFIValue, RBoxError> {
    match VTerm::new(pty, rows, cols) {
        Ok(vterm) => RResult::ROk(vterm.into_ffi_val().expect("VTerm is always convertible")),
        Err(err) => RResult::RErr(to_rbox(err)),
    }
}

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("ghosterm");

    module
        .register_fn("default-shell", default_shell)
        .register_fn("create-pty", create_pty)
        .register_fn("pty-write", Pty::send_command)
        .register_fn("pty-write-char", Pty::send_command_char)
        .register_fn("pty-resize", Pty::resize)
        .register_fn("pty-kill", Pty::kill)
        .register_fn("pty-alive?", Pty::alive)
        .register_fn("pty-read", Pty::async_read)
        .register_fn("pty-foreground-process", Pty::foreground_pgid)
        .register_fn("virtual-terminal", virtual_terminal)
        .register_fn("vte/advance-bytes", VTerm::advance_bytes)
        .register_fn("vte/resize", VTerm::resize)
        .register_fn("vte/reset-iterator!", VTerm::reset_iterator)
        .register_fn("vte/advance-iterator!", VTerm::advance_iterator)
        .register_fn("vte/iter-x", VTerm::iter_x)
        .register_fn("vte/iter-y", VTerm::iter_y)
        .register_fn("vte/iter-cell-str", VTerm::iter_cell_str)
        .register_fn("vte/iter-cell-fg", VTerm::iter_cell_fg)
        .register_fn("vte/iter-cell-bg", VTerm::iter_cell_bg)
        .register_fn("vte/iter-cell-attrs", VTerm::iter_cell_attrs)
        .register_fn("vte/render-runs", VTerm::render_runs)
        .register_fn("vte/cursor-x", VTerm::cursor_x)
        .register_fn("vte/cursor-y", VTerm::cursor_y)
        .register_fn("vte/cursor-visible?", VTerm::cursor_visible)
        .register_fn("vte/scroll-offset", VTerm::scroll_offset)
        .register_fn("vte/scroll-up", VTerm::scroll_up)
        .register_fn("vte/scroll-down", VTerm::scroll_down)
        .register_fn("vte/scroll-top", VTerm::scroll_top)
        .register_fn("vte/scroll-bottom", VTerm::scroll_bottom);

    module
}
