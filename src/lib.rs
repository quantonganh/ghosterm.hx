use std::cell::{Cell, RefCell};
#[cfg(unix)]
use std::fs;
use std::io::Write;
#[cfg(unix)]
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::{Arc, atomic::AtomicBool, atomic::Ordering};
use std::time::{Duration, Instant};

use abi_stable::std_types::{RBoxError, RResult, RVec};
use anyhow::{Context, Result};
use futures::future::Either;
use futures::lock::Mutex as AsyncMutex;
use libghostty_vt::render::{CellIterator, RenderState, RowIterator};
use libghostty_vt::style::Underline;
use libghostty_vt::terminal::{Mode, ScrollViewport, Terminal};
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
const RENDER_HOLD_TIMEOUT: Duration = Duration::from_secs(1);

fn to_rbox(err: impl std::fmt::Display) -> RBoxError {
    RBoxError::new(std::io::Error::other(err.to_string()))
}

fn pack_color(color: libghostty_vt::style::RgbColor) -> isize {
    (((color.r as isize) << 16) | ((color.g as isize) << 8) | (color.b as isize)) as isize
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn abbreviate_cwd(path: &str) -> String {
    let home = std::env::var("HOME").unwrap_or_default();
    let path = if !home.is_empty() && path.starts_with(&home) {
        let rest = &path[home.len()..];
        if rest.is_empty() {
            "~".to_string()
        } else if rest.starts_with('/') {
            format!("~{}", rest)
        } else {
            path.to_string()
        }
    } else {
        path.to_string()
    };

    let absolute = path.starts_with('/');
    let parts: Vec<&str> = path.split('/').filter(|part| !part.is_empty()).collect();
    if parts.is_empty() {
        return if absolute { "/".to_string() } else { path };
    }

    let last = parts.len() - 1;
    let mut result = String::new();
    if absolute && !path.starts_with('~') {
        result.push('/');
    }
    for (index, part) in parts.iter().enumerate() {
        if index > 0 {
            result.push('/');
        }
        if index == last {
            result.push_str(part);
        } else if let Some(ch) = part.chars().next() {
            result.push(ch);
        }
    }
    result
}

#[cfg(target_os = "linux")]
fn process_for_pgid(pgid: i32) -> Option<i32> {
    let entries = std::fs::read_dir("/proc").ok()?;
    for entry in entries.flatten() {
        let name = entry.file_name();
        let name = name.to_string_lossy();
        let Ok(pid) = name.parse::<i32>() else {
            continue;
        };
        let Ok(stat) = std::fs::read_to_string(entry.path().join("stat")) else {
            continue;
        };
        let Some(close) = stat.rfind(')') else {
            continue;
        };
        let mut fields = stat[close + 1..].split_whitespace();
        let Some(_state) = fields.next() else {
            continue;
        };
        let Some(_ppid) = fields.next() else {
            continue;
        };
        let Some(pgrp) = fields.next().and_then(|value| value.parse::<i32>().ok()) else {
            continue;
        };
        if pgrp == pgid {
            return Some(pid);
        }
    }
    None
}

#[cfg(target_os = "linux")]
fn process_comm(pid: i32) -> String {
    let comm = std::fs::read_to_string(format!("/proc/{}/comm", pid))
        .ok()
        .map(|value| value.trim().to_string())
        .filter(|value| !value.is_empty());
    if comm.is_some() {
        return comm.unwrap();
    }

    let Ok(stat) = std::fs::read_to_string(format!("/proc/{}/stat", pid)) else {
        return String::new();
    };
    match (stat.find('('), stat.rfind(')')) {
        (Some(start), Some(end)) if start < end => stat[start + 1..end].to_string(),
        _ => String::new(),
    }
}

#[cfg(target_os = "linux")]
fn process_cwd(pid: i32) -> String {
    if let Ok(path) = std::fs::read_link(format!("/proc/{}/cwd", pid)) {
        let value = path.to_string_lossy().into_owned();
        if !value.is_empty() {
            return value;
        }
    }

    let Ok(environ) = std::fs::read(format!("/proc/{}/environ", pid)) else {
        return String::new();
    };
    for entry in environ.split(|byte| *byte == 0) {
        if entry.starts_with(b"PWD=") {
            return String::from_utf8_lossy(&entry[4..]).into_owned();
        }
    }
    String::new()
}

#[cfg(target_os = "linux")]
fn child_pids(pid: i32) -> Vec<i32> {
    let mut result = Vec::new();
    let Ok(tasks) = std::fs::read_dir(format!("/proc/{}/task", pid)) else {
        return result;
    };
    for task in tasks.flatten() {
        let Ok(children) = std::fs::read_to_string(task.path().join("children")) else {
            continue;
        };
        for value in children.split_whitespace() {
            if let Ok(child) = value.parse::<i32>() {
                result.push(child);
            }
        }
    }
    result
}

#[cfg(target_os = "linux")]
fn descendant_pids(root: i32) -> Vec<i32> {
    let mut result = Vec::new();
    let mut queue = vec![root];
    let mut seen = Vec::new();

    while let Some(pid) = queue.pop() {
        if seen.contains(&pid) {
            continue;
        }
        seen.push(pid);
        for child in child_pids(pid) {
            result.push(child);
            queue.push(child);
        }
    }

    result
}

#[cfg(target_os = "macos")]
fn bsd_info(pid: i32) -> Option<libc::proc_bsdinfo> {
    let mut info: libc::proc_bsdinfo = unsafe { std::mem::zeroed() };
    let size = std::mem::size_of::<libc::proc_bsdinfo>() as i32;
    let result = unsafe {
        libc::proc_pidinfo(
            pid,
            libc::PROC_PIDTBSDINFO,
            0,
            &mut info as *mut _ as *mut libc::c_void,
            size,
        )
    };
    if result == size { Some(info) } else { None }
}

#[cfg(target_os = "macos")]
fn process_comm(pid: i32) -> String {
    let Some(info) = bsd_info(pid) else {
        return String::new();
    };
    for name in [info.pbi_name.as_slice(), info.pbi_comm.as_slice()] {
        let value = unsafe { std::ffi::CStr::from_ptr(name.as_ptr()) }
            .to_string_lossy()
            .trim()
            .to_string();
        if !value.is_empty() {
            return value;
        }
    }
    String::new()
}

#[cfg(target_os = "macos")]
fn process_cwd(pid: i32) -> String {
    let mut info: libc::proc_vnodepathinfo = unsafe { std::mem::zeroed() };
    let size = std::mem::size_of::<libc::proc_vnodepathinfo>() as i32;
    let result = unsafe {
        libc::proc_pidinfo(
            pid,
            libc::PROC_PIDVNODEPATHINFO,
            0,
            &mut info as *mut _ as *mut libc::c_void,
            size,
        )
    };
    if result != size {
        return String::new();
    }
    unsafe { std::ffi::CStr::from_ptr(info.pvi_cdir.vip_path.as_ptr() as *const libc::c_char) }
        .to_string_lossy()
        .into_owned()
}

#[cfg(target_os = "macos")]
fn process_for_pgid(pgid: i32) -> Option<i32> {
    let mut pids = vec![0i32; 4096];
    let count = unsafe {
        libc::proc_listallpids(
            pids.as_mut_ptr() as *mut libc::c_void,
            (pids.len() * std::mem::size_of::<i32>()) as i32,
        )
    };
    for pid in pids.into_iter().take(count.max(0) as usize) {
        if pid > 0 && bsd_info(pid).map(|info| info.pbi_pgid as i32) == Some(pgid) {
            return Some(pid);
        }
    }
    None
}

#[cfg(target_os = "macos")]
fn child_pids(pid: i32) -> Vec<i32> {
    let mut children = vec![0i32; 1024];
    let count = unsafe {
        libc::proc_listchildpids(
            pid,
            children.as_mut_ptr() as *mut libc::c_void,
            (children.len() * std::mem::size_of::<i32>()) as i32,
        )
    };
    children
        .into_iter()
        .take(count.max(0) as usize)
        .filter(|pid| *pid > 0)
        .collect()
}

#[cfg(target_os = "macos")]
fn descendant_pids(root: i32) -> Vec<i32> {
    let mut result = Vec::new();
    let mut queue = vec![root];
    let mut seen = Vec::new();

    while let Some(pid) = queue.pop() {
        if seen.contains(&pid) {
            continue;
        }
        seen.push(pid);
        for child in child_pids(pid) {
            result.push(child);
            queue.push(child);
        }
    }

    result
}

struct Pty {
    master: Box<dyn MasterPty + Send>,
    child: Box<dyn Child + Send + Sync>,
    receiver: Arc<AsyncMutex<mpsc::UnboundedReceiver<Vec<u8>>>>,
    writer: Arc<parking_lot::Mutex<Box<dyn Write + Send>>>,
    dead: Arc<AtomicBool>,
    shell_pgid: Option<i32>,
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

    #[cfg(unix)]
    fn foreground_is_shell(&self) -> bool {
        let Some(fd) = self.master.as_raw_fd() else {
            return false;
        };

        let pgid = unsafe { libc::tcgetpgrp(fd) };
        pgid > 0 && self.shell_pgid == Some(pgid as i32)
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    fn foreground_title(&self) -> String {
        let Some(fd) = self.master.as_raw_fd() else {
            return String::new();
        };

        let pgid = unsafe { libc::tcgetpgrp(fd) };
        let shell_pid = self.child.process_id().map(|pid| pid as i32);
        let mut pids: Vec<i32> = Vec::new();
        if let Some(pid) = shell_pid {
            for child in child_pids(pid) {
                if !pids.contains(&child) {
                    pids.push(child);
                }
            }
        }
        if pgid > 0 {
            if let Some(pid) = process_for_pgid(pgid) {
                if !pids.contains(&pid) {
                    pids.push(pid);
                }
            }
            if !pids.contains(&pgid) {
                pids.push(pgid);
            }
        }
        if let Some(pid) = shell_pid {
            for child in descendant_pids(pid) {
                if !pids.contains(&child) {
                    pids.push(child);
                }
            }
            if !pids.contains(&pid) {
                pids.push(pid);
            }
        }

        let mut name = String::new();
        let mut cwd = String::new();
        let mut title_pid = None;
        let mut fallback_name = String::new();
        let mut fallback_pid = None;
        for pid in pids {
            let candidate_name = process_comm(pid);
            let candidate_cwd = process_cwd(pid);
            if fallback_name.is_empty() && !candidate_name.is_empty() {
                fallback_name = candidate_name.clone();
                fallback_pid = Some(pid);
            }
            if !candidate_cwd.is_empty() {
                name = candidate_name;
                cwd = candidate_cwd;
                title_pid = Some(pid);
                break;
            }
        }
        if name.is_empty() {
            name = fallback_name;
            title_pid = fallback_pid;
        }

        if title_pid.map(|pid| Some(pid) == shell_pid || self.shell_pgid == Some(pid)) == Some(true)
        {
            name.clear();
        }

        let cwd = abbreviate_cwd(&cwd);
        match (name.is_empty(), cwd.is_empty()) {
            (true, true) => String::new(),
            (true, false) => cwd,
            (false, true) => name,
            (false, false) => format!("{} {}", name, cwd),
        }
    }

    #[cfg(all(unix, not(any(target_os = "linux", target_os = "macos"))))]
    fn foreground_title(&self) -> String {
        String::new()
    }

    #[cfg(not(unix))]
    fn foreground_title(&self) -> String {
        String::new()
    }
}

impl Drop for Pty {
    fn drop(&mut self) {
        self.kill();
    }
}

#[cfg(unix)]
fn ensure_terminfo_dir() -> Option<PathBuf> {
    const TERMINFO_XTERM_256COLOR: &[u8] = include_bytes!("../terminfo/x/xterm-256color");

    let cache_root = std::env::var_os("XDG_CACHE_HOME")
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("HOME").map(|home| PathBuf::from(home).join(".cache")))
        .unwrap_or_else(|| std::env::temp_dir().join("ghosterm-cache"));
    let directory = cache_root.join("ghosterm").join("terminfo");
    let target_dir = directory.join("x");
    fs::create_dir_all(&target_dir).ok()?;

    let target = target_dir.join("xterm-256color");
    if fs::read(&target).ok().as_deref() != Some(TERMINFO_XTERM_256COLOR) {
        fs::write(&target, TERMINFO_XTERM_256COLOR).ok()?;
    }

    Some(directory)
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
    #[cfg(unix)]
    if let Some(terminfo_dir) = ensure_terminfo_dir() {
        let mut terminfo_dirs = terminfo_dir.as_os_str().to_os_string();
        terminfo_dirs.push(":");
        if let Some(existing) = std::env::var_os("TERMINFO_DIRS") {
            terminfo_dirs.push(existing);
        }
        command.env("TERMINFO_DIRS", terminfo_dirs);
    }
    if let Ok(cwd) = std::env::current_dir() {
        command.cwd(cwd);
    }

    let child = pair
        .slave
        .spawn_command(command)
        .context("unable to spawn shell")?;

    #[cfg(unix)]
    let shell_pgid = child.process_id().and_then(|pid| {
        let pgid = unsafe { libc::getpgid(pid as libc::pid_t) };
        if pgid > 0 { Some(pgid as i32) } else { None }
    });
    #[cfg(not(unix))]
    let shell_pgid = None;

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
        shell_pgid,
    })
}

#[derive(Clone, Default)]
struct CellData {
    text: String,
    fg: isize,
    bg: isize,
    attrs: u16,
}

#[derive(Clone)]
struct Frame {
    cells: Vec<CellData>,
    cursor: Option<(u16, u16)>,
    cursor_visible: bool,
    default_fg: isize,
    default_bg: isize,
    cols: u16,
    rows: u16,
}

fn capture_frame<'alloc, 'cb>(
    state: &mut RenderState<'alloc>,
    term: &Terminal<'alloc, 'cb>,
) -> Option<Frame> {
    let mut frame = Frame {
        cells: Vec::new(),
        cursor: None,
        cursor_visible: false,
        default_fg: -1,
        default_bg: -1,
        cols: 0,
        rows: 0,
    };

    let snapshot = state.update(term).ok()?;

    if let Ok(colors) = snapshot.colors() {
        frame.default_fg = pack_color(colors.foreground);
        frame.default_bg = pack_color(colors.background);
    }

    if let Ok(cur) = snapshot.cursor() {
        frame.cursor_visible = cur.visible;
        if let Some(viewport) = cur.viewport {
            frame.cursor = Some((viewport.x, viewport.y));
        }
    }

    frame.cols = snapshot.cols().ok().filter(|cols| *cols > 0).unwrap_or(0);
    frame.rows = snapshot.rows().unwrap_or(0);
    let cols = frame.cols;

    let mut row_iterator = RowIterator::new().ok()?;
    let mut cell_iterator = CellIterator::new().ok()?;
    let mut rows_iteration = row_iterator.update(&snapshot).ok()?;

    while let Some(row) = rows_iteration.next() {
        let mut in_row = 0u16;
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

                frame.cells.push(CellData {
                    text,
                    fg,
                    bg,
                    attrs,
                });
                in_row += 1;
            }
        }

        while in_row < cols {
            frame.cells.push(CellData::default());
            in_row += 1;
        }
    }

    Some(frame)
}

struct VTerm {
    term: Terminal<'static, 'static>,
    state: Rc<RefCell<RenderState<'static>>>,
    frame: Rc<RefCell<Frame>>,
    held: Rc<Cell<bool>>,
    hold_started: Rc<Cell<Instant>>,
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

        let state = Rc::new(RefCell::new(
            RenderState::new().context("unable to create render state")?,
        ));
        let frame = Rc::new(RefCell::new(Frame {
            cells: Vec::new(),
            cursor: None,
            cursor_visible: false,
            default_fg: -1,
            default_bg: -1,
            cols,
            rows,
        }));
        let held = Rc::new(Cell::new(false));
        let hold_started = Rc::new(Cell::new(Instant::now()));

        let hold_state = Rc::clone(&state);
        let hold_frame = Rc::clone(&frame);
        let hold_flag = Rc::clone(&held);
        let hold_started_at = Rc::clone(&hold_started);
        term.on_render_hold(move |term, holding| {
            if holding {
                if let (Ok(mut state), Ok(mut frame)) =
                    (hold_state.try_borrow_mut(), hold_frame.try_borrow_mut())
                {
                    if let Some(captured) = capture_frame(&mut state, term) {
                        *frame = captured;
                    }
                }
                hold_started_at.set(Instant::now());
            }
            hold_flag.set(holding);
        })
        .context("unable to register render hold callback")?;

        Ok(Self {
            term,
            state,
            frame,
            held,
            hold_started,
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

    fn apply_frame(&mut self, frame: Frame) {
        self.cells = frame.cells;
        self.cursor = frame.cursor;
        self.cursor_visible = frame.cursor_visible;
        self.default_fg = frame.default_fg;
        self.default_bg = frame.default_bg;
        if frame.cols > 0 {
            self.cols = frame.cols;
        }
        if frame.rows > 0 {
            self.rows = frame.rows;
        }
        self.iter = None;
    }

    fn refresh(&mut self) {
        if self.held.get() && self.hold_started.get().elapsed() >= RENDER_HOLD_TIMEOUT {
            let _ = self.term.set_mode(Mode::SYNC_OUTPUT, false);
            self.held.set(false);
        }

        if self.held.get() {
            let frame = self.frame.borrow().clone();
            self.apply_frame(frame);
            return;
        }

        let Some(frame) = ({
            let mut state = self.state.borrow_mut();
            capture_frame(&mut state, &self.term)
        }) else {
            return;
        };

        if let Ok(mut shared) = self.frame.try_borrow_mut() {
            *shared = frame.clone();
        }
        self.apply_frame(frame);
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

    fn mouse_event(&self, button: isize, x: isize, y: isize) -> String {
        let sgr = self.term.mode(Mode::SGR_MOUSE).unwrap_or(false)
            || self.term.mode(Mode::SGR_PIXELS_MOUSE).unwrap_or(false);
        let urxvt = self.term.mode(Mode::URXVT_MOUSE).unwrap_or(false);
        let tracked = self.term.mode(Mode::NORMAL_MOUSE).unwrap_or(false)
            || self.term.mode(Mode::BUTTON_MOUSE).unwrap_or(false)
            || self.term.mode(Mode::ANY_MOUSE).unwrap_or(false)
            || self.term.mode(Mode::X10_MOUSE).unwrap_or(false)
            || self.term.mode(Mode::UTF8_MOUSE).unwrap_or(false);
        if !sgr && !urxvt && !tracked {
            return String::new();
        }

        let col = (x.max(0) + 1) as u32;
        let row = (y.max(0) + 1) as u32;
        if sgr {
            return format!("\x1b[<{};{};{}M", button, col, row);
        }
        if urxvt {
            return format!("\x1b[<{};{};{}M", button + 32, col, row);
        }

        let encode = |value: u32| -> char { char::from_u32(value.clamp(32, 255)).unwrap_or(' ') };
        format!(
            "\x1b[M{}{}{}",
            encode((button + 32) as u32),
            encode(col + 32),
            encode(row + 32)
        )
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
        .register_fn("pty-foreground-shell?", Pty::foreground_is_shell)
        .register_fn("pty-foreground-title", Pty::foreground_title)
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
        .register_fn("vte/mouse-event", VTerm::mouse_event)
        .register_fn("vte/scroll-offset", VTerm::scroll_offset)
        .register_fn("vte/scroll-up", VTerm::scroll_up)
        .register_fn("vte/scroll-down", VTerm::scroll_down)
        .register_fn("vte/scroll-top", VTerm::scroll_top)
        .register_fn("vte/scroll-bottom", VTerm::scroll_bottom);

    module
}
