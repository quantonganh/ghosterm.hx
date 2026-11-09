use std::io::Write;
use std::sync::{Arc, atomic::AtomicBool, atomic::Ordering};

use abi_stable::std_types::{RBoxError, RResult};
use anyhow::{Context, Result};
use futures::lock::Mutex as AsyncMutex;
use portable_pty::{Child, CommandBuilder, MasterPty, NativePtySystem, PtySize, PtySystem};
use steel::declare_module;
use steel::rvals::Custom;
use steel::steel_vm::ffi::{FFIModule, FFIValue, FfiFuture, FfiFutureExt, IntoFFIVal, RegisterFFIFn};
use tokio::sync::mpsc;

fn to_rbox(err: impl std::fmt::Display) -> RBoxError {
    RBoxError::new(std::io::Error::other(err.to_string()))
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

            match tokio::time::timeout(std::time::Duration::from_millis(4), guard.recv()).await {
                Ok(Some(chunk)) => RResult::ROk(FFIValue::ByteVector(chunk.into())),
                Ok(None) => RResult::ROk(FFIValue::BoolV(false)),
                Err(_) => RResult::ROk(FFIValue::BoolV(false)),
            }
        }
        .into_ffi()
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

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("ghosterm");

    module
        .register_fn("default-shell", default_shell)
        .register_fn("create-pty", create_pty)
        .register_fn("pty-write", Pty::send_command)
        .register_fn("pty-resize", Pty::resize)
        .register_fn("pty-kill", Pty::kill)
        .register_fn("pty-alive?", Pty::alive)
        .register_fn("pty-read", Pty::async_read);

    module
}
