# Ghosterm

A integrated terminal emulator for Steel-based [Helix editor](https://helix-editor.com/), powered by [libghostty-vt](https://lib.ghostty.org/vt/)

# Why

One long-standing feature request in Helix is an integrated terminal ([#1976](https://github.com/helix-editor/helix/issues/1976))

Several comments in this discussion describe why an integrated terminal can make an editor a more productive development environment.

## A portable development environment

> I want a setup that I can take to any OS, including config files, without having to worry about other tooling (as much as possible, since obviously LSP servers etc need to be installed separately). Recently I have had to do some dev work on Windows, and it has been impossible to bring the tmux + nvim setup I usually use on Linux to this environment. I have ended up setting neovim up with a toggle-able integrated terminal to fit this use case.

[Source](https://github.com/helix-editor/helix/issues/1976#issuecomment-1141340715)

## Seamless navigation from terminal output

> For example, I can run a linter, get some warnings with file paths and line numbers. Then focus on the path, press gf and I get a buffer opened for that file (and even exactly at that line). This is not something a multiplexer can do. And this is just one example.

[Source](https://github.com/helix-editor/helix/issues/1976#issuecomment-1316223463)

> Having the terminal directly in the editor would allow e.g. jumping to file specified in the terminal output (see my cursor at the screenshot). Pressing gf on cargo output and going to the right file automatically sounds pretty nice.

## Interactive REPLs

> Chiming in to mention that I love an integrated terminal in order to easily run a REPL.
In Neovim, I like to open a terminal split and run a REPL there (say Python, Julia, etc.) Then I visual select some code, and use the NeoTerm plugin to paste the selection straight into the terminal/REPL. This is a very (for me) productive workflow, that seems to not be possible in helix.

[Source](https://github.com/helix-editor/helix/issues/1976#issuecomment-1341706192)

## Run configurations

> I really like jetbrains "Run Configurations", where I just need to press Ctrl-F10 and my projects gets run/tested and with Ctrl-F9 I can even bring up the debugger automatically.

[Source](https://github.com/helix-editor/helix/issues/1976#issuecomment-2266816022)

## Floating terminals

> My personal favorite use case is a floating terminal that I can dismiss after I run a command and the pick up where I left off next time I need to run something.

[Source](https://github.com/helix-editor/helix/issues/1976#issuecomment-2698874729)

---

Most of these workflows can already be archived by using [WezTerm](https://wezterm.org/index.html), as described in my [previous article](https://quantonganh.com/posts/2023/08/19/turn-helix-into-ide/).

However, this introduces a dependency on a particular terminal emulator. What if you want to switch to another one?

With the release of [libghostty 1.0](https://mitchellh.com/writing/libghostty-1-0), I want to explore another approach: bringing terminal emulation directly into Helix.

That's how Ghosterm started.

# Requirements

Ghosterm requires:

- [libghostty-vt](https://lib.ghostty.org/vt/) for terminal emulation
- A [Steel-enabled Helix build](https://github.com/mattwparas/helix/blob/steel-event-system/STEEL.md)

## Build libghostty-vt from source

```sh
curl -LO https://release.files.ghostty.org/libghostty-vt/1.0.0/libghostty-vt-1.0.0.tar.gz
tar -xzf libghostty-vt-1.0.0.tar.gz
cd libghostty-vt-1.0.0
zig build -Doptimize=ReleaseFast --prefix "$HOME/.local"
```

Configure `pkg-config` to find the installed library:

```sh
export PKG_CONFIG_PATH="${HOME}/.local/lib/pkgconfig"
```

## Build Helix with Steel support

```
git clone -b steel-event-system https://github.com/mattwparas/helix.git
cd helix
cargo xtask steel  
```

# Installation

Install Ghosterm using `forge`:

```sh
forge pkg install --git https://github.com/quantonganh/ghosterm.hx.git
```

Load the plugin by adding the following line to `~/.config/helix/init.scm`:

```
(require "ghosterm/ghosterm.scm")  
```

# Usage

Ghosterm currently provides the following command:

- `:terminal`: open a new terminal
- `:terminal-toggle`: toggle the terminal
- `Ctrl-\`: toggle focus between the editor and the terminal
- `:terminal-run`: run the current code base on the file extension
- `:terminal-exec`: execute a command

## Run configuration

Define project run configurations using file extensions:

```scheme
(define run-config
  '(
    ("go" "go run *.go")
    ("rs" "cargo run")
    ))
```

For example, when the current file has a `.go` extension, `:terminal-run` executes `go run *.go`.

## Navigation from build output

After running a build or linter, you can click a file path to open the file at the relevant line.
