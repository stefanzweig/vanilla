# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is "Vanilla Emacs" - a minimal, personally-configured Emacs setup. It uses a modular architecture where configuration is split into separate files under `personal/`.

Target: Emacs 30.2 (native-comp). Entry point: `init.el`. Verified to load cleanly with
`emacs -Q --batch -l ~/.emacs.d/init.el`.

## Architecture

### Directory Structure
- `init.el` - Main entry point that loads personal configurations and ELPA packages
- `personal/` - Personal configuration modules (loaded via `(personal 'module-name)`); also
  holds bundled third-party libraries (dash, s, f, projectile, which-key, iedit, crux,
  org-brain, key-chord)
- `elpa/` - ELPA package directory (git-ignored, re-fetchable)
- `backups/`, `auto-save-list/`, `tree-sitter/` - Runtime state (git-ignored)
- `vendor/` - Referenced by `(vendor 'library)` but **does not exist**; third-party libraries
  currently live in `personal/` instead

### Loading Mechanism

The `defuns.el` file defines helper functions:
- `(personal 'library)` - Loads `personal/library.el`
- `(package 'pkg)` - Installs and loads an ELPA package
- `(vendor 'library)` - Loads from vendor directory (defined but unused)

### Configuration Modules

Core modules loaded in `init.el` (in order):
- `crux` - Collection of useful commands (kill-line, duplicate-line, etc.)
- `org-bullets` - UTF-8 bullets in Org mode
- `infrastructure` - Loads the bundled third-party libraries (dash, s, f)
- `which-key` - Display available keybindings in popup
- `iedit` - Edit multiple regions simultaneously
- `projectile` - Project navigation
- `zweig` / `vanilla-utils` - Personal utility functions
- `key-chord` - Two-key chord bindings (jj, jl, jk, kk, etc.)
- `winpulse` - Window focus flash animation
- `global` - Global settings (theme, recentf, modes, hooks)
- `fonts` - GUI font configuration (see Fonts below)
- `org` - Org mode configuration
- `vertico` / `consult` / `embark` / `corfu` - Completion stack
- `doom-modeline` - Mode line (replaced powerline)
- `treesit` / `eglot` - Tree-sitter grammars and LSP
- `bindings` - Global keybindings

The `xah-*` commands bound in `bindings.el` are defined in `vanilla-utils.el` and `zweig.el`.

### ELPA Packages

Managed in `init.el`:
- magit, avy, expand-region, smartparens, paredit, yasnippet, hydra
- vertico, consult, embark, embark-consult, orderless, corfu, cape
- doom-modeline, treesit-auto, exec-path-from-shell, highlight-parentheses, diminish

`(package 'pkg)` only *installs*; the load happens through the autoloads written by
`package-initialize`, so a package needs a `(package 'pkg)` line even when no `personal/pkg.el`
exists. A missing line means the bindings in `bindings.el` point at void functions - this is how
the `jj`/`jl`/`jk` chords were broken until avy was added.

`multiple-cursors` has no `(package ...)` line and must not be pruned: `personal/iedit.el`
requires `multiple-cursors-core`, and the `mc/*` bindings resolve through its autoloads.
`llama` / `cond-let` / `with-editor` / `transient` / `magit-section` are magit dependencies;
`compat` / `nerd-icons` / `shrink-path` / `lv` belong to the completion and mode-line packages.

## Fonts

`personal/fonts.el` is fussy about advance widths on purpose. Emacs gives a CJK character two
Latin columns, so org tables only line up when a han glyph is drawn exactly twice as wide as a
space. **Only 0.5 em families are allowed in `my/gui-fonts`** - a 0.6 em font (Fira Code,
JetBrains Mono) allocates 1.2 em per han but the CJK glyph still draws 1.0 em, and every `|` in
an org table drifts right.

- Candidates: `Sarasa Mono SC`, then `Iosevka` (both 0.5 em), height 200, GUI frames only
- `han` / `cjk-misc` / `bopomofo` / `kana` are pinned to the same family, so CJK never silently
  falls back to PingFang
- `#xe000-#xf8ff` falls back to `Symbols Nerd Font` for doom-modeline/powerline glyphs
- Installed weights in `~/.local/share/fonts/`: Regular, Bold, Italic, BoldItalic (only the
  essential 4 weights kept to save disk space; `light` / `semi-bold` resolve to Regular)

The file itself is always correct - `org-table-align` counts two columns per han character - so
realigning in Emacs cannot fix a broken table. Check the rendering:

```elisp
(string-pixel-width "中")        ; must equal 2 x (string-pixel-width " ")
```

## Key Bindings

### Window Management (M-1/2/3/0 remapped)
- `M-1` - Delete other windows
- `M-2` - Split window vertically
- `M-3` - Split window horizontally
- `M-0` - Delete window
- `M-o` - Other window
- `C-x 0/1/2/3/o` - Deliberately unbound (to help unlearn them)

### Org Mode
- `C-c a` / `F8 a` - Org agenda
- `F8 c` - Org capture
- `F8 l` - Org todo list (current file)
- `F8 m` - Yank more (org link)
- `F8 n` - Default orgmode playground

### Git
- `C-x g` - Magit status

### Editing
- `C-=` / `M-=` - Expand region
- `C-s` - Consult line (search)
- `C-k` - Crux smart kill line
- `C-,` - Duplicate line/region
- `C-t` - Hydra utils (movement, zoom, buffers)
- `M-[`, `M-{`, `M-"` - Insert pair
- `M-)` - Delete pair
- `F5` - Hydra zoom menu
- `F8 (` / `[` / `{` - Insert brackets
- `F8 f` - Copy file name to clipboard
- `F8 g` - Consult git grep
- `F8 o` - Open file at cursor
- `F8 s` - Select block

### Key Chords (jj, jl, jk, kk, etc.)
- `jj` - Avy goto word
- `jl` - Avy goto line
- `jk` - Avy goto char
- `kk` - Execute extended command
- `yy` - Copy line/region
- `dd` - Cut line/region
- `hh` - Consult line
- `gg` - Consult git grep

Chords only fire while `key-chord-mode` is on; it is enabled in `global.el`, and the chord
lookups live in `global-map` under the `key-chord` pseudo-event.

### Multiple Cursors
- `C-S-c C-S-c` - Edit lines
- `C->` / `C-<` - Mark next/previous like this
- `C-c C-<` - Mark all like this

`multiple-cursors` has no `(package ...)` line and still works: the bindings resolve through
its autoloads, which load the package on first use.

### Shell
- `C-!` - Shell

## Org Mode Configuration

- Agenda files: `~/Documents/orgfiles` (**this directory does not exist yet**, so agenda and
  capture are effectively empty)
- TODO keywords: `TODO`, `NEXT` | `DONE`; `WAIT`, `APPT` | `CNCL`, `DFRD`
- Clock tracking enabled with persistence
- Babel languages: Python (`python3`), Emacs Lisp, Shell, Java, C, Lua, Calc, Dot, Ditaa, PlantUML
- Custom agenda commands: "D" (Daily, `org-agenda-span` = `day`), "P" (By priority)

## Coding Style

- Elisp files use `;;; filename.el --- Description` headers
- Provide statements at end: `(provide 'module-name)`
- Indentation: 2 spaces for Elisp
- Line numbers: relative mode enabled
- Trailing whitespace auto-cleanup on save

## Custom Functions (zweig.el / defuns.el)

- `zweig/default_orgmode_playground` - New scratch org buffer
- `xah-open-file-at-cursor` - Open file path at cursor
- `xah-insert-paren/square-bracket/brace` - Insert bracket pairs
- `move-text-up/down` - Move lines with S-C-up/down
- `convert-selected-hex-to-decimal` - Hex to decimal conversion
- `split-region-by-two/four-chars-safe` - Split text by character count

## Themes

- Active theme: `misterioso`
- Font: Sarasa Mono SC, height 200 (GUI only; falls back to Iosevka)
- Mode line: doom-modeline with icons, height 25
- Relative line numbers enabled
