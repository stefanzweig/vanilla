# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is "Vanilla Emacs" - a minimal, personally-configured Emacs setup. It uses a modular architecture where configuration is split into separate files under `personal/`.

## Architecture

### Directory Structure
- `init.el` - Main entry point that loads personal configurations and ELPA packages
- `personal/` - Personal configuration modules (loaded via `(personal 'module-name)`)
- `vendor/` - External libraries (submodule-managed, currently empty)
- `elpa/` - ELPA package directory
- `backups/` - Backup files

### Loading Mechanism

The `defuns.el` file defines helper functions:
- `(personal 'library)` - Loads `personal/library.el`
- `(package 'pkg)` - Installs and loads an ELPA package
- `(vendor 'library)` - Loads from vendor directory

### Configuration Modules

Core modules loaded in `init.el`:
- `crux` - Collection of useful commands (kill-line, duplicate-line, etc.)
- `org` / `org-bullets` - Org mode with UTF-8 bullets
- `which-key` - Display available keybindings in popup
- `iedit` - Edit multiple regions simultaneously
- `projectile` - Project navigation
- `key-chord` - Two-key chord bindings (jj, jl, jk, kk, etc.)
- `powerline` - Custom mode-line theme
- `winpulse` - Window focus flash animation
- `zweig` - Personal utility functions
- `global` - Global settings (themes, modes, hooks)
- `bindings` - Global keybindings
- `fonts` - Font configuration
- `vanilla-company` - Company mode settings
- `vanilla-utils` - Utility functions

### ELPA Packages

Managed in `init.el`:
- magit, expand-region, company, ivy, swiper, counsel
- smartparens, paredit, yasnippet
- exec-path-from-shell, highlight-parentheses, diminish
- ivy-posframe, hydra

## Key Bindings

### Window Management (M-1/2/3/0 remapped)
- `M-1` - Delete other windows
- `M-2` - Split window vertically
- `M-3` - Split window horizontally
- `M-0` - Delete window
- `M-o` - Other window

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
- `C-s` - Swiper (search)
- `C-,` - Duplicate line/region
- `M-[`, `M-{`, `M-"` - Insert pair
- `M-)` - Delete pair
- `F5` - Hydra zoom menu
- `F8 (` / `[` / `{` - Insert brackets
- `F8 f` - Copy file name to clipboard
- `F8 g` - Git grep
- `F8 o` - Open file at cursor
- `F8 s` - Select block

### Key Chords (jj, jl, jk, kk, etc.)
- `jj` - Avy goto word
- `jl` - Avy goto line
- `jk` - Avy goto char
- `kk` - Execute extended command
- `yy` - Copy line/region
- `dd` - Cut line/region
- `hh` - Swiper thing at point
- `gg` - Counsel git grep

### Multiple Cursors
- `C-S-c C-S-c` - Edit lines
- `C->` / `C-<` - Mark next/previous like this
- `C-c C-<` - Mark all like this

### Shell
- `C-!` - Shell

## Org Mode Configuration

- Agenda files: `~/Documents/orgfiles`
- TODO keywords: `TODO`, `NEXT` | `DONE`; `WAIT`, `APPT` | `CNCL`, `DFRD`
- Clock tracking enabled with persistence
- Babel languages: Python, Emacs Lisp, Shell, Java, C, Lua, Calc, Dot, Ditaa, PlantUML
- Custom agenda commands: "D" (Daily), "P" (By priority)

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
- Font: Iosevka (height 220)
- Powerline: default theme with center layout
- Relative line numbers enabled
