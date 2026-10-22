;;; treesit.el --- Tree-sitter configuration -*- lexical-binding: t; coding: utf-8; -*-

;; Tree-sitter for better syntax highlighting and navigation
;; Requires Emacs 29+

(require 'cl-lib)

(declare-function vanilla-ts--switch-parser-silent "treesit" nil t)

;; ──────────────────────────────────────────────────────────────────────
;; Offline grammar support
;; ──────────────────────────────────────────────────────────────────────
;; If you cannot `git clone' from github (e.g. behind a restrictive
;; proxy), pre-clone or download the grammar repos on a machine that
;; can, and drop each `tree-sitter-<lang>' directory into:
;;
;;     ~/ts-grammars/        (i.e. C:\Users\<you>\ts-grammars)
;;
;; Layout expected by this config:
;;
;;     ~/ts-grammars/tree-sitter-c/src/parser.c
;;     ~/ts-grammars/tree-sitter-python/src/parser.c
;;     ~/ts-grammars/tree-sitter-bash/src/parser.c
;;     ...
;;
;; For each language whose local directory exists, we override the
;; recipe URL with the local path.  Emacs' built-in
;; `treesit-install-language-grammar' detects that the URL is a local
;; directory and SKIPS `git clone', going straight to compile with the
;; C compiler on PATH (gcc found: MinGW 7.3.0).  The compiled
;; `libtree-sitter-<lang>.dll' lands in `~/.emacs.d/tree-sitter/'.
;;
;; Languages without a local directory fall back to the normal
;; (github clone) behavior — harmless if the network is unavailable.
;;
;; To compile a single grammar interactively:  M-x treesit-install-language-grammar RET c RET
;; To compile every available local grammar:  M-x treesit-auto-install-all  (or just open a .c / .py file)

(defvar vanilla-ts-grammars-dir
  (let ((candidates nil))
    ;; Collect candidate paths in priority order.  We check EXISTENCE
    ;; rather than trusting any single env var, because a native
    ;; Windows Emacs injects HOME=.../AppData/Roaming even when the
    ;; real profile is C:\Users\<you>, and that injected HOME points
    ;; where the grammars do NOT live.
    (dolist (home (list (getenv "HOME")
                        (getenv "USERPROFILE")))
      (when home
        (push (expand-file-name "ts-grammars" home) candidates)))
    ;; Final fallback: walk up from the directory of this file (or
    ;; user-emacs-directory) to find a `ts-grammars' dir.  In a real
    ;; Emacs session `personal/treesit.el' lives at
    ;;   C:\Users\<you>\AppData\Roaming\.emacs.d\personal\treesit.el
    ;; so its great-grandparent is C:\Users\<you>\, where the
    ;; grammars live.  Batch tests use a different load-file-name so
    ;; this fallback only fires when env vars are also missing.
    (let* ((file (or load-file-name
                     (buffer-file-name)
                     (expand-file-name user-emacs-directory)))
           (dir (file-name-directory file))
           (user-home nil))
      (cl-block nil
        (dolist (_ '(1 2 3 4 5))
          (let ((parent (file-name-directory
                         (directory-file-name dir))))
            (when (file-accessible-directory-p
                   (expand-file-name "ts-grammars" parent))
              (setq user-home parent)
              (cl-return))
            (setq dir parent))))
      (when user-home
        (push (expand-file-name "ts-grammars" user-home) candidates)))
    (setq candidates (nreverse candidates))
    ;; Return the first candidate that exists; else the first one
    ;; (so diagnose shows a sensible string even if nothing matches).
    (or (cl-find-if #'file-accessible-directory-p candidates)
        (car candidates)))
  "Local directory holding pre-cloned `tree-sitter-<lang>' grammar repos.
If a language's directory exists here, its recipe URL is rewritten
to this local path so `treesit-install-language-grammar' skips
`git clone' and compiles from the local source.

Resolved by checking which candidate path actually EXISTS:
$HOME/ts-grammars, then $USERPROFILE/ts-grammars, then the parent
of `user-emacs-directory'/ts-grammars.  We cannot trust `$HOME'
alone: a native-Windows Emacs injects HOME=.../AppData/Roaming
even when the real profile is C:\\Users\\<you>, and that injected
value points where the grammars do NOT live.")

(defun vanilla-ts--local-source-for-lang (lang &optional url)
  "Return local source dir for LANG if present, else nil.

Tries `tree-sitter-<lang>' under `vanilla-ts-grammars-dir' first.
If that doesn't exist but URL points at github, try the repo-name
from URL too — this covers languages whose repo-name != lang,
e.g. `tsx' and `typescript' both live in `tree-sitter-typescript'."
  (let ((dir (expand-file-name
              (concat "tree-sitter-" (symbol-name lang))
              vanilla-ts-grammars-dir)))
    (cond
     ((file-accessible-directory-p dir) dir)
     ;; Fall back: derive dir name from the github repo in URL.
     ((and url (string-match "github.com/[^/]+/\\([^/]+\\)\\(?:\\.git\\)?/?\\'" url))
      (let ((repo-dir (expand-file-name
                       (match-string 1 url)
                       vanilla-ts-grammars-dir)))
        (and (file-accessible-directory-p repo-dir) repo-dir)))
     (t nil))))

(defun vanilla-ts--collect-src-dirs ()
  "Return list of `src/' dirs that actually hold a compiled grammar.
Scans `vanilla-ts-grammars-dir' directly — does NOT depend on
`treesit-language-source-alist', which `treesit-auto' only fills
via a temporary `let' binding at runtime (so the global value is
empty at load time).  This is the robust way to populate
`treesit-extra-load-path'."
  (let ((dirs nil)
        (suffix (if (eq system-type 'windows-nt) "\\.dll\\'" "\\.so\\'")))
    (when (file-accessible-directory-p vanilla-ts-grammars-dir)
      (dolist (dll (directory-files-recursively
                    vanilla-ts-grammars-dir
                    (concat "\\`libtree-sitter-.*" suffix)))
        ;; Parent dir of each dll is the `src/' treesit should load from.
        (let ((src (file-name-directory dll)))
          (cl-pushnew src dirs :test #'string=))))
    dirs))

(with-eval-after-load 'treesit
  ;; Tell the *built-in* `treesit' where to LOAD already-compiled
  ;; grammars from.  Each `libtree-sitter-<lang>.dll' lives in its
  ;; grammar repo's `src/' dir under `~/ts-grammars/'.  Without this,
  ;; `treesit' only looks in `~/.emacs.d/tree-sitter/' (empty here)
  ;; and reports every language as "not installed" even though the
  ;; dlls exist — which is exactly why opening a .c file kept
  ;; prompting "Tree-sitter grammar for c is missing.  Install it?".
  ;; We scan the directory (not the alist) because `treesit-auto'
  ;; populates `treesit-language-source-alist' only via a temporary
  ;; `let' binding inside its own functions, so the global value is
  ;; empty at load time.
  (let ((src-dirs (vanilla-ts--collect-src-dirs)))
    (when src-dirs
      (dolist (d src-dirs)
        (cl-pushnew d (default-value 'treesit-extra-load-path)
                    :test #'string=)))))

;; Rewrite recipes at install time.  `treesit-auto' rebuilds
;; `treesit-language-source-alist' from its recipe list on every
;; call (via `treesit-auto--build-treesit-source-alist'), so setting
;; the global alist once has no effect.  Instead, advise
;; `treesit-install-language-grammar': right before it runs, rewrite
;; the recipe for the requested LANG so the URL points at the local
;; repo (skipping `git clone') and REVISION is nil (skipping
;; `git checkout <tag>', which fails if your local clone is master
;; but the recipe pins v0.23.6).
(defun vanilla-ts--rewrite-recipe-around (orig lang &rest args)
  "Advice: redirect LANG's recipe URL to a local grammar repo if present."
  (when-let* ((entry (assq lang treesit-language-source-alist))
              (url (cadr entry))
              (local (vanilla-ts--local-source-for-lang lang url)))
    (setcar (cdr entry) local)            ; url  -> local path
    (setcar (cddr entry) nil))            ; rev  -> nil (skip checkout)
  (apply orig lang args))

(with-eval-after-load 'treesit
  (advice-add #'treesit-install-language-grammar :around
              #'vanilla-ts--rewrite-recipe-around))

(defun vanilla-ts-diagnose ()
  "Print where treesit looks for grammars and which are loadable.
Scans `vanilla-ts-grammars-dir' directly (not the global
`treesit-language-source-alist', which `treesit-auto' leaves empty
at load time).  Run `M-x vanilla-ts-diagnose' to verify: every
`libtree-sitter-<lang>.dll' under `~/ts-grammars/...' should map
to `loadable? YES' once `treesit-extra-load-path' is set."
  (interactive)
  (let ((buf (get-buffer-create "*treesit-diagnose*"))
        (suffix (if (eq system-type 'windows-nt) ".dll" ".so")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "treesit-extra-load-path:\n  %S\n\n"
                      (default-value 'treesit-extra-load-path)))
      (insert (format "vanilla-ts-grammars-dir:\n  %S\n\n"
                      vanilla-ts-grammars-dir))
      (insert "dll                                             | dll-exists? | loadable?\n")
      (insert "------------------------------------------------|-------------|----------\n")
      ;; Walk every compiled dll under the grammars dir.
      (let ((dlls nil))
        (when (file-accessible-directory-p vanilla-ts-grammars-dir)
          (setq dlls
                (directory-files-recursively
                 vanilla-ts-grammars-dir
                 (concat "\\`libtree-sitter-.*" (regexp-quote suffix) "\\'"))))
        (dolist (dll (sort dlls #'string<))
          (let* ((base (file-name-nondirectory dll))
                 (lang-str (and (string-match
                                 "\\`libtree-sitter-\\(.+\\)\\."
                                 base)
                                (match-string 1 base)))
                 (lang (and lang-str (intern lang-str))))
            (insert (format "%-47s | %-11s | %s\n"
                            (abbreviate-file-name dll)
                            (if (file-exists-p dll) "yes" "no")
                            (if (and lang (treesit-language-available-p lang))
                                "YES" "no")))))
        (when (null dlls)
          (insert "(no libtree-sitter-*.dll found under vanilla-ts-grammars-dir)\n"))))
    (display-buffer buf)
    (message "see *treesit-diagnose* buffer")))

;; ──────────────────────────────────────────────────────────────────────
;; treesit-explore-mode: pick the first parser silently instead of
;; prompting with `Parser:' every time you toggle it on.  Emacs pops
;; `completing-read' whenever a buffer has >=2 parsers (HTML, Vue,
;; org-babel, etc.); the first entry is already `treesit-primary-
;; parser', which is what you'd pick anyway.
;; ──────────────────────────────────────────────────────────────────────
(with-eval-after-load 'treesit
  (defun vanilla-ts--switch-parser-silent (orig prompt collection &rest args)
    "Silently pick the first parser when `treesit-explore-mode' prompts.

`treesit-explore-mode' calls `call-interactively' on
`treesit-explorer-switch-parser', whose interactive form runs
`(completing-read \"Parser: \" ...)' whenever a buffer has >=2
parsers (HTML, Vue, org-babel, etc.).  Emacs evaluates the
interactive form BEFORE any :around advice on the command runs,
so advising the command itself can't suppress the prompt.  We
instead intercept `completing-read' and return the first
candidate when the prompt is `Parser: '.  The first entry is
already `treesit-primary-parser', which is what you'd pick anyway."
    (if (and (stringp prompt)
             (string-prefix-p "Parser: " prompt))
        ;; collection is (mapcar #'car parser-alist) -> list of names
        (if (listp collection)
            (car collection)
          ;; function table: ask it for the entries
          (car (funcall collection nil nil)))
      (apply orig prompt collection args)))
  (advice-add #'completing-read :around
              #'vanilla-ts--switch-parser-silent))

;; ──────────────────────────────────────────────────────────────────────

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode))
  :custom
  (treesit-font-lock-level 4)  ; maximum syntax highlighting
  :config
  ;; Do NOT call `treesit-auto-install-all' on startup: it scans all
  ;; 60+ recipes and prompts "grammars are/were missing" for every
  ;; language you have not installed.  Installed grammars load from
  ;; `~/.emacs.d/tree-sitter/'; missing ones are silently ignored
  ;; because `treesit-auto-install' is nil.  To force-compile all
  ;; local grammars, run M-x treesit-auto-install-all manually.
  )

(use-package treesit-auto
  :custom
  (treesit-auto-install nil)   ; don't auto-clone on every startup
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'treesit)
;;; treesit.el ends here
