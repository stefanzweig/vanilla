;;; iedit-lib.el --- APIs for editing multiple regions in the same way
;;; simultaneously.

;; Copyright (C) 2010, 2011, 2012 Victor Ren

;; Time-stamp: <2020-08-26 19:29:09 Victor Ren>
;; Author: Victor Ren <victorhge@gmail.com>
;; Keywords: occurrence region simultaneous rectangle refactoring
;; Version: 0.9.9.9
;; X-URL: https://github.com/victorhge/iedit
;;        https://www.emacswiki.org/emacs/Iedit
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is iedit APIs library that allow you to write your own minor mode.
;; The functionalities of the APIs:
;; - Create occurrence overlays
;; - Navigate in the occurrence overlays
;; - Modify the occurrences
;; - Hide/show
;; - Other basic support APIs
;; 
;; A few concepts that help you understand what this is about:
;;
;; Occurrence - one of the regions that are selected, highlighted, usually the
;; same and about to be modified.
;;
;; Occurrence overlay - overlay used to provide a different face for occurrence
;;
;; Occurrence line - the line that has at least one occurrence
;;
;; Context line - the line that doesn't have occurrences

;;; todo:
;; - Update comments for APIs
;; - Add more easy access keys for whole occurrence

;;; Code:

;; (eval-when-compile (require 'cl-lib))

(defgroup iedit nil
  "Edit multiple regions in the same way simultaneously.
The regions are usually the same, called 'occurrence' in the mode."
  :prefix "iedit-"
  :group 'replace
  :group 'convenience)

(defface iedit-occurrence
  '((t :inherit highlight))
  "*Face used for the occurrences' default values."
  :group 'iedit)

(defface iedit-read-only-occurrence
  '((t :inherit region))
  "*Face used for the read-only occurrences' default values."
  :group 'iedit)

(defcustom iedit-case-sensitive-default t
  "If no-nil, matching is case sensitive."
  :type 'boolean
  :group 'iedit)

(defcustom iedit-transient-mark-sensitive t
  "If no-nil, Iedit mode is sensitive to the Transient Mark mode.
It means Iedit works as expected only when regions are
highlighted.  If you want to use iedit without Transient Mark
mode, set it as nil."
  :type 'boolean
  :group 'iedit)

(defcustom iedit-auto-buffering nil
  "If no-nil, iedit-mode automatically starts buffering the changes.
 This could be a workaround for lag problem under certain modes."
  :type 'boolean
  :group 'iedit)

(defcustom iedit-overlay-priority 200
  "The priority of the overlay used to indicate matches."
  :type 'integer
  :group 'iedit)

(defcustom iedit-index-update-limit 200
  "If the number of occurrences is great than this, the
`iedit-occurrence-index' will not be updated.  This is to avoid
the traverse of the long `iedit-occurrences-overlays' list."
  :type 'integer
  :group 'iedit)

(defcustom iedit-increment-format-string "%03d"
  "Format string used to format incremented numbers.
This is used by `iedit-number-occurrences'."
  :type 'string
  :group 'iedit)

(defvar iedit-occurrences-overlays nil
  "The occurrences slot contains a list of overlays used to
indicate the position of each editable occurrence.  In addition, the
occurrence overlay is used to provide a different face
configurable via `iedit-occurrence'.")

(defvar iedit-read-only-occurrences-overlays nil
  "The occurrences slot contains a list of overlays used to
indicate the position of each read-only occurrence.  In addition, the
occurrence overlay is used to provide a different face
configurable via `iedit-ready-only-occurrence'.")

(defvar iedit-case-sensitive iedit-case-sensitive-default
  "This is buffer local variable.
If no-nil, matching is case sensitive.")

(defvar iedit-hiding nil
  "This is buffer local variable which indicates whether buffer lines are hided. ")

(defvar iedit-forward-success t
  "This is buffer local variable which indicates the moving
forward or backward successful")

(defvar iedit-before-modification-string ""
  "This is buffer local variable which is the buffer substring
that is going to be changed.")

(defvar iedit-before-buffering-string ""
  "This is buffer local variable which is the buffer substring
that is going to be changed.")

(defvar iedit-before-buffering-undo-list nil
  "This is buffer local variable which is the buffer undo list before modification.")

(defvar iedit-before-buffering-point nil
  "This is buffer local variable which is the point before modification.")

;; `iedit-update-occurrences' gets called twice when change==0 and
;; occurrence is zero-width (beg==end) -- for front and back insertion.
(defvar iedit-skip-modification-once t
  "Variable used to skip first modification hook run when
insertion against a zero-width occurrence.")

(defvar iedit-aborting nil
  "This is buffer local variable which indicates Iedit mode is aborting.")

(defvar iedit-aborting-hook nil
  "Functions to call before iedit-abort.  Normally it should be mode exit function.")

(defvar iedit-post-undo-hook-installed nil
  "This is buffer local variable which indicated if
`iedit-post-undo' is installed in `post-command-hook'.")

(defvar iedit-buffering nil
  "This is buffer local variable which indicates iedit-mode is
buffering, which means the modification to the current occurrence
is not applied to other occurrences when it is true.")

(defvar iedit-occurrence-context-lines 1
  "The number of lines before or after the occurrence.")

(defvar iedit-occurrence-index 0
  "The index of the current occurrence, counted from the beginning of the buffer.
Used in mode-line to indicate the position of the current
occurrence.")

(defvar iedit-after-change-list nil
  "Used to store the modifications in the command being run.")

(defvar iedit-updating nil
  "Used to prevent recursive calling change hooks.
It replaces `inhibit-modification-hooks' which prevents calling
`after-change-functions'.")

(make-variable-buffer-local 'iedit-updating)
(make-variable-buffer-local 'iedit-after-change-list)
(make-variable-buffer-local 'iedit-occurrences-overlays)
(make-variable-buffer-local 'iedit-read-only-occurrences-overlays)
(make-variable-buffer-local 'iedit-hiding)
(make-local-variable 'iedit-case-sensitive)
(make-variable-buffer-local 'iedit-forward-success)
(make-variable-buffer-local 'iedit-before-modification-string)
(make-variable-buffer-local 'iedit-before-buffering-string)
(make-variable-buffer-local 'iedit-before-buffering-undo-list)
(make-variable-buffer-local 'iedit-before-buffering-point)
(make-variable-buffer-local 'iedit-skip-modification-once)
(make-variable-buffer-local 'iedit-aborting)
(make-variable-buffer-local 'iedit-buffering)
(make-variable-buffer-local 'iedit-auto-buffering)
(make-variable-buffer-local 'iedit-post-undo-hook-installed)
(make-variable-buffer-local 'iedit-occurrence-context-lines)
(make-variable-buffer-local 'iedit-occurrence-index)

(defconst iedit-occurrence-overlay-name 'iedit-occurrence-overlay-name)
(defconst iedit-invisible-overlay-name 'iedit-invisible-overlay-name)

;;; Define Iedit mode map
(defvar iedit-lib-keymap
  (let ((map (make-sparse-keymap)))
    ;; Default key bindings
    (define-key map (kbd "TAB") 'iedit-next-occurrence)
    (define-key map (kbd "<tab>") 'iedit-next-occurrence)
    (define-key map (kbd "<S-tab>") 'iedit-prev-occurrence)
    (define-key map (kbd "<S-iso-lefttab>") 'iedit-prev-occurrence)
    (define-key map (kbd "<backtab>") 'iedit-prev-occurrence)
    (define-key map (kbd "C-'") 'iedit-show/hide-context-lines)
	(define-key map (kbd "C-\"") 'iedit-show/hide-occurrence-lines)
    map)
  "Keymap used while Iedit mode is enabled.")

(defvar iedit-occurrence-keymap-default
  (let ((map (make-sparse-keymap)))
    ;; `yas-minor-mode' uses tab by default and installs its keymap in
    ;; `emulation-mode-map-alists', which is used before before
    ;; ‘minor-mode-map-alist’.  So TAB is bond to get used even before
    ;; `yas-minor-mode', to prevent overriding.
    (define-key map (kbd "TAB") 'iedit-next-occurrence)
    (define-key map (kbd "<tab>") 'iedit-next-occurrence)
    (define-key map (kbd "M-U") 'iedit-upcase-occurrences)
    (define-key map (kbd "M-L") 'iedit-downcase-occurrences)
    (define-key map (kbd "M-R") 'iedit-replace-occurrences)
    (define-key map (kbd "M-SPC") 'iedit-blank-occurrences)
    (define-key map (kbd "M-D") 'iedit-delete-occurrences)
    (define-key map (kbd "M-N") 'iedit-number-occurrences)
    (define-key map (kbd "M-B") 'iedit-toggle-buffering)
    (define-key map (kbd "M-<") 'iedit-goto-first-occurrence)
    (define-key map (kbd "M->") 'iedit-goto-last-occurrence)
    (define-key map (kbd "C-?") 'iedit-help-for-occurrences)
    (define-key map [remap keyboard-escape-quit] 'iedit-quit)
    (define-key map [remap keyboard-quit] 'iedit-quit)
    map)
  "Default keymap used within occurrence overlays.")

;; The declarations are to avoid compile errors if mc is unknown by Emacs.
(declare-function mc/create-fake-cursor-at-point "ext:mutiple-cursors-core.el" nil)
(declare-function multiple-cursors-mode "ext:mutiple-cursors-core.el")
(defvar mc/cmds-to-run-once)

(when (require 'multiple-cursors-core nil t)
  (defun iedit-switch-to-mc-mode ()
    "Switch to `multiple-cursors-mode'.  So that you can navigate
out of the occurrence and edit simultaneously with multiple
cursors."
    (interactive "*")
    (iedit-barf-if-buffering)
    (let* ((ov (iedit-find-current-occurrence-overlay))
	   (offset (- (point) (overlay-start ov)))
	   (master (point)))
      (save-excursion
        (dolist (occurrence iedit-occurrences-overlays)
	  (goto-char (+ (overlay-start occurrence) offset))
	  (unless (= master (point))
	    (mc/create-fake-cursor-at-point))))
      (run-hooks 'iedit-aborting-hook)
      (multiple-cursors-mode 1)))
  ;; `multiple-cursors-mode' runs `post-command-hook' function for all the
  ;; cursors. `post-command-hook' is setup in `iedit-switch-to-mc-mode' So the
  ;; function is executed after `iedit-switch-to-mc-mode'. It is not expected.
  ;; `mc/cmds-to-run-once' is for skipping this.
  (add-to-list 'mc/cmds-to-run-once 'iedit-switch-to-mc-mode)
  (define-key iedit-occurrence-keymap-default (kbd "M-M") 'iedit-switch-to-mc-mode))

(defvar iedit-occurrence-keymap 'iedit-occurrence-keymap-default
  "Keymap used within occurrence overlays.
It should be set before occurrence overlay is created.")
(make-local-variable 'iedit-occurrence-keymap)

(defun iedit-help-for-occurrences ()
  "Display `iedit-occurrence-keymap-default'"
  (interactive)
  (message (concat (substitute-command-keys "\\[iedit-upcase-occurrences]") "/"
                   (substitute-command-keys "\\[iedit-downcase-occurrences]") ":up/downcase "
                   (substitute-command-keys "\\[iedit-replace-occurrences]") ":replace "
                   (substitute-command-keys "\\[iedit-blank-occurrences]") ":blank "
                   (substitute-command-keys "\\[iedit-delete-occurrences]") ":delete "
                   (substitute-command-keys "\\[iedit-number-occurrences]") ":number "
                   (substitute-command-keys "\\[iedit-toggle-buffering]") ":buffering "
                   (substitute-command-keys "\\[iedit-goto-first-occurrence]") "/"
                   (substitute-command-keys "\\[iedit-goto-last-occurrence]") ":first/last "
                   )))

(defun iedit-quit ()
  "Quit the current mode."
  (interactive)
  (run-hooks 'iedit-aborting-hook))

(defun iedit-make-markers-overlays (markers)
  "Create occurrence overlays on a list of markers."
  (setq iedit-occurrences-overlays
       (mapcar #'(lambda (marker)
                   (iedit-make-occurrence-overlay (car marker) (cdr marker)))
               markers)))

(defun iedit-make-occurrences-overlays (occurrence-regexp beg end)
  "Create occurrence overlays for `occurrence-regexp' in a region.
Return the number of occurrences."
  (setq iedit-aborting nil)
  (setq iedit-occurrences-overlays nil)
  (setq iedit-read-only-occurrences-overlays nil)
  ;; Find and record each occurrence's markers and add the overlay to the occurrences
  (let ((counter 0)
        (case-fold-search (not iedit-case-sensitive))
	(length 0))
    (save-excursion
      (save-window-excursion
        (goto-char end)
        ;; todo: figure out why re-search-forward is slow without "recenter"
        (recenter)
        (goto-char beg)
        (while (re-search-forward occurrence-regexp end t)
          (let ((beginning (match-beginning 0))
                (ending (match-end 0)))
	    (if (and (> length 0) (/= (- ending beginning) length))
		(throw 'not-same-length 'not-same-length)
	      (setq length (- ending beginning)))
            (if (text-property-not-all beginning ending 'read-only nil)
                (push (iedit-make-read-only-occurrence-overlay beginning ending)
                      iedit-read-only-occurrences-overlays)
              (push (iedit-make-occurrence-overlay beginning ending)
                    iedit-occurrences-overlays))
            (setq counter (1+ counter))))))
    (iedit-update-index)
    counter))

(defun iedit-update-index (&optional point)
  "Update `iedit-occurrence-index' with the current occurrence,
if the total number of occurrences is less than
`iedit-index-update-limit'."
  (if (< (length iedit-occurrences-overlays) iedit-index-update-limit)
    (let ((pos (or point (point)))
	  (index 0))
      (dolist (occurrence iedit-occurrences-overlays)
	(if (>= pos (overlay-start occurrence))
	    (setq index (1+ index))))
      (setq iedit-occurrence-index index))))

(defun iedit-add-next-occurrence-overlay (occurrence-exp &optional point)
  "Create next occurrence overlay for `occurrence-exp'."
  (iedit-add-occurrence-overlay occurrence-exp point t))

(defun iedit-add-previous-occurrence-overlay (occurrence-exp &optional point)
  "Create previous occurrence overlay for `occurrence-exp'."
  (iedit-add-occurrence-overlay occurrence-exp point nil))

(defun iedit-add-occurrence-overlay (occurrence-exp point forward &optional bound)
  "Create next or previous occurrence overlay for `occurrence-exp'.
Return the start position of the new occurrence if successful."
  (or point
      (setq point (point)))
  (let ((case-fold-search (not iedit-case-sensitive))
        (pos nil))
    (save-excursion
      (goto-char point)
      (if (not (if forward
                   (re-search-forward occurrence-exp bound t)
                 (re-search-backward occurrence-exp bound t)))
          (message "No more matches.")
        (setq pos (match-beginning 0))
        (if (or (iedit-find-overlay-at-point (match-beginning 0) 'iedit-occurrence-overlay-name)
                (iedit-find-overlay-at-point (match-end 0) 'iedit-occurrence-overlay-name))
            (error "Conflict region"))
        (push (iedit-make-occurrence-overlay (match-beginning 0)
                                             (match-end 0))
              iedit-occurrences-overlays)
	(iedit-update-index point)
        (message "Add one match for \"%s\"." (iedit-printable occurrence-exp))
        (when iedit-hiding
          (iedit-show-all)
          (iedit-hide-context-lines iedit-occurrence-context-lines))
        ))
    pos))

(defun iedit-add-region-as-occurrence (beg end)
  "Add region as an occurrence.
The length of the region must the same as other occurrences if
there are."
  (or (= beg end)
      (error "No region"))
  (if (null iedit-occurrences-overlays)
      (push
       (iedit-make-occurrence-overlay beg end)
       iedit-occurrences-overlays)
    (or (= (- end beg) (iedit-occurrence-string-length))
        (error "Wrong region"))
    (if (or (iedit-find-overlay-at-point beg 'iedit-occurrence-overlay-name)
            (iedit-find-overlay-at-point end 'iedit-occurrence-overlay-name))
        (error "Conflict region"))
    (push (iedit-make-occurrence-overlay beg end)
          iedit-occurrences-overlays)
    (iedit-update-index)
    )) ;; todo test this function

(defun iedit-lib-start ()
  "Initialize the hooks."
  (add-hook 'post-command-hook 'iedit-update-occurrences-2 nil t)
  (setq iedit-after-change-list nil))

(defun iedit-lib-cleanup ()
  "Clean up occurrence overlay, invisible overlay and local variables."
  (remove-hook 'post-command-hook 'iedit-update-occurrences-2 t)
  (remove-overlays nil nil iedit-occurrence-overlay-name t)
  (iedit-show-all)
  (setq iedit-occurrences-overlays nil)
  (setq iedit-read-only-occurrences-overlays nil)
  (setq iedit-aborting nil)
  (setq iedit-before-modification-string "")
  (setq iedit-before-buffering-undo-list nil)
  (setq iedit-hiding nil))

(defun iedit-make-occurrence-overlay (begin end)
  "Create an overlay for an occurrence in Iedit mode.
Add the properties for the overlay: a face used to display a
occurrence's default value, and modification hooks to update
occurrences if the user starts typing."
  (let ((occurrence (make-overlay begin end (current-buffer) nil t)))
    (overlay-put occurrence iedit-occurrence-overlay-name t)
    (overlay-put occurrence 'face 'iedit-occurrence)
    (overlay-put occurrence 'keymap iedit-occurrence-keymap)
    (overlay-put occurrence 'insert-in-front-hooks '(iedit-update-occurrences))
    (overlay-put occurrence 'insert-behind-hooks '(iedit-update-occurrences))
    (overlay-put occurrence 'modification-hooks '(iedit-update-occurrences))
    (overlay-put occurrence 'priority iedit-overlay-priority)
    (overlay-put occurrence 'category 'iedit-overlay)
    occurrence))

(defun iedit-make-read-only-occurrence-overlay (begin end)
  "Create an overlay for an read-only occurrence in Iedit mode."
  (let ((occurrence (make-overlay begin end (current-buffer) nil t)))
    (overlay-put occurrence iedit-occurrence-overlay-name t)
    (overlay-put occurrence 'face 'iedit-read-only-occurrence)
    occurrence))

(defun iedit-make-invisible-overlay (begin end)
  "Create an invisible overlay from `begin` to `end`."
  (let ((invisible-overlay (make-overlay begin end (current-buffer) nil t)))
    (overlay-put invisible-overlay iedit-invisible-overlay-name t)
    (overlay-put invisible-overlay 'invisible 'iedit-invisible-overlay-name)
    ;;    (overlay-put invisible-overlay 'intangible t)
    invisible-overlay))

(defun iedit-post-undo ()
  "Check if it is time to abort iedit after undo command is executed.
This is added to `post-command-hook' when undo command is executed
in occurrences."
  (if (iedit-same-length)
      nil
    (run-hooks 'iedit-aborting-hook))
  (remove-hook 'post-command-hook 'iedit-post-undo t)
  (setq iedit-post-undo-hook-installed nil))

(defun iedit-reset-aborting ()
  "Turning Iedit mode off and reset `iedit-aborting'.
This is added to `post-command-hook' when aborting Iedit mode is
decided.  `iedit-aborting-hook' is postponed after the current
command is executed for avoiding `iedit-update-occurrences'
is called for a removed overlay."
  (run-hooks 'iedit-aborting-hook)
  (remove-hook 'post-command-hook 'iedit-reset-aborting t)
  (setq iedit-aborting nil))

;; There are two ways to update all occurrences.  One is to redefine all key
;; stroke map for overlay, the other is to figure out three basic modifications
;; in the modification hook.  This function chooses the latter.
(defun iedit-update-occurrences (occurrence after beg end &optional change)
  "Update all occurrences.
This modification hook is triggered when a user edits any
occurrence and is responsible for updating all other
occurrences. Refer to `modification-hooks' for more details.
Current supported edits are insertion, yank, deletion and
replacement.  If this modification is going out of the
occurrence, it will abort Iedit mode."
  (if undo-in-progress
      ;; If the "undo" change make occurrences different, it is going to mess up
      ;; occurrences.  So a length check will be done after undo command is executed.
      (when (not iedit-post-undo-hook-installed)
        (add-hook 'post-command-hook 'iedit-post-undo nil t)
        (setq iedit-post-undo-hook-installed t))
    (when (and (not iedit-updating) (not iedit-aborting))
      ;; before modification
      (if (null after)
          (if (or (< beg (overlay-start occurrence))
                  (> end (overlay-end occurrence)))
              (progn (setq iedit-aborting t) ; abort iedit-mode
					 (add-hook 'post-command-hook 'iedit-reset-aborting nil t))
			(setq iedit-before-modification-string
                  (buffer-substring-no-properties beg end))
			;; Check if this is called twice before modification. When inserting
			;; into zero-width occurrence or between two conjoined occurrences,
			;; both insert-in-front-hooks and insert-behind-hooks will be
			;; called.  Two calls will make `iedit-skip-modification-once' true.
			(setq iedit-skip-modification-once (not iedit-skip-modification-once)))
		;; after modification
        (if iedit-skip-modification-once
            ;; Skip the first hook
            (setq iedit-skip-modification-once nil)
		  (setq iedit-skip-modification-once t)
		  (when (and (not iedit-buffering) (not iedit-updating))
			(when (or (eq 0 change) ;; insertion
                      (eq beg end)  ;; deletion
                      (not (string= iedit-before-modification-string ;; replacement
									(buffer-substring-no-properties beg end))))
			  (let* ((inslen (- end beg))
					 (dellen change))
				(push (list occurrence
							(- beg 1)			; From 1 to beg
							(- (point-max) end) ; From end to point-max
							(- inslen dellen))	; changed number
					  iedit-after-change-list)))))))))

(defun iedit-update-occurrences-2 ()
  "The second part of updating other occurrences.
This part is running in `post-command-hook'. It combines
`iedit-after-change-list' into one change and then call the third
part to apply it to all the other occurrences."
  (when (and (not iedit-updating) iedit-after-change-list)
	(let ((beg (buffer-size))
		  (end (buffer-size))
		  (change 0))
	  (dolist (mod iedit-after-change-list)
		(setq beg (min beg (nth 1 mod)))
		(setq end (min end (nth 2 mod)))
		(setq change (+ change (nth 3 mod))))
	  (let* ((begpos (1+ beg))
			 (endpos (- (point-max) end))
			 (inslen (- endpos begpos))
			 (dellen (- inslen change))
			 (endpos (+ begpos inslen)))
		(iedit-update-occurrences-3
		 (caar iedit-after-change-list)
		 begpos
		 endpos
		 dellen)
		(setq iedit-after-change-list nil)))))
  
(defun iedit-update-occurrences-3 (occurrence beg end &optional change)
  "The third part of updateing occurrences.
Apply the change to all the other occurrences. "
  (let ((iedit-updating t)
        (offset (- beg (overlay-start occurrence)))
        (value (buffer-substring-no-properties beg end))
		;; c-before-change is really slow. It is safe to skip change functions
		;; for all the other occurrences
		(inhibit-modification-hooks (memq #'c-before-change before-change-functions)))
    (save-excursion
      (dolist (another-occurrence iedit-occurrences-overlays)
            (let* ((beginning (+ (overlay-start another-occurrence) offset))
                   (ending (+ beginning (- end beg))))
              (when (not (eq another-occurrence occurrence))
				(when change (delete-region beginning (+ beginning change))) ;; delete
				(when (/= beg end) ;; insert
				  (goto-char beginning)
				  (insert-and-inherit value)))
              (iedit-move-conjoined-overlays another-occurrence))))
	(when inhibit-modification-hooks
	  ;; run the after change functions only once. It seems OK for c-mode
	  (run-hook-with-args 'after-change-functions beg end change))))

(defun iedit-next-occurrence ()
  "Move forward to the next occurrence in the `iedit'.
If the point is already in the last occurrences, you are asked to type
another `iedit-next-occurrence', it starts again from the
beginning of the buffer."
  (interactive)
  (let* ((pos (point))
		 (ov (iedit-find-current-occurrence-overlay)))
    (if ov
		(if (iedit-find-overlay-at-point (overlay-end ov) 'iedit-occurrence-overlay-name)
			(setq pos (overlay-end ov)) ; conjoined overlay
		  ;; from inside
		  (setq pos (next-single-char-property-change pos 'iedit-occurrence-overlay-name))
		  (setq pos (next-single-char-property-change pos 'iedit-occurrence-overlay-name)))
	  ;; from outside
	  (setq pos (next-single-char-property-change pos 'iedit-occurrence-overlay-name)))
	  
    (if (/= pos (point-max))
        (setq iedit-forward-success t)
      (if (and iedit-forward-success ov)
          (progn (message "This is the last occurrence.")
                 (setq iedit-forward-success nil))
        (progn
          (if (get-char-property (point-min) 'iedit-occurrence-overlay-name)
              (setq pos (point-min))
            (setq pos (next-single-char-property-change
                       (point-min)
                       'iedit-occurrence-overlay-name)))
          (setq iedit-forward-success t)
          (message "Located the first occurrence."))))
    (when iedit-forward-success
      (iedit-update-index pos)
      (goto-char pos))))

(defun iedit-prev-occurrence ()
  "Move backward to the previous occurrence in the `iedit'.
If the point is already in the first occurrences, you are asked to type
another `iedit-prev-occurrence', it starts again from the end of
the buffer."
  (interactive)
  (let ((pos (point))
		(ov (iedit-find-current-occurrence-overlay))
		(previous-overlay))
	(when (/= pos (point-min))
	  (when ov (setq pos (overlay-start ov)))
	  (if (and ov
			   (setq previous-overlay (iedit-find-overlay-at-point (1- pos) 'iedit-occurrence-overlay-name)))
		  (setq pos (overlay-start previous-overlay))
		(setq pos (previous-single-char-property-change pos 'iedit-occurrence-overlay-name))
		(setq pos (previous-single-char-property-change pos 'iedit-occurrence-overlay-name))))
    ;; At the start of the first occurrence
    (if (or (and (eq pos (point-min))
                 (not (get-char-property (point-min) 'iedit-occurrence-overlay-name)))
            (and (eq (point) (point-min))
                 ov))
        (if (and iedit-forward-success ov)
            (progn (message "This is the first occurrence.")
                   (setq iedit-forward-success nil))
          (progn
            (setq pos (iedit-last-occurrence))
            (setq iedit-forward-success t)
            (message "Located the last occurrence.")))
      (setq iedit-forward-success t))
    (when iedit-forward-success
      (iedit-update-index pos)
      (goto-char pos))))

(defun iedit-goto-first-occurrence ()
  "Move to the first occurrence."
  (interactive)
  (goto-char (iedit-first-occurrence))
  (setq iedit-forward-success t)
  (setq iedit-occurrence-index 1)
  (message "Located the first occurrence."))

(defun iedit-first-occurrence ()
  "return the position of the first occurrence."
  (if (get-char-property (point-min) 'iedit-occurrence-overlay-name)
      (point-min)
    (next-single-char-property-change
     (point-min) 'iedit-occurrence-overlay-name)))

(defun iedit-goto-last-occurrence ()
  "Move to the last occurrence."
  (interactive)
  (goto-char (iedit-last-occurrence))
  (setq iedit-forward-success t)
  (setq iedit-occurrence-index (length iedit-occurrences-overlays))
  (message "Located the last occurrence."))

(defun iedit-last-occurrence ()
  "return the position of the last occurrence."
  (let ((ov (iedit-find-overlay-at-point (1- (point-max)) 'iedit-occurrence-overlay-name))
		(pos (point-max)))
	(if ov
		(overlay-start ov)
	  (setq pos (previous-single-char-property-change pos 'iedit-occurrence-overlay-name))
	  (overlay-start (iedit-find-overlay-at-point (1- pos) 'iedit-occurrence-overlay-name)))))

(defun iedit-show/hide-context-lines (&optional arg)
  "Show or hide context lines.
A prefix ARG specifies how many lines before and after the
occurrences are not hidden;  negative is treated the same as zero.
If no prefix argument, the prefix argument last time or default
value of `iedit-occurrence-context-lines' is used for this time."
  (interactive "P")
  (if (null arg)
      ;; toggle visible
      (progn (setq iedit-hiding (not iedit-hiding))
             (if iedit-hiding
                 (iedit-hide-context-lines iedit-occurrence-context-lines)
               (iedit-show-all)))
    ;; reset invisible lines
    (setq arg (prefix-numeric-value arg))
    (if (< arg 0)
        (setq arg 0))
    (unless (and iedit-hiding
                 (= arg iedit-occurrence-context-lines))
	  (when iedit-hiding
        (remove-overlays nil nil iedit-invisible-overlay-name t))
	  (setq iedit-occurrence-context-lines arg)
	  (setq iedit-hiding t)
	  (iedit-hide-context-lines iedit-occurrence-context-lines))))

(defun iedit-show-all()
  "Show hidden lines."
  (setq line-move-ignore-invisible nil)
  (remove-from-invisibility-spec '(iedit-invisible-overlay-name . t))
  (remove-overlays nil nil iedit-invisible-overlay-name t))

(defun iedit-hide-context-lines (visible-context-lines)
  "Hide context lines using invisible overlay."
  (let ((prev-occurrence-end 1)
        (hidden-regions nil))
    (save-excursion
      (goto-char (iedit-first-occurrence))
      (while (/= (point) (point-max))
        ;; Now at the beginning of an occurrence
        (let ((current-start (point)))
          (forward-line (- visible-context-lines))
          (let ((line-beginning (line-beginning-position)))
            (if (> line-beginning prev-occurrence-end)
                (push  (list prev-occurrence-end (1- line-beginning)) hidden-regions)))
          ;; goto the end of the occurrence
          (goto-char (next-single-char-property-change current-start 'iedit-occurrence-overlay-name)))
        (let ((current-end (point)))
          (forward-line visible-context-lines)
          (setq prev-occurrence-end (1+ (line-end-position)))
          ;; goto the beginning of next occurrence
          (goto-char (next-single-char-property-change current-end 'iedit-occurrence-overlay-name))))
      (if (< prev-occurrence-end (point-max))
          (push (list prev-occurrence-end (point-max)) hidden-regions))
      (when hidden-regions
        (set (make-local-variable 'line-move-ignore-invisible) t)
        (add-to-invisibility-spec '(iedit-invisible-overlay-name . t))
        (dolist (region hidden-regions)
          (iedit-make-invisible-overlay (car region) (cadr region)))))
    hidden-regions))

;;;; functions for overlay keymap
(defun iedit-hide-occurrence-lines ()
  "Hide occurrence lines using invisible overlay."
  (let ((hidden-regions nil)
		  (beginning  nil)
		  (end nil))
      (save-excursion
		(goto-char (iedit-first-occurrence))
		;; Now at the beginning of an occurrence
		(setq beginning (line-beginning-position))
		(while (/= (point) (point-max))
          ;; goto the end of the occurrence
          (goto-char (next-single-char-property-change (point) 'iedit-occurrence-overlay-name))
		  (setq end (line-end-position))
		  ;; goto the next beginning of the occurrence
		  (goto-char (next-single-char-property-change (point) 'iedit-occurrence-overlay-name))
		  (when (or (> (line-beginning-position) (1+ end))
					(= (line-end-position) (point-max)))
			(push (list beginning end) hidden-regions)
			(setq beginning (line-beginning-position)))))
	  (when hidden-regions
		(set (make-local-variable 'line-move-ignore-invisible) t)
		(add-to-invisibility-spec '(iedit-invisible-overlay-name . t))
		(dolist (region hidden-regions)
          (iedit-make-invisible-overlay (car region) (cadr region))))
	    ;; Value returned is for ert
	  hidden-regions))

(defun iedit-show/hide-occurrence-lines ()
  "Show or hide occurrence lines using invisible overlay."
  (interactive "*")
  (setq iedit-hiding (not iedit-hiding))
  (if (not iedit-hiding)
	  (iedit-show-all)
	(iedit-hide-occurrence-lines)))

(defun iedit-apply-on-occurrences (function &rest args)
  "Call function for each occurrence."
  (let ((iedit-updating t))
      (save-excursion
        (dolist (occurrence iedit-occurrences-overlays)
          (apply function (overlay-start occurrence) (overlay-end occurrence) args)))))

(defun iedit-upcase-occurrences ()
  "Covert occurrences to upper case."
  (interactive "*")
  (iedit-barf-if-buffering)
  (iedit-apply-on-occurrences 'upcase-region))

(defun iedit-downcase-occurrences()
  "Covert occurrences to lower case."
  (interactive "*")
  (iedit-barf-if-buffering)
  (iedit-apply-on-occurrences 'downcase-region))

(defun iedit-number-occurrences (start-at &optional format-string)
  "Insert numbers in front of the occurrences.
START-AT, if non-nil, should be a number from which to begin
counting.  FORMAT, if non-nil, should be a format string to pass
to `format-string' along with the line count.  When called
interactively with a prefix argument, prompt for START-AT and
FORMAT."
  (interactive
   (if current-prefix-arg
       (let* ((start-at (read-number "Number to count from: " 1)))
		 (list start-at
			   (read-string
				(format "Format incremented numbers (default '%s'): "
						iedit-increment-format-string)
				nil nil iedit-increment-format-string)))
	 (list 1 iedit-increment-format-string)))
  (iedit-barf-if-buffering)
  (let ((number start-at)
        (iedit-updating t))
    (save-excursion
	  (goto-char (iedit-first-occurrence))
	  (cl-loop for counter from number
			   for ov = (iedit-find-current-occurrence-overlay)
			   while (/= (point) (point-max)) 
			   do (progn
		  (if (re-search-forward "\\\\#" (overlay-end ov) t)
			  (replace-match (format format-string counter) t)
			(insert (format format-string counter)))
		  (iedit-move-conjoined-overlays ov)
		  ;; goto the beginning of the next occourrence overlay
		  (if (iedit-find-overlay-at-point (overlay-end ov) 'iedit-occurrence-overlay-name)
			  (goto-char (overlay-end ov)) ; conjoined overlay
			(when (< (point) (overlay-end ov))
			  (goto-char (next-single-char-property-change (point) 'iedit-occurrence-overlay-name)))
			(goto-char (next-single-char-property-change (point) 'iedit-occurrence-overlay-name))))))))

;;; Don't downcase from-string to allow case freedom!
(defun iedit-replace-occurrences(&optional to-string)
  "Replace occurrences with STRING."
  (interactive "*")
  (iedit-barf-if-buffering)
  (let* ((ov (iedit-find-current-occurrence-overlay))
         (offset (- (point) (overlay-start ov)))
         (from-string (buffer-substring-no-properties
                       (overlay-start ov)
                       (overlay-end ov)))
         (to-string (if (not to-string)
			(read-string "Replace with: "
				     nil nil
				     from-string
				     nil)
                      to-string)))
    (iedit-apply-on-occurrences
     (lambda (beg end from-string to-string)
       (goto-char beg)
       (search-forward from-string end)
       (replace-match to-string t))
     from-string to-string)
    (goto-char (+ (overlay-start ov) offset))))

(defun iedit-blank-occurrences()
  "Replace occurrences with blank spaces."
  (interactive "*")
  (iedit-barf-if-buffering)
  (let* ((ov (car iedit-occurrences-overlays))
         (offset (- (point) (overlay-start ov)))
         (count (- (overlay-end ov) (overlay-start ov))))
    (iedit-apply-on-occurrences
     (lambda (beg end )
       (delete-region beg end)
       (goto-char beg)
       (insert-and-inherit (make-string count 32))))
    (goto-char (+ (overlay-start ov) offset))))

(defun iedit-delete-occurrences()
  "Delete occurrences."
  (interactive "*")
  (iedit-barf-if-buffering)
  (iedit-apply-on-occurrences 'delete-region))

;; todo: add cancel buffering function
(defun iedit-toggle-buffering ()
  "Toggle buffering.
This is intended to improve iedit's response time.  If the number
of occurrences are huge, it might be slow to update all the
occurrences for each key stoke.  When buffering is on,
modification is only applied to the current occurrence and will
be applied to other occurrences when buffering is off."
  (interactive "*")
  (if iedit-buffering
      (iedit-stop-buffering)
    (iedit-start-buffering))
  (message (concat "Modification Buffering "
                   (if iedit-buffering
                       "started."
                     "stopped."))))

(defun iedit-start-buffering ()
  "Start buffering."
  (setq iedit-buffering t)
  (setq iedit-before-buffering-string (iedit-current-occurrence-string))
  (setq iedit-before-buffering-undo-list buffer-undo-list)
  (setq iedit-before-buffering-point (point))
  (buffer-disable-undo)
  (message "Start buffering editing..."))

(defun iedit-stop-buffering ()
  "Stop buffering and apply the modification to other occurrences.
If current point is not at any occurrence, the buffered
modification is not going to be applied to other occurrences."
  (let ((ov (iedit-find-current-occurrence-overlay)))
    (when ov
      (let* ((beg (overlay-start ov))
             (end (overlay-end ov))
             (modified-string (buffer-substring-no-properties beg end))
             (offset (- (point) beg)) ;; delete-region moves cursor
             (iedit-updating t))
        (when (not (string= iedit-before-buffering-string modified-string))
          (save-excursion
            ;; Rollback the current modification and buffer-undo-list. This is
            ;; to avoid the inconsistency if user undoes modifications
            (delete-region beg end)
            (goto-char beg)
            (insert-and-inherit iedit-before-buffering-string)
			(goto-char iedit-before-buffering-point)
			(buffer-enable-undo)
            (setq buffer-undo-list iedit-before-buffering-undo-list)
			;; go back here if undo
			(push (point) buffer-undo-list)
            (dolist (occurrence iedit-occurrences-overlays) ; todo:extract as a function
              (let ((beginning (overlay-start occurrence))
                    (ending (overlay-end occurrence)))
                (delete-region beginning ending)
                (unless (eq beg end) ;; replacement
                  (goto-char beginning)
                  (insert-and-inherit modified-string))
                (iedit-move-conjoined-overlays occurrence))))
          (goto-char (+ (overlay-start ov) offset))))))
  (setq iedit-buffering nil)
  (message "Buffered modification applied.")
  (setq iedit-before-buffering-undo-list nil))

(defun iedit-move-conjoined-overlays (occurrence)
  "This function keeps overlays conjoined after modification.
After modification, conjoined overlays may be overlapped."
  (let ((beginning (overlay-start occurrence))
        (ending (overlay-end occurrence)))
    (unless (= beginning (point-min))
      (let ((previous-overlay (iedit-find-overlay-at-point
                               (1- beginning)
                               'iedit-occurrence-overlay-name)))
        (if previous-overlay ; two conjoined occurrences
            (move-overlay previous-overlay
                          (overlay-start previous-overlay)
                          beginning))))
    (unless (= ending (point-max))
      (let ((next-overlay (iedit-find-overlay-at-point
                           ending
                           'iedit-occurrence-overlay-name)))
        (if next-overlay ; two conjoined occurrences
            (move-overlay next-overlay ending (overlay-end next-overlay)))))))

;;; help functions
(defun iedit-find-current-occurrence-overlay ()
  "Return the current occurrence overlay  at point or point - 1.
This function is supposed to be called in overlay keymap."
  (or (iedit-find-overlay-at-point (point) 'iedit-occurrence-overlay-name)
      (iedit-find-overlay-at-point (1- (point)) 'iedit-occurrence-overlay-name)))

(defun iedit-find-overlay-at-point (point property)
  "Return the overlay with PROPERTY at POINT."
  (let ((overlays (overlays-at point))
        found)
    (while (and overlays (not found))
      (let ((overlay (car overlays)))
        (if (overlay-get overlay property)
            (setq found overlay)
          (setq overlays (cdr overlays)))))
    found))

(defun iedit-same-column ()
  "Return t if all occurrences are at the same column."
  (save-excursion
    (let ((column (progn (goto-char (overlay-start (car iedit-occurrences-overlays)))
                         (current-column)))
          (overlays (cdr  iedit-occurrences-overlays))
          (same t))
      (while (and overlays same)
        (let ((overlay (car overlays)))
          (if (/= (progn (goto-char (overlay-start overlay))
                         (current-column))
                  column)
              (setq same nil)
            (setq overlays (cdr overlays)))))
      same)))

(defun iedit-same-length ()
  "Return t if all occurrences are the same length."
  (save-excursion
    (let ((length (iedit-occurrence-string-length))
          (overlays (cdr iedit-occurrences-overlays))
          (same t))
      (while (and overlays same)
        (let ((ov (car overlays)))
          (if (/= (- (overlay-end ov) (overlay-start ov))
                  length)
              (setq same nil)
            (setq overlays (cdr overlays)))))
      same)))

;; This function might be called out of any occurrence
(defun iedit-current-occurrence-string ()
  "Return current occurrence string.
Return nil if occurrence string is empty string."
  (let ((ov (or (iedit-find-current-occurrence-overlay)
                 (car iedit-occurrences-overlays))))
    (if ov
        (let ((beg (overlay-start ov))
              (end (overlay-end ov)))
          (if (/=  beg end)
              (buffer-substring-no-properties beg end)
            nil))
      nil)))

(defun iedit-occurrence-string-length ()
  "Return the length of current occurrence string."
  (let ((ov (car iedit-occurrences-overlays)))
    (- (overlay-end ov) (overlay-start ov))))

(defun iedit-find-overlay (beg end property &optional exclusive)
  "Return a overlay with property in region, or out of the region if EXCLUSIVE is not nil."
  (if exclusive
      (or (iedit-find-overlay-in-region (point-min) beg property)
          (iedit-find-overlay-in-region end (point-max) property))
    (iedit-find-overlay-in-region beg end property)))

(defun iedit-find-overlay-in-region (beg end property)
  "Return a overlay with property in region."
  (let ((overlays (overlays-in beg end))
        found)
    (while (and overlays (not found))
      (let ((overlay (car overlays)))
        (if (and (overlay-get overlay property)
                 (>= (overlay-start overlay) beg)
                 (<= (overlay-end overlay) end))
            (setq found overlay)
          (setq overlays (cdr overlays)))))
    found))

(defun iedit-cleanup-occurrences-overlays (beg end &optional inclusive)
  "Remove deleted overlays from list `iedit-occurrences-overlays'."
  (if inclusive
      (remove-overlays beg end iedit-occurrence-overlay-name t)
    (remove-overlays (point-min) beg iedit-occurrence-overlay-name t)
    (remove-overlays end (point-max) iedit-occurrence-overlay-name t))
  (let (overlays)
    (dolist (overlay iedit-occurrences-overlays)
      (if (overlay-buffer overlay)
          (push overlay overlays)))
    (setq iedit-occurrences-overlays overlays)
    (iedit-update-index)))

(defun iedit-printable (string)
  "Return a omitted substring that is not longer than 50.
STRING is already `regexp-quote'ed"
  (let ((first-newline-index (string-match "$" string))
        (length (length string)))
    (if (and first-newline-index
             (/= first-newline-index length))
        (if (< first-newline-index 50)
            (concat (substring string 0 first-newline-index) "...")
          (concat (substring string 0 50) "..."))
      (if (> length 50)
          (concat (substring string 0 50) "...")
        string))))

(defun iedit-char-at-bol (&optional N)
  "Get char position of the beginning of the current line. If `N'
is given, move forward (or backward) that many lines (using
`forward-line') and get the char position at the beginning of
that line."
  (save-excursion
    (forward-line (if N N 0))
    (point)))

(defun iedit-char-at-eol (&optional N)
  "Get char position of the end of the current line. If `N' is
given, move forward (or backward) that many lines (using
`forward-line') and get the char position at the end of that
line."
  (save-excursion
    (forward-line (if N N 0))
    (end-of-line)
    (point)))

(defun iedit-region-active ()
  "Return t if region is active and not empty.
If variable `iedit-transient-mark-sensitive' is t, active region
means `transient-mark-mode' is on and mark is active. Otherwise,
it just means mark is active."
  (and (if iedit-transient-mark-sensitive
           transient-mark-mode
         t)
       mark-active
       (not (equal (mark) (point)))))

(defun iedit-barf-if-lib-active()
  "Signal error if Iedit lib is active."
  (or (and (null iedit-occurrences-overlays)
           (null iedit-read-only-occurrences-overlays))
      (error "Iedit lib is active")))

(defun iedit-barf-if-buffering()
  "Signal error if Iedit lib is buffering."
  (or  (null iedit-buffering)
      (error "Iedit is buffering")))

;; (provide 'iedit-lib)

;;; iedit-lib.el ends here


(eval-when-compile
  (require 'cl-lib)
  (require 'sgml-mode))
;; (require 'iedit-lib)

(defcustom iedit-toggle-key-default (kbd "C-;")
  "If no-nil, the key is inserted into global-map,
isearch-mode-map, esc-map and help-map."
  :type 'vector
  :group 'iedit)

(defvar iedit-mode-hook nil
  "Function(s) to call after starting up an iedit.")

(defvar iedit-mode-end-hook nil
  "Function(s) to call after terminating an iedit.")

(defvar iedit-mode nil) ;; Name of the minor mode

(defcustom iedit-auto-narrow nil
  "If no-nil, the buffer is narrowed temporairily if iedit-mode
is enabled on current defun."
  :type 'boolean
  :group 'iedit)

(defvar iedit-is-narrowed nil
  "This is buffer local variable which indicates if the buffer is
  narrowed by iedit temporarily.")

(defvar iedit-use-symbol-boundaries t
  "If no-nil, matches have to start and end at symbol boundaries. Otherwise,
matches starts and end at word boundaries.")

(defvar iedit-occurrence-type-local 'symbol
  "This is buffer local variable which indicates the occurrence
type. It might be (symbol word email url markup-tag regexp selection other).")

(defvar iedit-occurrence-type-global 'symbol
  "This is global variable which indicates the last global occurrence
type. It might be (symbol word email url markup-tag regexp selection other).")

(defvar iedit-last-occurrence-local nil
  "This is buffer local variable which is the occurrence when
Iedit mode is turned off last time.")

(defvar iedit-last-occurrence-global nil
  "This is global variable which is the occurrence when
Iedit mode is turned off last time.")

(defvar iedit-last-initial-string-global nil
  "This is a global variable which is the last initial occurrence string.")

(defvar iedit-initial-string-local nil
  "This is buffer local variable which is the initial string to start Iedit mode.")
(defvar iedit-initial-region nil
  "This is buffer local variable which is the initial region
where Iedit mode is started from.")

(defvar iedit-num-lines-to-expand-up 0
  "This is a global variable indicating how many lines up from
point should be included in the replacement region.")

(defvar iedit-num-lines-to-expand-down 0
  "This is a global variable indicating how many lines down from
point should be included in the replacement region.")

(defvar iedit-default-occurrence-local nil
  "This is a function which returns a string as occurrence candidate.
It is called in `iedit-default-occurrence'.  This buffer local
variable can be configured in some modes.  An example of how to
use this variable:
(add-hook 'perl-mode-hook
          '(lambda ()
             (setq iedit-default-occurrence-local
                   '(lambda ()
                      (let* ((bound (bounds-of-thing-at-point 'symbol))
                             (prefix-char (char-after (1- (car bound)))))
                        (if (memq prefix-char '(?$ ?% ?@ ?*))
                            (progn
                              (setq iedit-occurrence-type-local 'regexp)
                              (concat (regexp-quote (buffer-substring-no-properties (1- (car bound)) (cdr bound))) \"\\\\_>\"))
                          (buffer-substring-no-properties (car bound) (cdr bound))))))))
'$%@*' will be included in the occurrences in perl mode.")

(defcustom iedit-mode-line
  `(" Iedit:" (:eval (format ,(propertize "%d/%d" 'face 'font-lock-warning-face)
                             iedit-occurrence-index (iedit-counter))))
  "Mode-line format for Iedit.
This should be set before Iedit is loaded."
  :type 'string
  :group 'iedit)
(put 'iedit-mode-line 'risky-local-variable t)

(make-variable-buffer-local 'iedit-mode)
(make-variable-buffer-local 'iedit-use-symbol-boundaries)
(make-variable-buffer-local 'iedit-occurrence-type-local)
(make-variable-buffer-local 'iedit-last-occurrence-local)
(make-variable-buffer-local 'iedit-initial-string-local)
(make-variable-buffer-local 'iedit-initial-region)
(make-variable-buffer-local 'iedit-default-occurrence-local)
(make-variable-buffer-local 'iedit-is-narrowed)

(or (assq 'iedit-mode minor-mode-alist)
    (nconc minor-mode-alist
           (list `(iedit-mode ,iedit-mode-line))))

;;; Define iedit help map.
(eval-when-compile (require 'help-macro))

(defvar iedit-help-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector (event-convert-list `(,help-char))) 'iedit-help-for-help)
    (define-key map [help] 'iedit-help-for-help)
    (define-key map [f1] 'iedit-help-for-help)
    (define-key map "?" 'iedit-help-for-help)
    (define-key map "b" 'iedit-describe-bindings)
    (define-key map "k" 'iedit-describe-key)
    (define-key map "m" 'iedit-describe-mode)
    (define-key map "q" 'help-quit)
    map)
  "Keymap for characters following the Help key for Iedit mode.")

(make-help-screen
 iedit-help-for-help-internal
 (purecopy "Type a help option: [bkm] or ?")
 "You have typed %THIS-KEY%, the help character.  Type a Help option:
\(Type \\<help-map>\\[help-quit] to exit the Help command.)
b           Display all Iedit key bindings.
k KEYS      Display full documentation of Iedit key sequence.
m           Display documentation of Iedit mode.
You can't type here other help keys available in the global help map,
but outside of this help window when you type them in Iedit mode,
they exit Iedit mode before displaying global help."
 iedit-help-map)

(defun iedit-help-for-help ()
  "Display Iedit help menu."
  (interactive)
  (let (same-window-buffer-names same-window-regexps)
    (iedit-help-for-help-internal)))

(defun iedit-describe-bindings ()
  "Show a list of all keys defined in Iedit mode, and their definitions.
This is like `describe-bindings', but displays only Iedit keys."
  (interactive)
  (let (same-window-buffer-names
        same-window-regexps
        (keymap (substitute-command-keys "\\{iedit-mode-keymap}\\{iedit-mode-occurrence-keymap}")))
    (with-help-window "*Help*"
      (with-current-buffer standard-output
        (princ "Iedit Mode Bindings: ")
        (princ keymap)))))

(defun iedit-describe-key ()
  "Display documentation of the function invoked by Iedit mode key."
  (interactive)
  (let (same-window-buffer-names same-window-regexps)
    (call-interactively 'describe-key)))

(defun iedit-describe-mode ()
  "Display documentation of Iedit mode."
  (interactive)
  (let (same-window-buffer-names same-window-regexps)
    (describe-function 'iedit-mode)))

(defun iedit-counter ()
  "Return the number of active occurrences."
  (length iedit-occurrences-overlays))

;;; Default key bindings:
(when (and iedit-toggle-key-default (null (where-is-internal 'iedit-mode)))
  (let ((key-def (lookup-key (current-global-map) iedit-toggle-key-default)))
    (if key-def
        (display-warning 'iedit (format "Iedit default key %S is occupied by %s."
                                        (key-description iedit-toggle-key-default)
                                        key-def)
                         :warning)
      (define-key global-map iedit-toggle-key-default 'iedit-mode)
      (define-key isearch-mode-map iedit-toggle-key-default 'iedit-mode-from-isearch)
      (define-key esc-map iedit-toggle-key-default 'iedit-execute-last-modification)
      (define-key help-map iedit-toggle-key-default 'iedit-mode-toggle-on-function)
      (message "Iedit default key binding is %s" (key-description iedit-toggle-key-default)))))

;; Avoid to restore Iedit mode when restoring desktop
(add-to-list 'desktop-minor-mode-handlers
             '(iedit-mode . nil))

;;; Define iedit help map.
(eval-when-compile (require 'help-macro))

(defvar iedit-mode-occurrence-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map iedit-occurrence-keymap-default)
    (define-key map (kbd "M-H") 'iedit-restrict-function)
    (define-key map (kbd "M-I") 'iedit-restrict-current-line)
    (define-key map (kbd "M-{") 'iedit-expand-up-a-line)
    (define-key map (kbd "M-}") 'iedit-expand-down-a-line)
    (define-key map (kbd "M-p") 'iedit-expand-up-to-occurrence)
    (define-key map (kbd "M-n") 'iedit-expand-down-to-occurrence)
    (define-key map (kbd "M-G") 'iedit-apply-global-modification)
    (define-key map (kbd "M-C") 'iedit-toggle-case-sensitive)
    map)
  "Keymap used within overlays in Iedit mode.")

(defvar iedit-mode-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map iedit-lib-keymap)
    (define-key map (vector (event-convert-list `(,help-char))) iedit-help-map)
    (define-key map [help] iedit-help-map)
    (define-key map [f1] iedit-help-map)
    (define-key map (kbd "M-;") 'iedit-toggle-selection)
    map)
  "Keymap used while Iedit mode is enabled.")

;;; Define Iedit mode map
(or (assq 'iedit-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'iedit-mode iedit-mode-keymap) minor-mode-map-alist)))

;;;###autoload
(defun iedit-mode (&optional arg)
  "Toggle Iedit mode.
This command behaves differently, depending on the mark, point,
prefix argument and variable `iedit-transient-mark-sensitive'.
If Iedit mode is off, turn Iedit mode on.
When Iedit mode is turned on, all the occurrences of the current
region in the buffer (possibly narrowed) or a region are
highlighted.  If one occurrence is modified, the change are
propagated to all other occurrences simultaneously.
If region is not active, `iedit-default-occurrence' is called to
get an occurrence candidate, according to the thing at point.  It
might be url, email address, markup tag or current symbol(or
word).
In the above two situations, with digit prefix argument 0, only
occurrences in current function are matched.  This is good for
renaming refactoring in programming.
You can also switch to Iedit mode from isearch mode directly. The
current search string is used as occurrence.  All occurrences of
the current search string are highlighted.
With an universal prefix argument, the occurrence when Iedit mode
is turned off last time in current buffer is used as occurrence.
This is intended to recover last Iedit mode which is turned off.
If region active, Iedit mode is limited within the current
region.
With repeated universal prefix argument, the occurrence when
Iedit mode is turned off last time (might be in other buffer) is
used as occurrence.  If region active, Iedit mode is limited
within the current region.
With digital prefix argument 1, Iedit mode is limited on the
current symbol or the active region, which means just one
instance is highlighted.  This behavior serves as a start point
of incremental selection work flow.
If Iedit mode is on and region is active, Iedit mode is
restricted in the region, e.g. the occurrences outside of the
region is excluded.
If Iedit mode is on and region is active, with an universal
prefix argument, Iedit mode is restricted outside of the region,
e.g. the occurrences in the region is excluded.
Turn off Iedit mode in other situations.
Commands:
\\{iedit-mode-keymap}
Keymap used within overlays:
\\{iedit-mode-occurrence-keymap}"
  (interactive "P")
  (if iedit-mode
      (iedit-mode-on-action arg)
    (iedit-barf-if-lib-active)
    (let (occurrence
          (beg (if (eq major-mode 'occur-edit-mode) ; skip the first occurrence
                   (next-single-char-property-change 1 'read-only)
                 (point-min)))
          (end (point-max)))
      ;; Get the occurrence and iedit-occurrence-type-local
      (cond ((and arg
                  (= 4 (prefix-numeric-value arg))
                  iedit-last-occurrence-local)
             (setq occurrence iedit-last-occurrence-local))
            ((and arg
                  (= 16 (prefix-numeric-value arg))
                  iedit-last-initial-string-global)
             (setq occurrence iedit-last-initial-string-global)
             (setq iedit-occurrence-type-local iedit-occurrence-type-global))
            ((iedit-region-active)
             (setq occurrence  (buffer-substring-no-properties
                                (mark) (point)))
             (setq iedit-occurrence-type-local 'selection))
            (t (setq occurrence (iedit-default-occurrence))
               (unless occurrence
                 (error "No candidate of the occurrence, cannot enable Iedit mode"))))
      ;; Get the scope
      (when arg
        (cond ((= 0 (prefix-numeric-value arg))
               (save-excursion
		 ;; Since Emacs 26.1, `mark-defun' marks the next defun if the
		 ;; mark is active.
		 (deactivate-mark t)
                 (mark-defun)
                 (setq beg (region-beginning))
                 (setq end (region-end)))
	       (when (and iedit-auto-narrow (not (buffer-narrowed-p)))
		 (narrow-to-region beg end)
		 (setq iedit-is-narrowed t)))
              ((and (= 1 (prefix-numeric-value arg))
                    (not (iedit-region-active)))
               (let ((region (bounds-of-thing-at-point 'symbol)))
                 (setq beg (car region))
                 (setq end (cdr region))))
              ((iedit-region-active)
                (setq beg (region-beginning))
                (setq end (region-end)))))
      (setq mark-active nil)
      (run-hooks 'deactivate-mark-hook)
      (setq iedit-initial-string-local occurrence)
      (iedit-start (iedit-regexp-quote occurrence) beg end)
      (unless iedit-occurrences-overlays
        ;; (message "No matches found for %s" (iedit-regexp-quote occurrence))
        (iedit-done)))))

(unless (boundp 'isearch-regexp-function)
  (defvaralias 'isearch-regexp-function 'isearch-word))
(defun iedit-mode-from-isearch (regexp)
  "Start Iedit mode using last search string as the regexp."
  (interactive
   (let ((regexp (cond
		  ((functionp isearch-regexp-function)
                   (funcall isearch-regexp-function isearch-string))
                  (isearch-regexp-function (word-search-regexp isearch-string))
                  (isearch-regexp isearch-string)
                  (t (regexp-quote isearch-string)))))
     (list regexp)))
  (or isearch-success
      (error "No match" ))
  (if (or isearch-regexp isearch-regexp-function)
      nil
    (setq iedit-initial-string-local isearch-string))
  (let ((iedit-case-sensitive (not isearch-case-fold-search))
	result)
    (isearch-exit)
    (setq mark-active nil)
    (run-hooks 'deactivate-mark-hook)
    (when iedit-mode
      (iedit-lib-cleanup))
    (setq result
	  (catch 'not-same-length
	    (iedit-start regexp (point-min) (point-max))))
    (cond ((not iedit-occurrences-overlays)
           (message "No matches found for %s" regexp)
           (iedit-done))
          ((equal result 'not-same-length)
           (message "Matches are not the same length.")
           (iedit-done)))))

(defun iedit-start (occurrence-regexp beg end)
  "Start Iedit mode for the `occurrence-regexp' in the current buffer."
  ;; enforce skip modification once, errors may happen to cause this to be
  ;; unset.
  (setq iedit-skip-modification-once t)
  (setq iedit-initial-region (list beg end))
  (let ((counter 0))
    (if (eq iedit-occurrence-type-local 'markup-tag)
        (progn
          (setq iedit-occurrence-keymap iedit-occurrence-keymap-default)
          (iedit-make-markers-overlays iedit-occurrences-overlays)
          (setq counter 2))
      (setq iedit-occurrence-keymap iedit-mode-occurrence-keymap)
      (setq counter (iedit-make-occurrences-overlays occurrence-regexp beg end)))
    (message "%d matches for \"%s\""
             counter
             (iedit-printable occurrence-regexp))
    (setq iedit-mode t))
  (when iedit-auto-buffering
	(iedit-start-buffering))
  (iedit-lib-start)
  (run-hooks 'iedit-mode-hook)
  (add-hook 'before-revert-hook 'iedit-done nil t)
  (add-hook 'kbd-macro-termination-hook 'iedit-done nil t)
  (add-hook 'change-major-mode-hook 'iedit-done nil t)
  (add-hook 'iedit-aborting-hook 'iedit-done nil t))

(defun iedit-default-occurrence()
  "This function returns a string as occurrence candidate.
The candidate depends on the thing at point."
  (let (occurrence-str)
    (cond
     ((thing-at-point 'url)
      (setq occurrence-str (thing-at-point 'url))
      (setq iedit-occurrence-type-local 'url))

     ((thing-at-point 'email)
      (setq occurrence-str (thing-at-point 'email))
      (setq iedit-occurrence-type-local 'email))

     (iedit-default-occurrence-local
      (setq occurrence-str (funcall iedit-default-occurrence-local)))
     ;; Try to mark sgml pair anyway
     ((and (not (bound-and-true-p sgml-electric-tag-pair-mode))
           (setq occurrence-str (iedit-mark-sgml-pair)))
      (setq iedit-occurrence-type-local 'markup-tag))

     ((and iedit-use-symbol-boundaries ;option
           (thing-at-point 'symbol))
      (setq occurrence-str (thing-at-point 'symbol))
      (setq iedit-occurrence-type-local 'symbol))

     ((thing-at-point 'word)
      (setq occurrence-str (thing-at-point 'word))
      (setq iedit-occurrence-type-local 'word)))
    occurrence-str))

(defun iedit-regexp-quote (exp)
  "Return a regexp string."
  (cl-case iedit-occurrence-type-local
    ('symbol (concat "\\_<" (regexp-quote exp) "\\_>"))
    ('word   (concat "\\<" (regexp-quote exp) "\\>"))
    ('regexp exp)
    ( t      (regexp-quote exp))))

(defun iedit-mark-sgml-pair ()
  "Check if the cursor is on a markup tag.
If the cursor is on a markup tag, the position of the opening and
closing markup tags are saved in `iedit-occurrence-overlays'
temporarily.
The code is adapted from
`sgml-electric-tag-pair-before-change-function'.
Return the tag if succeeded, nil if failed."
  (condition-case err
  (save-excursion
    (skip-chars-backward "[:alnum:]-_.:")
    (if  (or (eq (char-before) ?<)
             (and (eq (char-before) ?/)
                  (eq (char-before (1- (point))) ?<)))
        (let* ((endp (eq (char-before) ?/))
               (cl-start (point))
               (cl-end (progn (skip-chars-forward "[:alnum:]-_.:") (point)))
               (match
                (if endp
                    (with-no-warnings (when (sgml-skip-tag-backward 1) (forward-char 1) t))
                  (with-syntax-table sgml-tag-syntax-table
                    (up-list -1)
                    (with-no-warnings (when (sgml-skip-tag-forward 1))
                      (backward-sexp 1)
                      (forward-char 2)
                      t)))))
          (when (and match
                     (/= cl-end cl-start)
                     (equal (buffer-substring cl-start cl-end)
                            (buffer-substring (point)
                                              (save-excursion
                                                (skip-chars-forward "[:alnum:]-_.:")
                                                (point))))
                     (or (not endp) (eq (char-after cl-end) ?>)))
            (push (cons cl-start cl-end) iedit-occurrences-overlays)
            (push (cons (point) (+ (point) (- cl-end cl-start))) iedit-occurrences-overlays)
            (buffer-substring cl-start cl-end)))))
  (error nil)))

(defun iedit-done ()
  "Exit Iedit mode.
Save the current occurrence string locally and globally.  Save
the initial string globally."
  (when iedit-buffering
      (iedit-stop-buffering))
  (setq iedit-last-occurrence-local (iedit-current-occurrence-string))
  (setq iedit-occurrence-type-global iedit-occurrence-type-local)
  (setq iedit-last-occurrence-global iedit-last-occurrence-local)
  (setq iedit-last-initial-string-global iedit-initial-string-local)
  (if iedit-last-occurrence-local
      (kill-new iedit-last-occurrence-local)) ; Make occurrence the latest kill in the kill ring.
  (setq iedit-num-lines-to-expand-up 0)
  (setq iedit-num-lines-to-expand-down 0)

  (iedit-lib-cleanup)

  (when iedit-is-narrowed
    (widen)
    (setq iedit-is-narrowed nil))
  (setq iedit-initial-string-local nil)
  (setq iedit-mode nil)
  (force-mode-line-update)
  (remove-hook 'before-revert-hook 'iedit-done t)
  (remove-hook 'kbd-macro-termination-hook 'iedit-done t)
  (remove-hook 'change-major-mode-hook 'iedit-done t)
  (remove-hook 'iedit-aborting-hook 'iedit-done t)
  (run-hooks 'iedit-mode-end-hook))

(defun iedit-mode-on-action (&optional arg)
  "Turn off Iedit mode or restrict it in a region if region is active."
  (cond ((iedit-region-active)
	 (iedit-restrict-region (region-beginning) (region-end) arg))
	((and arg
	      (= 0 (prefix-numeric-value arg)))
	 (iedit-restrict-function nil))
	(t (iedit-done))))

;;;###autoload
(defun iedit-mode-toggle-on-function ()
  "Toggle Iedit mode on current function."
  (interactive)
  (if iedit-mode
	  (iedit-done)
	(iedit-mode 0)))

(defun iedit-execute-last-modification (&optional arg)
  "Apply last modification in Iedit mode to the current buffer or an active region."
  (interactive "*P")
  (or (and iedit-last-initial-string-global
           (not (string= iedit-last-initial-string-global iedit-last-occurrence-global)))
      (error "No modification available"))
  (let ((occurrence-exp (regexp-quote iedit-last-initial-string-global))
        (replacement  iedit-last-occurrence-global)
        (case-fold-search (not iedit-case-sensitive))
        beg end)
    (when case-fold-search
      (setq occurrence-exp (downcase occurrence-exp))
      (setq replacement (downcase replacement)))
    ;; `iedit-regexp-quote' depends on iedit-occurrence-type-local
    (setq iedit-occurrence-type-local iedit-occurrence-type-global)
    (setq occurrence-exp (iedit-regexp-quote  occurrence-exp))
    (when (iedit-region-active)
      (setq beg (region-beginning))
      (setq end (region-end)))
    (perform-replace occurrence-exp replacement t t nil nil nil beg end)))

(defun iedit-apply-global-modification ()
  "Apply last global modification."
  (interactive "*")
  (if (and iedit-last-initial-string-global
           (string= iedit-initial-string-local iedit-last-initial-string-global)
           (not (string= iedit-last-initial-string-global iedit-last-occurrence-global)))
      (iedit-replace-occurrences iedit-last-occurrence-global)
    (message "No global modification available.")))

(defun iedit-toggle-selection ()
  "Select or deselect the occurrence under point."
  (interactive)
  (iedit-barf-if-buffering)
  (let ((ov (iedit-find-current-occurrence-overlay)))
    (if ov
        (iedit-restrict-region (overlay-start ov) (overlay-end ov) t)
      (let ((current-occurrence-string (iedit-current-occurrence-string)))
        (when (not (null current-occurrence-string))
          (save-excursion
            (goto-char (if (> (point) (length current-occurrence-string))
                           ( - (point) (length current-occurrence-string))
                         (point-min)))
            (iedit-add-next-occurrence-overlay
             (iedit-regexp-quote current-occurrence-string)))
          (force-mode-line-update))))))

(defun iedit-restrict-function(&optional arg)
  "Restricting Iedit mode in current function."
  (interactive "P")
  (let (beg end)
    (save-excursion
      (deactivate-mark t)
      (mark-defun)
      (setq beg (region-beginning))
      (setq end (region-end)))
    (iedit-restrict-region beg end arg)
    (when (and (not arg)
    	       iedit-auto-narrow
    	       (not (buffer-narrowed-p)))
      (narrow-to-region beg end)
      (setq iedit-is-narrowed t)))
  (message "Restricted in current function, %d matches."
           (length iedit-occurrences-overlays)))

(defun iedit-restrict-current-line ()
  "Restrict Iedit mode to current line."
  (interactive)
  (iedit-restrict-region (iedit-char-at-bol) (iedit-char-at-eol))
  (setq iedit-num-lines-to-expand-up 0
        iedit-num-lines-to-expand-down 0)
  (message "Restricted to current line, %d match%s."
           (length iedit-occurrences-overlays)
           (if (= 1 (length iedit-occurrences-overlays)) "" "es")))

(defun iedit-expand-by-a-line (where amount)
  "Expands the top or bottom of the search region upwards or
downwards by `amount' lines. The region being acted upon is
controlled with `where' ('top to act on the top, anything else
for the bottom).  If amount is negative, collapses the top or
bottom of the search region by `-amount' lines."
  (let ((occurrence (iedit-current-occurrence-string)))
    (iedit-lib-cleanup)
    (if (eq where 'top)
        (setq iedit-num-lines-to-expand-up
              (max 0 (+ amount iedit-num-lines-to-expand-up)))
      (setq iedit-num-lines-to-expand-down
            (max 0 (+ amount iedit-num-lines-to-expand-down))))
    (iedit-start (iedit-regexp-quote occurrence)
                 (iedit-char-at-bol (- iedit-num-lines-to-expand-up))
                 (iedit-char-at-eol iedit-num-lines-to-expand-down))
    (message "Now looking -%d/+%d lines around current line, %d match%s."
             iedit-num-lines-to-expand-up
             iedit-num-lines-to-expand-down
             (length iedit-occurrences-overlays)
             (if (= 1 (length iedit-occurrences-overlays)) "" "es"))))

(defun iedit-expand-up-a-line (&optional N)
  "After start iedit-mode only on current symbol or the active
region, this function expands the search region upwards by N
line.  N defaults to 1.  If N is negative, collapses the top of
the search region by `-N' lines."
  (interactive "p")
  (iedit-expand-by-a-line 'top N))
  
(defun iedit-expand-down-a-line (&optional N)
  "After start iedit-mode only on current symbol or the active
region, this function expands the search region downwards by N
line.  N defaults to 1.  If N is negative, collapses the bottom
of the search region by `-N' lines."
  (interactive "p")
  (iedit-expand-by-a-line 'bottom N))

(defun iedit-expand-down-to-occurrence (&optional arg)
  "Expand the search region downwards until reaching a new occurrence.
If no such occurrence can be found, throw an error.  With a
prefix, bring the bottom of the region back up one occurrence."
  (interactive "P")
  (if arg
      (progn (iedit-restrict-region
              (iedit-first-occurrence)
              (1- (iedit-last-occurrence)))
             (when iedit-mode
               (goto-char (iedit-last-occurrence))))
  (iedit-expand-to-occurrence t)))

(defun iedit-expand-up-to-occurrence (&optional arg)
  "Expand the search region upwards until reaching a new occurrence.
If no such occurrence can be found, throw an error.  With a
prefix, bring the top of the region back down one occurrence."
  (interactive "P")
  (if arg
      (progn (iedit-restrict-region
              (+ (iedit-occurrence-string-length) (iedit-first-occurrence))
              (+ (iedit-occurrence-string-length) (iedit-last-occurrence)))
             (when iedit-mode
               (goto-char (iedit-first-occurrence))))
    (iedit-expand-to-occurrence nil)))

(defun iedit-expand-to-occurrence (forward)
  "Expand to next or previous occurrence."
  (let ((pos (iedit-add-occurrence-overlay
                (iedit-regexp-quote (iedit-current-occurrence-string))
                (if forward
                    (1+ (iedit-last-occurrence))
                  (iedit-first-occurrence))
                forward)))
    (when pos
      (goto-char pos)
      (force-mode-line-update))))

(defun iedit-restrict-region (beg end &optional exclusive)
  "Restricting Iedit mode in a region."
  (if (null (iedit-find-overlay beg end 'iedit-occurrence-overlay-name exclusive))
      (iedit-done)
    (when iedit-buffering
      (iedit-stop-buffering))
    (setq iedit-last-occurrence-local (iedit-current-occurrence-string))
    (setq mark-active nil)
    (run-hooks 'deactivate-mark-hook)
    (iedit-show-all)
    (iedit-cleanup-occurrences-overlays beg end exclusive)
    (if iedit-hiding
        (iedit-hide-context-lines iedit-occurrence-context-lines))
    (force-mode-line-update)))

(defun iedit-toggle-case-sensitive ()
  "Toggle case-sensitive matching occurrences. "
  (interactive)
  (setq iedit-case-sensitive (not iedit-case-sensitive))
  (if iedit-buffering
      (iedit-stop-buffering))
  (setq iedit-last-occurrence-local (iedit-current-occurrence-string))
  (when iedit-last-occurrence-local
    (remove-overlays nil nil iedit-occurrence-overlay-name t)
    (iedit-show-all)
    (let* ((occurrence-regexp (iedit-regexp-quote iedit-last-occurrence-local))
           (begin (car iedit-initial-region))
           (end (cadr iedit-initial-region))
           (counter (iedit-make-occurrences-overlays occurrence-regexp begin end)))
      (message "iedit %s. %d matches for \"%s\""
               (if iedit-case-sensitive
                   "is case sensitive"
                 "ignores case")
               counter
               (iedit-printable occurrence-regexp))
      (force-mode-line-update))))

(provide 'iedit)

;;; iedit.el ends here