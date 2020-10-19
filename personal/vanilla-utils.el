(defun jump-forward (phrase)
  "Move forward looking for PHRASE.  If not found, jump backward to it."
  (if (not (search-forward phrase nil t))
      (search-backward phrase nil t)))

(defun jump-backward (phrase)
  "Move backward looking for PHRASE.  If not found, jump foreward to it."
  (if (not (search-backward phrase nil t))
      (search-forward phrase nil t)))


(defun insert-typewriter (str)
  "Insert STR into the current buffer as if you were typing it by hand."
  (interactive "s")
  ;; (insert str)
  (dolist (ch (string-to-list str))
    (insert ch)
    (sit-for (/ 1.0 (+ 10 (random 100))) nil)))
