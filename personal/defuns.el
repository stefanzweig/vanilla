;; For loading personal configurations
(defun personal (library)
  (load (concat vanilla-personal-dir "/" (symbol-name library)) 'noerror))