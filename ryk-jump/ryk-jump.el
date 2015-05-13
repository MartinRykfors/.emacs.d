(setq left-chars '(?a ?o ?e ?u))
(setq right-chars '(?s ?n ?t ?h))

(defun keys (n)
  `(,(/ n 16) ,(/ (mod n 16) 4) ,(mod n 4)))

(defun index-from (keys)
  (let* ((key-list (string-to-list keys))
         (c1 (first key-list))
         (c2 (nth 1 key-list))
         (c3 (nth 2 key-list))
         (v1 (position c1 right-chars))
         (v2 (position c2 left-chars))
         (v3 (position c3 right-chars)))
    (+ (* 16 v1) (* 4 v2) v3)))

(defun keys-to-string (keys)
  (let ((k1 (first keys))
        (k2 (nth 1 keys))
        (k3 (nth 2 keys)))
    (string (nth k1 right-chars) (nth k2 left-chars) (nth k3 right-chars))))

(setq all-strings (apply 'vector (mapcar (lambda (n) (keys-to-string (keys n))) (number-sequence 0 (- (* 4 4 4) 1)))))
(setq num-strings (length all-strings))
(defvar current-key)
(make-variable-buffer-local 'current-key)

(defun thing ()
  (let* ((a (progn (princ ": ") (read-char)))
         (b (progn (princ ": a" ) (read-char)))
         (c (progn (princ ": ao") (read-char))))
    (string a b c)))


(defface key-leap-inactive
  '((t :inherit (shadow default) :foreground "#606060"))
  "inactive face")

(defface key-leap-active
  '((t :inherit (shadow default) :foreground "#CC0000"))
  "inactive face")

(defun jump-to ()
  (let* ((d (index-from current-key))
         (top (line-number-at-pos (window-start))))
    (goto-line (+ d top))))

(defun color-substring (str)
  (if (string-match (concat "^" current-key) str)
      (propertize str 'face 'key-leap-active)
    (propertize str 'face 'key-leap-inactive)))

(defun key-leap--update-margin-keys (win)
  (remove-overlays (point-min) (point-max) 'window win)
  (set-window-margins win 3)
  (let ((start (line-number-at-pos (window-start win))) (limit (- num-strings 1)))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- start))
      (unless (bolp) (forward-line 1))
      (let ((line (line-number-at-pos)))
        (while (and (not (eobp)) (<= (- line start) limit)
                    (let* ((ol (make-overlay (point) (+ 1 (point))))
                           (str (elt all-strings (- line start)))
                           (colored-string (color-substring str)))
                      (overlay-put ol 'window win)
                      (overlay-put ol 'before-string
                                   (propertize " " 'display`((margin left-margin) ,colored-string)))
                      (setq line (+ 1 line))
                      (zerop (forward-line 1))))))))
  nil)

(defun key-leap--after-change (beg end len)
  (unless (eq (line-number-at-pos beg) (line-number-at-pos end))
    (key-leap--update-current-buffer)))

(defun key-leap--window-scrolled (win beg)
  (key-leap--update-margin-keys win))

(defun key-leap--update-buffer (buffer)
  (with-current-buffer buffer
    (when key-leap-mode
      (dolist (win (get-buffer-window-list buffer nil t))
        (key-leap--update-margin-keys win)))))

(defun key-leap--update-current-buffer ()
  (key-leap--update-buffer (current-buffer)))

(defun key-leap--reset-match-state ()
  (setq current-key "*")
  (key-leap--update-margin-keys (selected-window)))

(defun key-leap--append-char (valid-chars)
  (let ((input-char (read-char)))
    (if (member input-char valid-chars)
        (setq current-key (concat current-key (char-to-string input-char)))
      (progn
        (error "Input char not part of any key")))))

(defun key-leap-start-matching ()
  (interactive)
  (with-local-quit
    (princ " ")
    (setq current-key "")
    (key-leap--update-margin-keys (selected-window))
    (key-leap--append-char right-chars)
    (key-leap--update-margin-keys (selected-window))
    (key-leap--append-char left-chars)
    (key-leap--update-margin-keys (selected-window))
    (key-leap--append-char right-chars)
    (jump-to))
  (key-leap--reset-match-state))

(define-minor-mode key-leap-mode
  "A superb way of leaping between lines"
  :lighter "!!!"
  (if key-leap-mode
      (progn
        (add-hook 'after-change-functions 'key-leap--after-change)
        (add-hook 'window-scroll-functions 'key-leap--window-scrolled)
        (key-leap--update-current-buffer))
    (progn
      (remove-hook 'after-change-functions 'key-leap--after-change)
      (remove-hook 'window-scroll-functions 'key-leap--window-scrolled))))
