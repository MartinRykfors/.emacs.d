(setq key-leap--first-chars '(?s ?n ?t ?h))
(setq key-leap--second-chars '(?a ?o ?e ?u))
(setq key-leap--third-chars '(?s ?n ?t ?h ?d))
(setq key-leap-soft-bol t)
(setq key-leap--first-count (length key-leap--first-chars))
(setq key-leap--second-count (length key-leap--second-chars))
(setq key-leap--third-count (length key-leap--third-chars))

(defun keys (n)
  `(,(/ n (* key-leap--second-count key-leap--third-count))
    ,(/ (mod n (* key-leap--second-count key-leap--third-count)) key-leap--third-count)
    ,(mod n key-leap--third-count)))

(defun index-from (keys)
  (let* ((key-list (string-to-list keys))
         (c1 (first key-list))
         (c2 (nth 1 key-list))
         (c3 (nth 2 key-list))
         (v1 (position c1 key-leap--first-chars))
         (v2 (position c2 key-leap--second-chars))
         (v3 (position c3 key-leap--third-chars)))
    (+ (* (* key-leap--second-count key-leap--third-count) v1) (* key-leap--third-count v2) v3)))

(defun keys-to-string (keys)
  (let ((k1 (first keys))
        (k2 (nth 1 keys))
        (k3 (nth 2 keys)))
    (string (nth k1 key-leap--first-chars) (nth k2 key-leap--second-chars) (nth k3 key-leap--third-chars))))

(setq all-strings)
(setq num-strings)

(defun key-leap--cache-keys ()
  (setq all-strings (apply 'vector (mapcar (lambda (n)
                                             (keys-to-string (keys n)))
                                           (number-sequence 0 (- (* key-leap--first-count key-leap--second-count key-leap--third-count) 1)))))
  (setq num-strings (length all-strings)))

(key-leap--cache-keys)

(defun key-leap-set-key-chars (first-chars second-chars third-chars)
  (setq key-leap--first-chars first-chars)
  (setq key-leap--second-chars second-chars)
  (setq key-leap--third-chars third-chars)
  (setq key-leap--first-count (length key-leap--first-chars))
  (setq key-leap--second-count (length key-leap--second-chars))
  (setq key-leap--third-count (length key-leap--third-chars))
  (key-leap--cache-keys))

(key-leap-set-key-chars '(?h ?g ?t ?c ?n ?s)
                        '(?a ?o ?e ?u)
                        '(?h ?t ?n ?s))

(defvar current-key "*")
(make-variable-buffer-local 'current-key)

(defface key-leap-inactive
  '((t :inherit (shadow default) :foreground "#606060"))
  "inactive face")

(defface key-leap-active
  '((t :inherit (shadow default) :foreground "#FF0000"))
  "inactive face")

(defun key-leap--leap-to-current-key ()
  (let* ((d (index-from current-key))
         (top (line-number-at-pos (window-start))))
    (goto-line (+ d top))
    (when key-leap-soft-bol
      (back-to-indentation))))

(defun color-substring (str)
  (if (string-match (concat "\\(^" current-key "\\)\\(.*\\)") str)
       (concat
          (propertize (match-string 1 str) 'face 'key-leap-inactive)
          (upcase (propertize (match-string 2 str) 'face 'key-leap-active)))
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
        (while (and (not (eobp)) (<= (- line start) limit))
          (let* ((ol (make-overlay (point) (+ 1 (point))))
                 (str (elt all-strings (- line start)))
                 (colored-string (color-substring str)))
            (overlay-put ol 'window win)
            (overlay-put ol 'before-string
                         (propertize " " 'display`((margin left-margin) ,colored-string)))
            (setq line (+ 1 line))
            (forward-line 1)))))))

(defun key-leap--after-change (beg end len)
  (unless (eq (line-number-at-pos beg) (line-number-at-pos end))
    (key-leap--update-current-buffer)))

(defun key-leap--window-scrolled (win beg)
  (with-current-buffer (window-buffer)
    (when key-leap-mode
      (key-leap--update-margin-keys win))))

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
        (key-leap--reset-match-state)
        (error "Input char not part of any key")))))

(defun key-leap-start-matching ()
  (interactive)
  (let ((inhibit-quit t))
    (if key-leap-mode
        (progn
          (unless
              (with-local-quit
                (princ " ")
                (setq current-key "")
                (key-leap--update-margin-keys (selected-window))
                (key-leap--append-char key-leap--first-chars)
                (key-leap--update-margin-keys (selected-window))
                (key-leap--append-char key-leap--second-chars)
                (key-leap--update-margin-keys (selected-window))
                (key-leap--append-char key-leap--third-chars)
                (key-leap--leap-to-current-key))
            (key-leap--reset-match-state))
          (key-leap--reset-match-state))
      (error "key-leap-mode not enabled in this buffer"))))

(defun key-leap--clean-buffer (buffer)
  (with-current-buffer buffer
    (dolist (win (get-buffer-window-list buffer nil t))
      (remove-overlays (point-min) (point-max) 'window win)
      (set-window-margins win 0))))

;;;###autoload
(define-minor-mode key-leap-mode
  "A superb way of leaping between lines"
  :lighter nil
  (if key-leap-mode
      (progn
        (add-hook 'after-change-functions 'key-leap--after-change)
        (add-hook 'window-scroll-functions 'key-leap--window-scrolled)
        (key-leap--update-current-buffer))
    (progn
      (remove-hook 'after-change-functions 'key-leap--after-change)
      (remove-hook 'window-scroll-functions 'key-leap--window-scrolled)
      (key-leap--clean-buffer (current-buffer)))))

;;;###autoload
(provide 'key-leap-mode)
