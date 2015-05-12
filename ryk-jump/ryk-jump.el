(setq left-chars '(?a ?o ?e ?u ))
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

(set-face-foreground 'linum "#404040")

(defun thing ()
  (let* ((a (progn (princ ": ") (read-char)))
         (b (progn (princ ": a" ) (read-char)))
         (c (progn (princ ": ao") (read-char))))
    (string a b c)))

(defun jump-to ()
  (interactive)
  (let* ((keys (thing))
         (d (index-from keys))
         (top (line-number-at-pos (window-start))))
    (goto-line (+ d top))))

(defun aa-stuff (win beg)
  (remove-overlays (point-min) (point-max) 'window win)
  (set-window-margins win 3)
  (let ((start (line-number-at-pos beg)) (limit (- num-strings 1)))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- start))
      (unless (bolp) (forward-line 1))
      (let ((line (line-number-at-pos)))
        (while (and (not (eobp)) (<= (- line start) limit)
                    (let* ((ol (make-overlay (point) (+ 1 (point))))
                           (str (propertize (elt all-strings (- line start)) 'face 'linum)))
                      (overlay-put ol 'ryk t)
                      (overlay-put ol 'window win)
                      (overlay-put ol 'before-string
                                   (propertize " " 'display`((margin left-margin) ,str)))
                      (setq line (+ 1 line))
                      (zerop (forward-line 1))))))))
  nil)

(defun add-stuff ()
  (interactive)
  (aa-stuff (selected-window) (window-start)))

(remove-overlays (point-min) (point-max) 'window (selected-window))
(remove-overlays (point-min) (point-max) 'ryk t)
(selected-window)



(add-hook 'after-change-functions 'ad)
(add-hook 'window-scroll-functions 'aa-stuff)
(remove-hook 'window-scroll-functions 'aa-stuff)

(set-window-margins nil 3)
(window-margins)

(evil-leader/set-key "<SPC>" 'jump-to)











