;;; key-leap-mode.el

;; Copyright (C) 2015  Martin Rykfors

;; Author: Martin Rykfors <martinrykfors@gmail.com> @rykarn
;; Version: 0.1.0-ALPHA
;; Package-Requires: ((emacs "24.1"))
;; Keywords: point, location

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; key-leap-mode allows you to quickly jump to any visible line in a
;; window. When key-leap-mode is enabled, it will populate the margin
;; of every line with an unique three-letter keyword. By calling the
;; interactive command `key-leap-start-matching' the keywords become
;; active. Typing the keyword of a line in this state will move the
;; point to the beginning of that line.

;; key-leap-mode will generate the keywords based on the characters
;; you provide through the function `key-leap-set-key-chars'.
;; This function takes three lists of chars as arguments. Each list
;; should specify the possible first, second and third chars to use
;; in the keywords, respectively.
;; By default, key-leap-mode will generate keywords from the home-row
;; of a qwerty keyboard layout, in a right-left-right fashion.

(setq key-leap--first-chars '(?h ?j ?k ?l ?\;))
(setq key-leap--second-chars '(?g ?f ?d ?s ?a))
(setq key-leap--third-chars '(?h ?j ?k ?l ?\;))
(defcustom key-leap-upcase-active t
  "If set to t, key-leap-mode will make active characters of the keys upper-cased when waiting for the key input.")
(setq key-leap--first-count (length key-leap--first-chars))
(setq key-leap--second-count (length key-leap--second-chars))
(setq key-leap--third-count (length key-leap--third-chars))
(defvar key-leap-after-leap-hook nil
  "Hook that runs after key-leap-mode has jumped to a new line.")

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

(defvar current-key "*")
(make-variable-buffer-local 'current-key)

(defface key-leap-inactive
  '((t :inherit (shadow default) :foreground "#606060"))
  "Face to use for the inactive parts of the keys.")

(defface key-leap-active
  '((t :inherit (shadow default) :foreground "#FF0000"))
  "Face to use for the parts of the keys that are still being matched.")

(defun key-leap--leap-to-current-key ()
  (let* ((d (index-from current-key))
         (top (line-number-at-pos (window-start))))
    (goto-line (+ d top))
    (run-hooks 'key-leap-after-leap-hook)))

(defun color-substring (str)
  (if (string-match (concat "\\(^" current-key "\\)\\(.*\\)") str)
       (concat
          (propertize (match-string 1 str) 'face 'key-leap-inactive)
          (let* ((active-str (match-string 2 str))
                 (cased-str (if key-leap-upcase-active (upcase active-str) active-str)))
            (propertize cased-str 'face 'key-leap-active)))
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
  "When called, will wait for the user to type the three characters of a key in the margin, and then jump to the corresponding line."
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

;;; key-leap-mode.el ends here
