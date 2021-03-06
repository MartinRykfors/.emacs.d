;;; ryk-mode.el --- Keybindings for live-coding overtone

;; Copyright (C) 2014-2015 Martin Rykfors

;; Author: Martin Rykfors <martinrykfors@gmail.com> @rykarn

;; This file is not part of GNU Emacs.

;; ryk-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ryk-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'dash)

(defun ryk--alter-fader (to-search to-replace)
  (save-excursion
    (beginning-of-line)
    (let ((success (search-forward to-search (point-at-eol) t)))
      (when success (replace-match to-replace))
      success)))

(defun ryk--cider-eval ()
  (if (bound-and-true-p cider-mode)
      (cider-eval-defun-at-point)))

(defun ryk-increase-fader ()
  (interactive)
  (when (ryk--alter-fader "#-" "-#")
    (ryk--cider-eval)))

(defun ryk-decrease-fader ()
  (interactive)
  (when (ryk--alter-fader "-#" "#-")
    (ryk--cider-eval)))

(defun ryk-add-synth-parameter (default-value)
  (interactive "nDefault value: ")
  (let ((name (thing-at-point 'symbol)))
    (save-excursion
      (beginning-of-defun)
      (search-forward "]")
      (backward-char)
      (if (string= "[]" (thing-at-point 'list t))
          (insert name " " (number-to-string default-value))
        (insert " " name " " (number-to-string default-value))))))

(defun ryk--get-next-parameter (bound)
  (if (search-forward-regexp "\\([a-zA-Z-0-9]+\\)\\s-+\\([0-9.]+\\)" bound t)
      (cons (match-string 1) (match-string 2))
    nil))

(defun ryk--get-parameter-list (bound parameters)
  (let ((parameter (ryk--get-next-parameter bound)))
    (if parameter
        (ryk--get-parameter-list bound (cons parameter parameters))
      parameters)))

(defun ryk--get-parameters-at-point ()
  (let ((bound (save-excursion (search-forward "]"))))
    (reverse (ryk--get-parameter-list bound (list)))))

(defun ryk--write-parameters (parameters)
  (let ((sorted (sort parameters (lambda (a b) (string< (car a) (car b))))))
    (let (parameter)
      (while sorted
        (setq parameter (car sorted))
        (insert (concat ":"(car parameter) " " (cdr parameter)))
        (setq sorted (cdr sorted))
        (when sorted (insert " "))))))

(defun ryk--find-synth-parameters (name)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp (concat "(defsynth\\s-+" name) (buffer-end 1) t)
        (progn
          (search-forward "[")
          (ryk--get-parameters-at-point))
      (progn
        (princ (concat "Found no defsynth with name " name))
        nil))))

(defun ryk-insert-synth-call (name)
  (interactive "sSynth name: ")
  (ryk--write-parameters (ryk--find-synth-parameters name)))

(defvar fader-markers nil)
(defface ryk-fader-highlight-face
  '((t :inherit (default) :foreground "#FF0000"))
  "face for highlighing faders in fader-change-state")

(defface ryk-fader-grayout-face
  '((t :inherit (default) :foreground "#808080"))
  "face for highlighing faders in fader-change-state")

(defvar marker-pairs '((?4 . ?7)
                       (?p . ?g) (?u . ?h) (?k . ?m) (?3 . ?8) (?\. . ?c)
                       (?e . ?t) (?j . ?w) (?2 . ?9) (?\, . ?r) (?o . ?n)
                       (?q . ?v) (?1 . ?0) (?\' . ?l) (?a . ?s) (?z . ?\;)
                       (?5 . ?6) (?y . ?f) (?i . ?d) (?x . ?b)))

(defun ryk-mark-fader (down-char)
  (interactive "cFader decrease: ")
  (let ((up-char (cdr (assq down-char marker-pairs))))
    (if (or (assq down-char fader-markers) (assq up-char fader-markers))
        (error "Duplicate fader key"))
    (let ((ol (make-overlay (line-end-position) (line-end-position))))
      (overlay-put ol 'after-string
                   (propertize
                    (string ?\s down-char up-char)
                    'face 'ryk-fader-highlight-face))
      (let ((marker (point-marker)))
        (add-to-list 'fader-markers `(,up-char ,marker up ,down-char ,ol))
        (add-to-list 'fader-markers `(,down-char ,marker down ,up-char ,ol))))))

(defun ryk-unmark-fader (char-to-unmark)
  (interactive "cChar to unmark: ")
  (let* ((val (assq char-to-unmark fader-markers))
         (complement-char (nth 3 val))
         (complement-val (assq complement-char fader-markers))
         (ol (nth 4 val)))
    (setq fader-markers (delq val fader-markers))
    (setq fader-markers (delq complement-val fader-markers))
    (delete-overlay ol)
    val))

(defun ryk--change-marked-fader (key-char)
  (let* ((val (assq key-char fader-markers))
         (marker (nth 1 val))
         (direction (nth 2 val)))
    (save-excursion
      (when marker
        (goto-char marker)
        (if (equal 'up direction)
            (ryk-increase-fader)
          (ryk-decrease-fader))))))

(defun ryk--one-interval (start-number numbers)
  (when numbers
    (if (= start-number (car numbers))
        (ryk--one-interval (1+ start-number) (cdr numbers))
      (cons (cons start-number (car numbers)) (ryk--one-interval (1+ (car numbers)) (cdr numbers))))))

(defun ryk--intervals-of (numbers)
  (ryk--one-interval 1 numbers))

(defun ryk--marker-line-starts ()
  (--> fader-markers
       (mapcar (lambda (l) (line-number-at-pos (nth 1 l))) it)
       (cons (1+ (line-number-at-pos (point-max))) it)
       (sort it '<)
       (delete-dups it)))

(defun ryk--line-start (N)
  (goto-char (point-min))
  (forward-line (1- N))
  (point))

(defun ryk--hide-non-fader-lines ()
  (let* ((starts (ryk--marker-line-starts))
         (intervals (ryk--intervals-of starts))
         (overlays nil))
    (dolist (interval intervals)
      (let* ((b (ryk--line-start (car interval)))
             (e (ryk--line-start (cdr interval)))
             (ol (make-overlay b e)))
        (overlay-put ol 'invisible t)
        (push ol overlays)))
    overlays))

(defun ryk--place-fader-overlays ()
  (let ((ols nil))
    (save-excursion
      (dolist (row fader-markers)
        (let ((marker (nth 1 row)))
          (goto-char marker)
          (let ((ol (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put ol 'face 'ryk-fader-highlight-face)
            (push ol ols)))))
    ols))

(defun ryk-clear-fader-markers ()
  (interactive)
  (dolist (row fader-markers)
    (let ((ol (nth 4 row)))
      (delete-overlay ol)))
  (setq fader-markers nil))

(defun ryk--is-fader-line-p (pos)
  (save-excursion
    (goto-char pos)
    (let ((eol (progn (end-of-line) (point))))
      (beginning-of-line)
      (search-forward-regexp "\"-*#-*\"" eol t))))

(defun ryk--auto-mark-faders (beg end)
  (ryk-clear-fader-markers)
  (let* ((available-keys (copy-tree marker-pairs))
        (current-pair (pop available-keys)))
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end)
                  (not (eobp)))
        (when (ryk--is-fader-line-p (point))
          (ryk-mark-fader (car current-pair))
          (setq current-pair (pop available-keys)))
        (forward-line)))))

(defun ryk-highlighted-fader-change-state ()
  (interactive)
  (ryk--auto-mark-faders (window-start) (window-end))
  (save-excursion
    (let ((inhibit-quit t)
          (cursor-type 'hollow)
          (fader-overlays (ryk--place-fader-overlays))
          (gray-overlay (let ((o (make-overlay (window-start) (window-end))))
                          (overlay-put o 'face 'ryk-fader-grayout-face)
                          o)))
      (unwind-protect
          (with-local-quit
            (while t
              (ryk--change-marked-fader (read-char " "))))
        (progn
          (delete-overlay gray-overlay)
          (dolist (overlay fader-overlays)
            (delete-overlay overlay))
          (ryk-clear-fader-markers))))))

(defun ryk-folded-fader-change-state ()
  (interactive)
  (ryk--auto-mark-faders (point-min) (point-max))
  (save-excursion
    (let ((inhibit-quit t)
          (cursor-type 'hollow)
          (invisible-overlays (ryk--hide-non-fader-lines)))
      (goto-char (point-min))
      (unwind-protect
          (with-local-quit
            (while t
              (ryk--change-marked-fader (read-char " "))))
        (progn
          (dolist (hide-overlay invisible-overlays)
            (delete-overlay hide-overlay))
          (ryk-clear-fader-markers))))))

;; "----------#--------" 1
;; "----#--------------" 2
;; "--------#----------" 3
;; "--------#----------" 4
;; "---------#---------" 5
;; "------------#------" 6
;; "--------#----------" 7
;; "---------#---------" 8

;;;###autoload
(define-minor-mode ryk-mode
  "Toggle ryk-mode"
  :lighter " ryk!"
  :keymap `((,(kbd "<f7>") . ryk-decrease-fader)
            (,(kbd "<f8>") . ryk-increase-fader)
            (,(kbd "C-S-r") . ryk-add-synth-parameter)
            (,(kbd "C-S-l") . ryk-insert-synth-call)
            (,(kbd "C-S-f") . ryk-mark-fader)
            (,(kbd "C-~") . ryk-folded-fader-change-state)
            (,(kbd "C-`") . ryk-highlighted-fader-change-state)))

;;;###autoload
(provide 'ryk-mode)

;; ryk-mode.el ends here
