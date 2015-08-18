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

(defun ryk-mark-fader (up-char down-char)
  (interactive "cFader increase: \ncFader decrease: ")
  (message (string up-char down-char))
  (let ((ol (make-overlay (line-end-position) (line-end-position))))
    (overlay-put ol 'after-string (string ?\s down-char up-char)))
  (let ((marker (point-marker)))
    (add-to-list 'fader-markers `(,up-char ,marker up))
    (add-to-list 'fader-markers `(,down-char ,marker down))))

(defun ryk-change-marked-fader (key-char)
  (interactive "cKey: ")
  (let* ((val (assoc key-char fader-markers))
         (marker (nth 1 val))
         (direction (nth 2 val)))
    (save-excursion
      (goto-char marker)
      (if (equal 'up direction)
          (ryk-increase-fader)
        (ryk-decrease-fader)))))

;; "------#---"
;; "--#-------"
;; "--#-------"

;;;###autoload
(define-minor-mode ryk-mode
  "Toggle ryk-mode"
  :lighter " ryk!"
  :keymap `((,(kbd "<f7>") . ryk-decrease-fader)
            (,(kbd "<f8>") . ryk-increase-fader)
            (,(kbd "C-S-r") . ryk-add-synth-parameter)
            (,(kbd "C-S-l") . ryk-insert-synth-call)))

;;;###autoload
(provide 'ryk-mode)

;; ryk-mode.el ends here
