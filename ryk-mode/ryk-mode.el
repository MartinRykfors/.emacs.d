;;; ryk-mode.el --- Keybindings for live-coding overtone

;; Copyright (C) 2014 Martin Rykfors

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

(defun ryk-add-synth-parameter (name default-value)
  (interactive "sParameter name: \nnDefault value: ")
  (save-excursion
    (insert name)
    (beginning-of-defun)
    (search-forward "]")
    (backward-char)
    (if (string= "[]" (thing-at-point 'list t))
        (insert name " " (number-to-string default-value))
      (insert " " name " " (number-to-string default-value))))
  (search-forward name))

(defun ryk--get-next-parameter (bound)
  (if (search-forward-regexp "\\([a-zA-Z-]+\\)\\s-+\\([0-9]+\\)" bound t)
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

(defun ryk-insert-synth-call (name)
  (interactive "sSynth name: ")
  (let ((parameters
         (save-excursion
           (goto-char (point-min))
           (search-forward-regexp (concat "(defsynth\\s-+" name))
           (search-forward "[")
           (ryk--get-parameters-at-point))))
    (insert name)
    (let (parameter)
      (while parameters
        (setq parameter (car parameters))
        (insert (concat " :"(car parameter) " " (cdr parameter)))
        (setq parameters (cdr parameters))))))

;;;###autoload
(define-minor-mode ryk-mode
  "Toggle ryk-mode"
  :lighter " ryk!"
  :keymap `((,(kbd "<f7>") . ryk-decrease-fader)
            (,(kbd "<f8>") . ryk-increase-fader)
            (,(kbd "C-S-r") . ryk-add-synth-parameter)))

;;;###autoload
(provide 'ryk-mode)

;; ryk-mode.el ends here
