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

(defun ryk-increase-fader ()
  (interactive)
  (beginning-of-line)
  (search-forward "\"" (point-at-eol) t)
  (replace-match "\"#")
  (if (bound-and-true-p cider-mode)
      (cider-eval-defun-at-point)))

(defun ryk-decrease-fader ()
  (interactive)
  (beginning-of-line)
  (search-forward "\"#" (point-at-eol) t)
  (replace-match "\"")
  (if (bound-and-true-p cider-mode)
      (cider-eval-defun-at-point)))

;;;###autoload
(define-minor-mode ryk-mode
  "Toggle ryk-mode"
  :lighter " ryk!"
  :keymap `((,(kbd "<f7>") . ryk-decrease-fader)
            (,(kbd "<f8>") . ryk-increase-fader)))

;;;###autoload
(provide 'ryk-mode)

;; ryk-mode.el ends here
