;;; powerline-evil-local-theme.el 

;;; Commentary:
;;
;; Contains one theme based on the powerline-evil-center-color-theme,
;; with the difference being that it notices evil-local-mode.
;; Original theme Copyright 2014 Chris Johnson
;; URL: http://github.com/raugturi/powerline-evil/
;;

;;; Code:

;;;###autoload
(defun powerline-evil-local-center-color-theme ()
  "Powerline's center-evil theme with the evil state in color."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-size nil 'l)
                                     (powerline-buffer-id nil 'l)
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (powerline-narrow face1 'l)
                                     (powerline-vc face1)))
                          (rhs (list (powerline-raw global-mode-string face1 'r)
                                     (powerline-raw "%4l" face1 'r)
                                     (powerline-raw ":" face1)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1)))
                          (center (append (list (powerline-raw " " face1)
                                                (funcall separator-left face1 face2)
                                                (when (boundp 'erc-modified-channels-object)
                                                  (powerline-raw erc-modified-channels-object face2 'l))
                                                (powerline-major-mode face2 'l)
                                                (powerline-process face2)
                                                (powerline-raw " " face2))
                                          (let ((evil-face (powerline-evil-face)))
                                            (if (split-string (format-mode-line minor-mode-alist))
                                                (append (if evil-local-mode
                                                            (list (funcall separator-right face2 evil-face)
                                                                  (powerline-raw (powerline-evil-tag) evil-face 'l)
                                                                  (powerline-raw " " evil-face)
                                                                  (funcall separator-left evil-face face2)))
                                                        (list (powerline-minor-modes face2 'l)
                                                              (powerline-raw " " face2)
                                                              (funcall separator-right face2 face1)))
                                              (list (powerline-raw (powerline-evil-tag) evil-face)
                                                    (funcall separator-right evil-face face1)))))))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

(provide 'powerline-evil-local-theme)
;;; powerline-evil-local-theme.el ends here
