(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq ryk--dark-theme 'zenburn)
(setq ryk--light-theme 'leuven)

(use-package color)

(defun ryk--set-hl-parens-colors (type)
  (global-highlight-parentheses-mode)
  (if (equal 'dark type)
      (progn
        (setq hl-paren-colors '("red1" "green2" "orange1" "DeepSkyBlue1" ))
        (setq hl-paren-background-colors (mapcar (lambda (col) (color-darken-name col 20)) hl-paren-colors)))
    (progn
        (setq hl-paren-colors '("red3" "green4" "orange3" "DeepSkyBlue3" ))
        (setq hl-paren-background-colors (mapcar (lambda (col) (color-lighten-name col 50)) hl-paren-colors))))
  (global-highlight-parentheses-mode))

(defun ryk-set-dark-theme ()
  (interactive)
  (disable-theme ryk--light-theme)
  (load-theme ryk--dark-theme t)
  (ryk--set-hl-parens-colors 'dark))

(defun ryk-set-light-theme ()
  (interactive)
  (disable-theme ryk--dark-theme)
  (load-theme ryk--light-theme t)
  (ryk--set-hl-parens-colors 'light))

(ryk-set-dark-theme)
