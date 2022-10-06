(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq ryk--dark-theme 'spacemacs-dark)
(setq ryk--light-theme 'spacemacs-light)

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

(defun ryk--set-evil-state-colors (normal-color insert-color visual-color operator-color)
    (set-face-background 'powerline-evil-normal-face normal-color)
    (set-face-background 'powerline-evil-insert-face insert-color)
    (set-face-background 'powerline-evil-visual-face visual-color)
    (set-face-background 'powerline-evil-operator-face operator-color))

(defun ryk-set-dark-theme ()
  (interactive)
  (disable-theme ryk--light-theme)
  (load-theme ryk--dark-theme t)
  (ryk--set-hl-parens-colors 'dark)
  (ryk--set-evil-state-colors "#119375" "#4244bb" "#e36463" "#c00078")
  (powerline-reset))

(defun ryk-set-light-theme ()
  (interactive)
  (disable-theme ryk--dark-theme)
  (load-theme ryk--light-theme t)
  (ryk--set-hl-parens-colors 'light)
  (ryk--set-evil-state-colors "#38cc38" "#8496ff" "#efba69" "#ee8888")
  (powerline-reset))

(defun ryk-set-initial-theme ()
  (ryk-set-light-theme))
