(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/settings.el")))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'brin t)

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))


(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

(package-initialize)

(dolist (package '(use-package diminish))
  (unless (package-installed-p package)
    (package-install package)))

(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

(use-package exec-path-from-shell
  :ensure t
  :if (not (eq system-type 'windows-nt))
  :init (exec-path-from-shell-initialize))

(use-package powerline
  :ensure t
  :init
  (progn
    (set-face-background 'powerline-active1 (color-lighten-name (face-background 'mode-line) 10))
    (set-face-background 'powerline-active2 (color-lighten-name (face-background 'mode-line) 20))
    (set-face-foreground 'powerline-active1 (color-lighten-name (face-foreground 'mode-line) 10))
    (set-face-foreground 'powerline-active2 (color-lighten-name (face-foreground 'mode-line) 10))
    (set-face-background 'powerline-inactive1 (color-darken-name (face-background 'mode-line) 10))
    (set-face-background 'powerline-inactive2 (color-darken-name (face-background 'mode-line) 10))
    (set-face-foreground 'powerline-inactive1 (color-darken-name (face-foreground 'mode-line) 20))
    (set-face-foreground 'powerline-inactive2 (color-darken-name (face-foreground 'mode-line) 20))))

(use-package evil
  :ensure t
  :init
  (progn 
    (global-set-key (kbd "<f6>") 'evil-local-mode) 
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (setq evil-search-module 'evil-search)
    (add-hook 'find-file-hook 'evil-local-mode)))

(use-package evil-surround
  :ensure t)

(use-package evil-numbers
  :ensure t
  :init
  (progn
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-;") 'evil-numbers/dec-at-pt)))

(use-package paredit
  :diminish "PrEd"
  :ensure t)

(use-package powerline-evil
  :ensure t
  :init
  (progn
    (add-to-list 'load-path "~/.emacs.d/powerline-evil-themes")
    (require 'powerline-evil-local-theme)
    (powerline-evil-local-center-color-theme)
    (setq powerline-default-separator (quote nil)) 
    (set-face-background 'powerline-evil-normal-face "SpringGreen3")
    (set-face-background 'powerline-evil-insert-face "blue3")
    (set-face-background 'powerline-evil-visual-face "goldenrod")
    (set-face-background 'powerline-evil-operator-face "red1")))

(use-package highlight-parentheses
  :ensure t
  :init
  (progn
    (setq hl-paren-colors '("red1" "green2" "orange1" "DeepSkyBlue1" ))
    (setq hl-paren-background-colors (mapcar (lambda (col) (color-darken-name col 20)) hl-paren-colors))
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
    (global-highlight-parentheses-mode)
    ;;make paren highlight update after stuff like paredit changes
    (add-to-list 'after-change-functions '(lambda (&rest x) (hl-paren-highlight)))))

(use-package undo-tree
  :ensure t
  :diminish "UnTr"
  :init
  (progn
    (define-key undo-tree-visualizer-mode-map (kbd "j") 'undo-tree-visualize-redo)
    (define-key undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualize-undo)
    (define-key undo-tree-visualizer-mode-map (kbd "h") 'undo-tree-visualize-switch-branch-left)
    (define-key undo-tree-visualizer-mode-map (kbd "l") 'undo-tree-visualize-switch-branch-right)))

(use-package company
  :ensure t
  :diminish "com"
  :init
  (progn
    (define-key company-active-map (kbd "M-e") 'company-select-next)
    (define-key company-active-map (kbd "M-u") 'company-select-previous)
    (define-key company-active-map [tab] nil)
    (define-key company-active-map [return] nil)
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map [return] 'company-complete-common)
    (define-key evil-insert-state-map (kbd "M-p") 'company-select-previous)
    (set-face-background 'company-tooltip (face-attribute 'default :background))
    (set-face-foreground 'company-tooltip (face-attribute 'font-lock-constant-face :foreground))
    (set-face-foreground 'company-tooltip-selection (face-attribute 'default :background))
    (set-face-background 'company-tooltip-selection (face-attribute 'font-lock-constant-face :foreground))
    (set-face-background 'company-scrollbar-bg (face-attribute 'default :background))
    (set-face-background 'company-scrollbar-fg (face-attribute 'font-lock-variable-name-face :foreground))
    (set-face-foreground 'company-tooltip-common (face-attribute 'font-lock-variable-name-face :foreground))
    (set-face-foreground 'company-tooltip-common-selection (face-attribute 'default :background))
    (global-company-mode)))

(use-package magit
  :ensure t
  :init
  (global-set-key (kbd "C-S-m") 'magit-status))

(use-package erc
  :init
  (set-face-attribute 'erc-notice-face nil :height 100))

(use-package paren
  :init
  (progn
    (set-face-background 'show-paren-match (color-lighten-name (face-background 'default) 20))
    (set-face-foreground 'show-paren-match (face-foreground 'default))
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
    (set-face-foreground 'isearch "green1")
    (set-face-background 'isearch (color-darken-name (face-foreground 'isearch) 25))
    (set-face-foreground 'lazy-highlight "green1")
    (set-face-background 'lazy-highlight (color-darken-name (face-foreground 'lazy-highlight) 35))))

(use-package clojure-mode
  :ensure t
  :init
  (progn
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'highlight-parentheses-mode)))

(use-package cider
  :ensure t)

(use-package processing-mode
  :ensure t
  :mode ("\\.pde$" . processing-mode)
  :init
  (progn
    (setq processing-location "/usr/bin/processing-java")
    (setq processing-application-dir "/Applications/Processing.app")
    (setq processing-sketchbook-dir "/Users/ryk/code/processing")))

(use-package glsl-mode
  :ensure t
  :init
  (append auto-mode-alist '('("\\.glsl\\'" . glsl-mode)
                            '("\\.vert\\'" . glsl-mode)
                            '("\\.frag\\'" . glsl-mode)
                            '("\\.geom\\'" . glsl-mode))))
