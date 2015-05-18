;;configure environment
(setq default-directory "~/")
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(setq menu-bar-mode nil)
(scroll-bar-mode -1)
(setq echo-keystrokes 0.01)
(global-set-key (kbd "<C-tab>") 'other-window)
;(setq scroll-margin 4)
;(setq scroll-conservatively 1)
(setq show-paren-delay 0)
(show-paren-mode)
(defun sett ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(electric-pair-mode)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4
      c-set-style "linux")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;;unbind set-fill-column because I have never called it except by mistake when trying to do C-x C-f
(global-unset-key (kbd "C-x f"))
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key [escape] 'keyboard-escape-quit)
(when (eq system-type 'darwin)
  (load "~/.emacs.d/ryk-typewriter.el"))
(load "~/.emacs.d/stars.el")
(setq initial-buffer-choice (lambda ()
                              (let ((new-buffer (get-buffer-create "*scratch*")))
                                (with-current-buffer new-buffer
                                  (funcall 'lisp-interaction-mode)
                                  (funcall 'evil-local-mode)
                                  (dotimes (_ 20)
                                    (insert-stars)))
                                new-buffer)))
(add-hook 'after-init-hook (lambda () (load-file "~/.emacs.d/color-setup.el")))

(when (eq system-type 'darwin)
  (set-default-font "Source Code Pro"))

(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

(defun ryk-frame-alpha (value)
  (interactive "nalpha-value (0 - 10): ")
  (set-frame-parameter (selected-frame) 'alpha (* value 10)))

;;set up all packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(dolist (package '(use-package diminish))
  (unless (package-installed-p package)
    (package-install package)))

(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

(use-package flx-ido
  :ensure t
  :config
  (progn
    (ido-mode 1)
    (setq ido-everywhere 1)
    (flx-ido-mode 1)
    (setq ido-enable-flex-matching 1)
    (setq ido-separator "  ")
    (setq ido-use-faces nil)))

(use-package exec-path-from-shell
  :ensure t
  :if (not (eq system-type 'windows-nt))
  :init (exec-path-from-shell-initialize))

(use-package yasnippet
  :ensure t
  :init
  (progn
    (yas-global-mode 1)
    (diminish 'yas-minor-mode)))

(use-package powerline
  :ensure t)

(use-package evil
  :ensure t
  :config
  (progn 
    (global-set-key (kbd "<f6>") 'evil-local-mode) 
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "C-S-d") 'evil-scroll-up)
    (define-key evil-normal-state-map (kbd "<SPC>") #'key-leap-start-matching)
    (setq evil-search-module 'evil-search)
    (add-hook 'find-file-hook 'evil-local-mode)))

(use-package evil-surround
  :ensure t
  :init
  (push '(evil-surround-mode " [ψ]") minor-mode-alist))

(use-package evil-numbers
  :ensure t
  :init
  (progn
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-;") 'evil-numbers/dec-at-pt)))

(defun ryk-to-enclosing-paren (n)
  (paredit-forward-up n)
  (when (< 0 n)
      (backward-char)))

(use-package paredit
  :diminish "()"
  :ensure t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (define-key paredit-mode-map (kbd "M-1") (lambda () (interactive) (ryk-to-enclosing-paren 1)))
    (define-key paredit-mode-map (kbd "M-2") (lambda () (interactive) (ryk-to-enclosing-paren 2)))
    (define-key paredit-mode-map (kbd "M-3") (lambda () (interactive) (ryk-to-enclosing-paren 3)))
    (define-key paredit-mode-map (kbd "M-4") (lambda () (interactive) (ryk-to-enclosing-paren 4)))
    (define-key paredit-mode-map (kbd "C-M-1") (lambda () (interactive) (ryk-to-enclosing-paren -1)))
    (define-key paredit-mode-map (kbd "C-M-2") (lambda () (interactive) (ryk-to-enclosing-paren -2)))
    (define-key paredit-mode-map (kbd "C-M-3") (lambda () (interactive) (ryk-to-enclosing-paren -3)))
    (define-key paredit-mode-map (kbd "C-M-4") (lambda () (interactive) (ryk-to-enclosing-paren -4)))))

(use-package evil-paredit
  :ensure t
  :init
  (progn
    (add-hook 'paredit-mode-hook 'evil-paredit-mode)
    (setq minor-mode-alist (append minor-mode-alist '((evil-paredit-mode " ()ψ"))))))

(use-package powerline-evil
  :ensure t
  :config
  (progn
    (add-to-list 'load-path "~/.emacs.d/powerline-evil-themes")
    (require 'powerline-evil-local-theme)
    (powerline-evil-local-center-color-theme)
    (setq powerline-default-separator (quote nil)) 
    (set-face-background 'powerline-evil-normal-face "SpringGreen3")
    (set-face-background 'powerline-evil-insert-face "blue3")
    (set-face-background 'powerline-evil-visual-face "goldenrod")
    (set-face-background 'powerline-evil-operator-face "red1")))

(use-package evil-exchange
  :ensure t
  :init
  (progn
    (evil-exchange-install)
    (defface ryk-exchange
      '((t (:inherit 'default :foreground "red1")))
      "Face for evil-exchange highlight")
    (setq evil-exchange-highlight-face 'ryk-exchange)))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (progn
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
    (global-highlight-parentheses-mode)
    ;;make paren highlight update after stuff like paredit changes
    (add-to-list 'after-change-functions '(lambda (&rest x) (hl-paren-highlight)))))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (progn
    (define-key undo-tree-visualizer-mode-map (kbd "j") 'undo-tree-visualize-redo)
    (define-key undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualize-undo)
    (define-key undo-tree-visualizer-mode-map (kbd "h") 'undo-tree-visualize-switch-branch-left)
    (define-key undo-tree-visualizer-mode-map (kbd "l") 'undo-tree-visualize-switch-branch-right)))

(use-package company
  :ensure t
  :diminish " ☭"
  :config
  (progn
    (unbind-key "[return]" company-active-map)
    (unbind-key (kbd "RET") company-active-map)
    (unbind-key "[tab]" company-active-map)
    (unbind-key (kbd "TAB") company-active-map)
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map [return] 'company-complete-selection)
    (define-key company-active-map (kbd "RET") 'company-complete-selection)
    (define-key company-active-map (kbd "M-j") 'company-select-next)
    (define-key company-active-map (kbd "M-k") 'company-select-previous)
    (define-key evil-insert-state-map (kbd "M-u") 'company-select-previous)
    (global-set-key (kbd "M-u") 'company-complete)
    (setq company-require-match nil)
    (global-company-mode)))

(use-package magit
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-S-m") 'magit-status)
    (diminish 'magit-auto-revert-mode)))

(use-package erc
  :config
  (set-face-attribute 'erc-notice-face nil :height 100))

(use-package paren)

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

(use-package ryk-mode
  :load-path "~/.emacs.d/ryk-mode"
  :init
  (add-hook 'cider-mode-hook 'ryk-mode))

(use-package smex
  :init
  (progn
    (smex-initialize)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

(use-package cider-eval-sexp-fu
  :if (eq system-type 'darwin)
  :config
  (progn
    (set-face-foreground 'eval-sexp-fu-flash "green1")))

(use-package key-leap-mode
  :load-path "~/.emacs.d/key-leap-mode"
  :config
  (progn
    (global-set-key (kbd "<f5>") 'key-leap-mode)
    (if (eq system-type 'darwin)
        (key-leap-set-key-chars '(?h ?g ?t ?c)
                                '(?a ?o ?e ?u)
                                '(?h ?t ?n ?s))
      (key-leap-set-key-chars '(?h ?g ?t ?c ?n ?s)
                              '(?a ?o ?e ?u)
                              '(?h ?t ?n ?s)))))

(use-package jammer
  :ensure t
  :init
  (progn
    (setq jammer-block-type 'blacklist)
    (setq jammer-block-list (list 'next-line 'previous-line 'evil-next-line 'evil-next-visual-line))
    (setq jammer-type 'repeat)
    (setq jammer-repeat-allowed-repetitions 4)
    (setq jammer-repeat-type 'constant)
    (setq jammer-repeat-delay 0.1)
    (jammer-mode)))

;; can only do this after initializing powerline and powerline-evil
(toggle-frame-maximized)
