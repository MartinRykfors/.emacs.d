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
;; (global-set-key [escape] 'keyboard-escape-quit)
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
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)
(global-set-key (kbd "M-`") 'other-frame)

(if (eq system-type 'darwin)
    (set-default-font "Source Code Pro")
  (progn
    (set-default-font "Consolas")
    (set-face-attribute 'default nil :height 105)))

(require 'cl)
(toggle-frame-maximized)
(when (eq system-type 'darwin)
    (set-frame-position (first (frame-list)) 0 0))

(defun ryk-set-font-height (height)
  (interactive "nFont height: ")
  (set-face-attribute 'default nil :height height)
  (toggle-frame-maximized))

(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

(defun ryk-frame-alpha (value)
  (interactive "nalpha-value (0 - 10): ")
  (set-frame-parameter (selected-frame) 'alpha (* value 10)))

;; I keep on hitting C-x C-b when I mean C-x b
;; Unless I use C-u, interpret both as me meaning C-x b
(defun ryk-switch-buffer (arg)
  (interactive "P")
  (if arg
      (list-buffers)
    (ido-switch-buffer)))
(global-set-key (kbd "C-x b") 'ryk-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ryk-switch-buffer)

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

(use-package spacemacs-theme
  :ensure t
  :init
  (setq spacemacs-theme-comment-bg nil))

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

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous))

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

;; (use-package powerline
;;   :ensure t)

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
    (define-key evil-insert-state-map (kbd "M-'") (lambda () (interactive) (insert ?å)))
    (define-key evil-insert-state-map (kbd "M-,") (lambda () (interactive) (insert ?ä)))
    (define-key evil-insert-state-map (kbd "M-.") (lambda () (interactive) (insert ?ö)))
    (define-key evil-insert-state-map (kbd "M-\"") (lambda () (interactive) (insert ?Å)))
    (define-key evil-insert-state-map (kbd "M-<") (lambda () (interactive) (insert ?Ä)))
    (define-key evil-insert-state-map (kbd "M->") (lambda () (interactive) (insert ?Ö)))
    (setq evil-search-module 'evil-search)
    (add-hook 'find-file-hook 'evil-local-mode)))

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
    (add-hook 'paredit-mode-hook (lambda ()
                                   (if paredit-mode
                                       (evil-paredit-mode 1)
                                     (evil-paredit-mode 0))))
    (setq minor-mode-alist (append minor-mode-alist '((evil-paredit-mode " ()ψ"))))))

(use-package evil-surround
  :ensure t
  :init
  (progn
    (push '(evil-surround-mode " [ψ]") minor-mode-alist)
    (add-hook 'paredit-mode-hook (lambda ()
                                   (if paredit-mode
                                       (evil-surround-mode 0)
                                     (evil-surround-mode 1))))
    (global-evil-surround-mode)))

(use-package powerline-evil
  :ensure t)

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
    (global-set-key (kbd "C-S-m") 'magit-status)))

(use-package paren)

(use-package clojure-mode
  :ensure t
  :init
  (progn
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'highlight-parentheses-mode)))

(use-package cider
  :ensure t
  :init
  (progn
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-auto-jump-to-error nil)))

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

(use-package key-leap
  :load-path "~/.emacs.d/key-leap"
  :config
  (progn
    (global-set-key (kbd "<f5>") 'key-leap-mode)
    (if (eq system-type 'darwin)
        (setq key-leap-key-strings '("htnsgcrlmdw" "aoeui"))
      (setq key-leap-key-strings '("htnsdmgcrlwvb" "aoeuiy")))
    (add-hook 'key-leap-after-leap-hook 'back-to-indentation)))

(use-package projectile
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-mode-line '(:eval (format " Pt[%s]" (projectile-project-name))))))

(use-package org
  :bind* ("<C-tab>" . other-window)
  :config
  (progn
    (setq org-default-notes-file "~/org/notes.org")))

;; spacemacs mode-line copypaste
(use-package powerline
  :ensure t
  :init
  (progn
    (defpowerline spacemacs-powerline-minor-modes
      (mapconcat (lambda (mm)
                   (propertize
                    mm
                    'mouse-face 'mode-line-highlight
                    'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map
                                   [mode-line down-mouse-1]
                                   (powerline-mouse 'minor 'menu mm))
                                 (define-key map
                                   [mode-line mouse-2]
                                   (powerline-mouse 'minor 'help mm))
                                 (define-key map
                                   [mode-line down-mouse-3]
                                   (powerline-mouse 'minor 'menu mm))
                                 (define-key map
                                   [header-line down-mouse-3]
                                   (powerline-mouse 'minor 'menu mm))
                                 map)))
                 (split-string (format-mode-line minor-mode-alist))
                 " "))

    (if (display-graphic-p)
        (setq-default powerline-default-separator 'curve)
      (setq-default powerline-default-separator 'utf-8))

    (defun spacemacs/mode-line-prepare-left ()
      (let* ((active (powerline-selected-window-active))
             (line-face (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (state-face face2)
             (vc-face face1)
             (separator-left (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (cdr powerline-default-separator-dir)))))

        (append
         ;; evil state
         (when evil-local-mode
           (list
            (powerline-raw evil-mode-line-tag (powerline-evil-face))
            (funcall separator-right (powerline-evil-face) line-face)))
         ;; buffer name
         (list
          (powerline-raw "%*" line-face 'l)
          (powerline-buffer-size line-face 'l)
          (powerline-buffer-id line-face 'l)
          (powerline-raw " " line-face)
          ;; major mode
          (funcall separator-left line-face face1)
          (powerline-major-mode face1 'l)
          (powerline-raw " " face1)
          (when active
            (funcall separator-right face1 line-face)))
         ;; minor modes
         (when active
           (list (spacemacs-powerline-minor-modes line-face 'l)
                 (powerline-raw mode-line-process line-face 'l)
                 (powerline-raw " " line-face)))
         ;; version control
         (when active
           (list (funcall separator-left (if vc-face line-face face1) vc-face)))
         (if active
             (list (powerline-vc vc-face)
                   (powerline-raw " " vc-face)
                   (funcall separator-right vc-face face2))
           (list (funcall separator-right face1 face2))))))

    (defun spacemacs/mode-line-prepare-right ()
      (let* ((active (powerline-selected-window-active))
             (line-face (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (state-face face2)
             (separator-left (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (cdr powerline-default-separator-dir)))))
        (append
         (list
          ;; row:column
          (powerline-raw " " face1)
          (powerline-raw "%l:%2c" face1 'r)
          (funcall separator-right face1 line-face)
          (powerline-raw " " line-face))
         (list
          ;; global-mode
          (when active
            (powerline-raw global-mode-string)
            (powerline-raw " " line-face)))
         (when active
           (let ((progress (format-mode-line "%p")))
             (list
              ;; percentage in the file
              (powerline-raw "%p" line-face 'r)))))))

    (defun spacemacs/mode-line-prepare ()
      (let* ((active (powerline-selected-window-active))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (lhs (spacemacs/mode-line-prepare-left))
             (rhs (spacemacs/mode-line-prepare-right)))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs))))

    (setq-default mode-line-format
                  '("%e" (:eval (spacemacs/mode-line-prepare))))))
