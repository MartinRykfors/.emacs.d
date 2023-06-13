(setq default-directory "~/")
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(setq menu-bar-mode nil)
(scroll-bar-mode -1)
(setq echo-keystrokes 0.01)
(global-set-key (kbd "<C-tab>") 'other-window)
(defun sett ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(defun ryk-thought ()
  (interactive)
  (let ((new-buffer (get-buffer-create "*thought*")))
    (with-current-buffer new-buffer
      ;; (funcall 'org-mode)
      (funcall 'evil-local-mode)
      (funcall 'key-leap-mode)
      (evil-insert-state))
    (switch-to-buffer new-buffer)))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;;unbind set-fill-column because I have never called it except by mistake when trying to do C-x C-f
(global-unset-key (kbd "C-x f"))
(global-set-key (kbd "C-x f") 'find-file)
;; unbind the keybind for sending mail, I press this by mistake sometimes and it is annoying
(global-unset-key (kbd "C-x m"))
(add-hook 'after-init-hook
          (lambda ()
            (load-file "~/.emacs.d/color-setup.el")
            (ryk-set-initial-theme)
            (ryk-set-font-height 180)))
(setq ryk-landing-file "~/org/landing.org")
(setq initial-buffer-choice
      (if (file-exists-p ryk-landing-file)
          ryk-landing-file
        (lambda ()
          (let ((new-buffer (get-buffer-create "*scratch*")))
            (with-current-buffer new-buffer
              (funcall 'lisp-interaction-mode)
              (funcall 'evil-local-mode)
              (set-buffer-modified-p nil))
           new-buffer))))
(global-set-key (kbd "M-`") 'other-frame)
(setq blink-matching-paren nil)
(setq disabled-command-function nil)
(setq dired-dwim-target t)
(setq inhibit-startup-screen t)

(setq tramp-histfile-override "~/.tramp_history")

;; I've found that when selecting a row from a compilation buffer (when using M-x ag mostly)
;; it will be really annoying and create a new window split every single time
;; This will check if we are opening a window from a compilation buffer
;; and try to open the file in the most sensible way
(defun display-buffer-from-compilation-p (buffer action)
  (unless current-prefix-arg
    (with-current-buffer (window-buffer)
      (or (derived-mode-p 'compilation-mode)
          (string= (buffer-name) "*xref*")))))

(setq display-buffer-alist '((display-buffer-from-compilation-p
                              (display-buffer-reuse-window display-buffer-use-some-window)
                              (inhibit-same-window . t))))
;; Never create horizontal splits
(setq split-height-threshold nil)

(if (eq system-type 'darwin)
    (progn
      (set-frame-parameter (selected-frame) 'height 56)
      (set-frame-parameter (selected-frame) 'width 203)
      (set-frame-position (first (frame-list)) 0 0))
  (progn
    (set-frame-parameter (selected-frame) 'menu-bar-lines 0)
    (toggle-frame-maximized)))

(defun ryk-set-font-height (height)
  (interactive (list (string-to-number
                      (read-from-minibuffer
                       (format "Font height (current is %s): "
                               (face-attribute 'default :height))))))
  (let ((f-height (frame-pixel-height))
        (f-width (frame-pixel-width)))
    (set-face-attribute 'default nil :height height)
    (set-frame-height (selected-frame) f-height nil t)
    (set-frame-width (selected-frame) (- f-width 16) nil t)))

(setq ryk-default-font-height 112) ; emacs default is 96
(ryk-set-font-height ryk-default-font-height)

(setq display-buffer-base-action
      '((display-buffer-reuse-window display-buffer-same-window)
        (reusable-frames . t)))

(setq even-window-sizes nil)

(defun ryk-toggle-whitespace ()
  (interactive)
  (setq-local show-trailing-whitespace (not show-trailing-whitespace))
  (message "Show trailing whitespace: %s" show-trailing-whitespace))
(defun ryk-show-whitespace ()
  (interactive)
  (setq-local show-trailing-whitespace t))

(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

(defun ryk-frame-alpha (value)
  (interactive "nalpha-value (0 - 10): ")
  (set-frame-parameter (selected-frame) 'alpha (* value 10)))

(defun ryk-sit ()
  (interactive)
  (shell-command "~/.screenlayout/sit.sh"))

(defun ryk-stand ()
  (interactive)
  (shell-command "~/.screenlayout/stand.sh"))

;; I keep on hitting C-x C-b when I mean C-x b
;; Unless I use C-u, interpret both as me meaning C-x b
(defun ryk-switch-buffer (arg)
  (interactive "P")
  (if arg
      (list-buffers)
    (ivy-switch-buffer)))
(global-set-key (kbd "C-x b") 'ryk-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ryk-switch-buffer)

;; same deal with C-x d
(global-set-key (kbd "C-x C-d") 'dired)

;;set up all packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(use-package diminish nord-theme))
  (unless (package-installed-p package)
    (package-install package)))

(setq use-package-verbose t)

(require 'use-package)

(use-package exec-path-from-shell
  :ensure t
  :if (not (eq system-type 'windows-nt))
  :init (exec-path-from-shell-initialize))

;; (use-package spacemacs-theme
;;   :ensure t)

(use-package evil
  :ensure t
  :config
  (progn
    (evil-set-undo-system 'undo-tree)
    (global-set-key (kbd "<f6>") 'evil-local-mode)
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "C-S-d") 'evil-scroll-up)
    (setq evil-search-module 'evil-search)
    (setq evil-want-change-word-to-end nil)
    (evil-define-operator ryk-evil-push-search-ring (beg end)
      "Push BEG -> END to search ring."
      (setq isearch-forward t)
      (push (buffer-substring-no-properties beg end) regexp-search-ring))
    (define-key evil-normal-state-map "\\" 'ryk-evil-push-search-ring)
    (define-key evil-visual-state-map "\\" 'ryk-evil-push-search-ring)
    (add-hook 'find-file-hook 'evil-local-mode)
    (add-hook 'shell-mode-hook 'evil-local-mode)))

(use-package evil-args
  :ensure t
  :config
  (progn
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)))

(use-package evil-numbers
  :ensure t
  :init
  (progn
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)))

(defun ryk--to-enclosing-paren (n)
  ;; we need to change n if we're in a string,
  ;; because paredit-fwd-up takes string delimiters into consideration as well
  (when (nth 3 (syntax-ppss))
    (if (> n 0)
        (setq n (+ 1 n))
      (setq n (- n 1))))
  (paredit-forward-up n)
  (when (< 0 n)
      (backward-char)))

(use-package paredit
  :diminish "()"
  :ensure t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (define-key paredit-mode-map (kbd "M-1") (lambda () (interactive) (ryk--to-enclosing-paren 1)))
    (define-key paredit-mode-map (kbd "M-2") (lambda () (interactive) (ryk--to-enclosing-paren 2)))
    (define-key paredit-mode-map (kbd "M-3") (lambda () (interactive) (ryk--to-enclosing-paren 3)))
    (define-key paredit-mode-map (kbd "M-4") (lambda () (interactive) (ryk--to-enclosing-paren 4)))
    (define-key paredit-mode-map (kbd "C-M-1") (lambda () (interactive) (ryk--to-enclosing-paren -1)))
    (define-key paredit-mode-map (kbd "C-M-2") (lambda () (interactive) (ryk--to-enclosing-paren -2)))
    (define-key paredit-mode-map (kbd "C-M-3") (lambda () (interactive) (ryk--to-enclosing-paren -3)))
    (define-key paredit-mode-map (kbd "C-M-4") (lambda () (interactive) (ryk--to-enclosing-paren -4)))))

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
  :config
  (progn
    (setq undo-tree-auto-save-history nil)
    (define-key undo-tree-visualizer-mode-map (kbd "j") 'undo-tree-visualize-redo)
    (define-key undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualize-undo)
    (define-key undo-tree-visualizer-mode-map (kbd "h") 'undo-tree-visualize-switch-branch-left)
    (define-key undo-tree-visualizer-mode-map (kbd "l") 'undo-tree-visualize-switch-branch-right)
    (global-undo-tree-mode)))

(use-package company
  :ensure t
  :diminish " ☭"
  :config
  (progn
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map (kbd "M-j") 'company-select-next)
    (define-key company-active-map (kbd "M-k") 'company-select-previous)
    (define-key company-active-map (kbd "M-u") 'company-abort)
    (define-key evil-insert-state-map (kbd "M-u") 'company-select-previous)
    (global-set-key (kbd "M-u") 'company-complete)
    (setq company-require-match nil)
    (setq company-idle-delay nil)
    (global-company-mode)))

(use-package magit
  :ensure t
  :bind ("C-S-m". magit-status)
  :config
  (progn
    (defun ryk-setup-git-commit-message ()
      (evil-insert-state)
      (setq-local fill-column 72))
    (add-hook 'git-commit-setup-hook 'ryk-setup-git-commit-message)
    (setq magit-revert-buffers t)
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)))

(use-package key-leap
  ;; :ensure t
  :load-path "/home/martin/code/key-leap/"
  :config
  (progn
    (global-set-key (kbd "<f5>") 'key-leap-mode)
    (if (eq system-type 'darwin)
        (setq key-leap-key-strings '("htnsgcrlmdw" "aoeui"))
      (setq key-leap-key-strings '("htnsdmgcrlwvbfz" "aoeuiyk")))
    (defun ryk--store-column ()
      (setq ryk--stored-column (current-column)))
    (defun back-to-indentation-no-dired ()
      (interactive)
      (if (eq major-mode 'dired-mode)
          (move-to-column ryk--stored-column)
        (back-to-indentation)))
    (add-hook 'key-leap-after-leap-hook 'back-to-indentation-no-dired)
    (add-hook 'key-leap-before-leap-hook 'ryk--store-column)
    (add-hook 'find-file-hook 'key-leap-mode)
    (add-hook 'dired-mode-hook 'key-leap-mode)
    (add-hook 'ag-mode-hook 'key-leap-mode)
    ;; (add-hook 'magit-status-mode-hook 'key-leap-mode)
    (setq key-leap-upcase-active t)))

(use-package evil-key-leap
  ;; :ensure t
  :load-path "/home/martin/code/evil-key-leap/"
  :config
  (progn
    (evil-key-leap-create-motion (kbd "<SPC>"))))

(use-package adaptive-wrap
  :ensure t)

(use-package org
  :bind* (("<C-tab>" . other-window)
          ("C-M-'" . org-capture)
          ("<C-M-return>" . org-insert-subheading)
          ("<S-C-M-return>" . org-insert-todo-subheading))
  :config
  (progn
    (add-hook 'org-mode-hook (lambda () (toggle-truncate-lines -1)))
    (add-hook 'org-mode-hook (lambda () (setq-local evil-auto-indent nil)))
    (add-hook 'org-mode-hook 'visual-line-mode)
    (add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode)
    (setq org-M-RET-may-split-line '((default . nil)))
    (setq org-insert-heading-respect-content t)
    (setq calendar-week-start-day 1)))

(use-package powerline
  :ensure t
  :init
  (progn
    (use-package powerline-evil
      :ensure t)
    (load-file "~/.emacs.d/powerline-evil-themes/spacemacs-powerline.el")
    (setq-default mode-line-format '("%e" (:eval (spacemacs/mode-line-prepare))))))

(use-package counsel
  :ensure t
  :bind (:map ivy-minibuffer-map
              ("M-j" . ivy-next-line)
              ("M-k" . ivy-previous-line)
              ("TAB" . ivy-alt-done)
              ("RET" . ivy-alt-done)
              ("M-RET" . ivy-immediate-done))
  :init
  (progn
    (setq ivy-re-builders-alist
          '((t . ivy--regex-fuzzy)))
    (ivy-mode)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (use-package flx
      :ensure t)
    (use-package smex
      :ensure t))
  :config
  (progn
    (setq ivy-display-functions-alist '())
    (add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))
    (use-package ivy-rich
      :ensure t)
    (setq ivy-format-function #'ivy-format-function-line)))

(use-package ag
  :ensure t)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(unless (file-exists-p custom-file)
  (shell-command (concat "touch " custom-file)))
(load custom-file)

(use-package ediff
  :config
  (progn
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)
    (setq ediff-split-window-function 'split-window-horizontally)))

(use-package ws-butler
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'feature-mode-hook #'ws-butler-mode)
  (add-hook 'text-mode-hook #'ws-butler-mode)
  (add-hook 'ws-butler-mode-hook #'ryk-show-whitespace)
  (setq ws-butler-keep-whitespace-before-point nil))

;; (use-package elpy
;;   :ensure t
;;   :bind
;;   (:map elpy-mode-map
;;         ("C-c M-q" . elpy-format-code))
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable)
;;   :config
;;   (setq elpy-formatter 'black)
;;   (setq elpy-rpc-timeout 3))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-S-l")
  :config
  (lsp-register-custom-settings
   '(("pylsp.plugins.black.enabled" t t)))
  (setq lsp-pylsp-plugins-pydocstyle-enabled nil)
  (setq lsp-pylsp-plugins-mccabe-enabled nil)
  (setq lsp-diagnostics-attributes nil)
  :hook
  ((python-mode . lsp)))

(use-package highlight-indentation
  :hook
  ((python-mode . highlight-indentation-mode)))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-flycheck-enable nil))

(use-package flycheck)

(use-package dsvn)
