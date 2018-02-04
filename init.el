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
      (funcall 'org-mode)
      (funcall 'evil-local-mode)
      (funcall 'key-leap-mode)
      (evil-insert-state))
    (switch-to-buffer new-buffer)))
(electric-pair-mode)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;;unbind set-fill-column because I have never called it except by mistake when trying to do C-x C-f
(global-unset-key (kbd "C-x f"))
(global-set-key (kbd "C-x f") 'ido-find-file)
;; (global-set-key [escape] 'keyboard-escape-quit)
(load "~/.emacs.d/stars.el")
(add-hook 'after-init-hook (lambda () (load-file "~/.emacs.d/color-setup.el")))
(setq ryk-landing-file "~/org/landing.org")
(setq initial-buffer-choice
      (if (file-exists-p ryk-landing-file)
          ryk-landing-file
        (lambda ()
          (let ((new-buffer (get-buffer-create "*scratch*")))
            (with-current-buffer new-buffer
              (funcall 'lisp-interaction-mode)
              (funcall 'evil-local-mode)
              (insert-star-lines)
              (set-buffer-modified-p nil))
            new-buffer))))
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)
(setq w32-lwindow-modifier 'hyper)
(global-set-key (kbd "M-`") 'other-frame)
(setq blink-matching-paren nil)
(setq disabled-command-function nil)

(if (eq system-type 'darwin)
    (set-default-font "Source Code Pro")
  (progn
    (set-default-font "Consolas")
    (set-face-attribute 'default nil :height 105)))

(require 'cl)
(if (eq system-type 'darwin)
    (progn
      (set-frame-parameter (selected-frame) 'height 56)
      (set-frame-parameter (selected-frame) 'width 203)
      (set-frame-position (first (frame-list)) 0 0))
  (progn
    (set-frame-parameter (selected-frame) 'menu-bar-lines 0)
    (setq frame-resize-pixelwise t)
    (let* ((outer-width (/ (display-pixel-width) 2))
           (inner-width (- outer-width 32))
           (inner-height 1372))
      (set-frame-width (selected-frame) 1248 nil t)
      (set-frame-height (selected-frame) inner-height nil t)
      (set-frame-parameter (selected-frame) 'left outer-width)
      (set-frame-parameter (selected-frame) 'top 0))))

(defun ryk-set-font-height (height)
  (interactive "nFont height: ")
  (let ((f-height (frame-pixel-height))
        (f-width (frame-pixel-width)))
    (set-face-attribute 'default nil :height height)
    (set-frame-height (selected-frame) f-height nil t)
    (set-frame-width (selected-frame) (- f-width 16) nil t)))

(defun ryk-open-scope ()
  (interactive)
  (end-of-line)
  (insert "{}")
  (backward-char)
  (newline-and-indent))

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

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(use-package diminish))
  (unless (package-installed-p package)
    (package-install package)))

(setq use-package-verbose t)

(require 'use-package)

(use-package spacemacs-theme
  :ensure t
  :init
  (progn
    (setq spacemacs-theme-comment-bg nil)
    (setq spacemacs-theme-org-highlight t)
    (setq spacemacs-theme-org-height nil)
    (custom-set-variables '(spacemacs-theme-custom-colors '((lnum . "#6f50e0"))))))

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
    (diminish 'yas-minor-mode)))

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
    (define-key evil-normal-state-map (kbd "C-<return>") 'ryk-open-scope)
    (define-key evil-insert-state-map (kbd "M-'") (lambda () (interactive) (insert ?å)))
    (define-key evil-insert-state-map (kbd "M-,") (lambda () (interactive) (insert ?ä)))
    (define-key evil-insert-state-map (kbd "M-.") (lambda () (interactive) (insert ?ö)))
    (define-key evil-insert-state-map (kbd "M-\"") (lambda () (interactive) (insert ?Å)))
    (define-key evil-insert-state-map (kbd "M-<") (lambda () (interactive) (insert ?Ä)))
    (define-key evil-insert-state-map (kbd "M->") (lambda () (interactive) (insert ?Ö)))
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-<return>") 'ryk-open-scope)
    (setq evil-search-module 'evil-search)
    (setq evil-want-change-word-to-end nil)
    (evil-define-operator ryk-evil-push-search-ring (beg end)
      "Push BEG -> END to search ring."
      (setq isearch-forward t)
      (push (buffer-substring-no-properties beg end) regexp-search-ring))
    (define-key evil-normal-state-map "\\" 'ryk-evil-push-search-ring)
    (define-key evil-visual-state-map "\\" 'ryk-evil-push-search-ring)
    (add-hook 'find-file-hook 'evil-local-mode)))

(use-package evil-numbers
  :ensure t
  :init
  (progn
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-;") 'evil-numbers/dec-at-pt)))

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
    (unbind-key "<return>" company-active-map)
    (unbind-key (kbd "RET") company-active-map)
    (unbind-key "[tab]" company-active-map)
    (unbind-key (kbd "TAB") company-active-map)
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map (kbd "M-j") 'company-select-next)
    (define-key company-active-map (kbd "M-k") 'company-select-previous)
    (define-key evil-insert-state-map (kbd "M-u") 'company-select-previous)
    (global-set-key (kbd "M-u") 'company-complete)
    (setq company-require-match nil)
    (global-company-mode)))

(use-package magit
  :ensure t
  :bind ("C-S-m". magit-status)
  :init
  (progn
    (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))
  :config
  (progn
    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    (setq magit-revert-buffers t)))

(use-package clojure-mode
  :ensure t
  :if (eq system-type 'darwin)
  :config
  (progn
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'highlight-parentheses-mode)))

(use-package cider
  :ensure t
  :if (eq system-type 'darwin)
  :config
  (progn
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-auto-jump-to-error nil)))

(use-package processing-mode
  :ensure t
  :mode ("\\.pde$" . processing-mode)
  :init
  (progn
    (setq processing-location "~/processing-java")
    (setq processing-application-dir "/Applications/Processing.app")
    (setq processing-sketchbook-dir "/Users/ryk/code/processing")))

(use-package glsl-mode
  :ensure t
  :if (eq system-type 'darwin)
  :init
  (append auto-mode-alist '('("\\.glsl\\'" . glsl-mode)
                            '("\\.vert\\'" . glsl-mode)
                            '("\\.frag\\'" . glsl-mode)
                            '("\\.geom\\'" . glsl-mode)))
  :config
  (add-hook 'glsl-mode-hook (lambda () (c-set-style "k&r"))))

(use-package ryk-mode
  :load-path "~/.emacs.d/ryk-mode"
  :if (eq system-type 'darwin)
  :config
  (add-hook 'cider-mode-hook 'ryk-mode))

(use-package smex
  :ensure t
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
      (setq key-leap-key-strings '("htnsdmgcrlwvbz" "aoeuiy")))
    (add-hook 'key-leap-after-leap-hook 'back-to-indentation)
    (add-hook 'find-file-hook 'key-leap-mode)
    (setq key-leap-upcase-active nil)
    (key-leap-create-evil-motion (kbd "<SPC>"))))

(use-package projectile
  :if (eq system-type 'darwin)
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-mode-line '(:eval (format " Pt[%s]" (projectile-project-name))))))

(use-package aggressive-indent
  :ensure t)

(use-package adaptive-wrap
  :ensure t)

(defun ryk--add-babel-cmd ()
  (require 'ob-sh)
  (defadvice org-babel-sh-evaluate (around set-shell activate)
    "Add header argument :shcmd that determines the shell to be called."
    (let* ((org-babel-sh-command (or (cdr (assoc :shcmd params)) org-babel-sh-command)))
      ad-do-it)))
(setq ryk-workitem-file "~/org/workitems.org" )
(setq ryk-meetings-file "~/org/meetings.org" )
(use-package org
  :bind* (("<C-tab>" . other-window)
          ("C-M-'" . org-capture)
          ("<C-M-return>" . org-insert-subheading)
          ("<S-C-M-return>" . org-insert-todo-subheading))
  :config
  (progn
    (defhydra hydra-open-favorite-org-files (:hint nil :color blue)
      "
Open org file: _w_: workitems.org     _m_: meetings.org"
      ("w" (lambda () (interactive) (find-file ryk-workitem-file)))
      ("m" (lambda () (interactive) (find-file ryk-meetings-file))))
    (global-set-key (kbd "C-M-\"") 'hydra-open-favorite-org-files/body)
    (setq org-capture-templates
          '(("a" "Workitem" entry (file+headline ryk-workitem-file "To do")
             "** TODO %?")))
    (add-hook 'org-capture-mode-hook (lambda () (interactive) (evil-insert 1)))
    (add-hook 'org-mode-hook (lambda () (toggle-truncate-lines -1)))
    (add-hook 'org-mode-hook 'visual-line-mode)
    (add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode)
    (setq org-M-RET-may-split-line '((default . nil)))
    (setq org-insert-heading-respect-content t)
    (setq calendar-week-start-day 1)
    (setq org-adapt-indentation nil)
    (if (eq system-type 'windows-nt)
        (ryk--add-babel-cmd)
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((sh . t))))
    (define-key org-mode-map (kbd "C-M-y") 'hydra-move-org-headings/body)))

(use-package hydra
  :ensure t
  :config
  (progn
    (setq hydra-lv t)
    (defhydra hydra-move-org-headings (:hint nil)
      "
Move headings: _h__j__k__l_: ←↓↑→  _H__J__K__L_: ◁▽△▷
"
      ("h" org-metaleft)
      ("j" org-metadown)
      ("k" org-metaup)
      ("l" org-metaright)
      ("H" org-shiftmetaleft)
      ("J" org-shiftmetadown)
      ("L" org-shiftmetaright)
      ("K" org-shiftmetaup))))

(use-package powerline
  :ensure t
  :init
  (progn
    (use-package powerline-evil
      :ensure t)
    (load-file "~/.emacs.d/powerline-evil-themes/spacemacs-powerline.el")
    (setq-default mode-line-format '("%e" (:eval (spacemacs/mode-line-prepare))))))

(use-package haskell-mode
  :ensure t
  :config
  (progn
    (use-package intero
      :ensure t
      :config
      (progn 
        (add-hook 'haskell-mode-hook 'intero-mode)
        (add-hook 'haskell-mode-hook (lambda () (setq evil-auto-indent nil)))))))

(use-package emms
  :config
  (progn
    (emms-all)
    (emms-default-players)
    (emms-add-directory-tree "~/mp3/")))

(use-package dired-launch
  :ensure t
  :config
  (progn
    (add-hook 'dired-mode-hook 'dired-launch-mode)))
