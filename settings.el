;;configure theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'brin t)

;;configure environment
(setq default-directory "~/")
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(setq menu-bar-mode nil)
(scroll-bar-mode -1)
(setq echo-keystrokes 0.01)
(global-set-key (kbd "<C-tab>") 'other-window)
(setq scroll-margin 4)
(setq scroll-conservatively 1)
(setq show-paren-delay 0)
(show-paren-mode)
(defun sett ()
  (interactive)
  (find-file "~/.emacs.d/settings.el"))
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(toggle-frame-maximized)
(electric-pair-mode)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-set-style "java")
(setq c-basic-offset 4)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;configure evil
(require 'evil)
(require 'powerline)
(require 'powerline-evil)
(add-to-list 'load-path "~/.emacs.d/powerline-evil-themes")
(require 'powerline-evil-local-theme)
(powerline-evil-local-center-color-theme)
(setq powerline-default-separator (quote nil))
(global-set-key (kbd "<f6>") 'evil-local-mode) 
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(set-face-background 'powerline-active1 (color-lighten-name (face-background 'mode-line) 10))
(set-face-background 'powerline-active2 (color-lighten-name (face-background 'mode-line) 20))
(set-face-foreground 'powerline-active1 (color-lighten-name (face-foreground 'mode-line) 10))
(set-face-foreground 'powerline-active2 (color-lighten-name (face-foreground 'mode-line) 10))
(set-face-background 'powerline-inactive1 (color-darken-name (face-background 'mode-line) 10))
(set-face-background 'powerline-inactive2 (color-darken-name (face-background 'mode-line) 10))
(set-face-foreground 'powerline-inactive1 (color-darken-name (face-foreground 'mode-line) 20))
(set-face-foreground 'powerline-inactive2 (color-darken-name (face-foreground 'mode-line) 20))
(set-face-background 'powerline-evil-normal-face "SpringGreen3")
(set-face-background 'powerline-evil-insert-face "blue3")
(set-face-background 'powerline-evil-visual-face "goldenrod")
(set-face-background 'powerline-evil-operator-face "red1")
(setq evil-search-module 'evil-search)
(require 'evil-visualstar)

;;configure evis-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'paren)
(set-face-background 'show-paren-match (color-lighten-name (face-background 'default) 20))
(set-face-foreground 'show-paren-match (face-foreground 'default))
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(set-face-foreground 'isearch "green1")
(set-face-background 'isearch (color-darken-name (face-foreground 'isearch) 25))
(set-face-foreground 'lazy-highlight "green1")
(set-face-background 'lazy-highlight (color-darken-name (face-foreground 'lazy-highlight) 35))

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;;use evil-numbers
(require 'evil-numbers)
(global-unset-key (kbd "C-a"))
(global-set-key (kbd "C-a") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-;") 'evil-numbers/dec-at-pt)

;;configure ace-jump
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-char-mode)

;;configure haskell mode
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;;get exec paths
(if (not (eq system-type 'windows-nt))
    (exec-path-from-shell-initialize))

;;start in scratch
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;;use evil local when visiting a file
(add-hook 'find-file-hook 'evil-local-mode)

;;configure clojure mode
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)

;;configure undo-tree
(define-key undo-tree-visualizer-mode-map (kbd "j") 'undo-tree-visualize-redo)
(define-key undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualize-undo)
(define-key undo-tree-visualizer-mode-map (kbd "h") 'undo-tree-visualize-switch-branch-left)
(define-key undo-tree-visualizer-mode-map (kbd "l") 'undo-tree-visualize-switch-branch-right)

;;configure highlight parantheses
(require 'highlight-parentheses)
(setq hl-paren-colors '("red1" "green2" "orange1" "DeepSkyBlue1" ))
(setq hl-paren-background-colors (mapcar (lambda (col) (color-darken-name col 20)) hl-paren-colors))
(set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
(global-highlight-parentheses-mode)
;;make paren highlight update after stuff like paredit changes
(add-to-list 'after-change-functions '(lambda (&rest x) (hl-paren-highlight)))

;;configure company
(require 'company)
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
(global-company-mode)

;;configure processing2-emacs
(add-to-list 'load-path "~/.emacs.d/processing2-emacs/")
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(setq processing-location "/usr/bin/processing-java")
(setq processing-application-dir "/Applications/Processing.app")
(setq processing-sketchbook-dir "/Users/ryk/code/processing")

;;configure glsl-mode
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;;configure narrow-indirect
(require 'narrow-indirect)
(define-key ctl-x-4-map "nn" 'ni-narrow-to-region-indirect-other-window)
(set-face-attribute 'ni-mode-line-buffer-id nil :box (face-background 'powerline-evil-normal-face))
(setq ni-narrowed-buf-name-max 10)

;;configure expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

;;play-sound
(add-to-list 'load-path "~/.emacs.d/play-sound-osx")
(unless (and (fboundp 'play-sound-internal)
             (subrp (symbol-function 'play-sound-internal)))
  (require 'play-sound))
(setq ryk-typewriter-ding nil)
(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (if ryk-typewriter-ding
                (play-sound-file "~/.emacs.d/sounds/204466__nhumphrey__typewriter-ding_cropped.wav"))))
(defun ryk-typewriter ()
  (interactive)
  (if ryk-typewriter-ding
      (progn
        (setq ryk-typewriter-ding nil)
        (princ "old-timey hacking disabled"))
    (progn
      (setq ryk-typewriter-ding t)
      (princ "old-timey hacking enabled"))))
(global-set-key (kbd "<f3>") 'ryk-typewriter)

;;configure magit
(global-set-key (kbd "C-S-m") 'magit-status)

;;configure fsharp-mode
(add-hook 'fsharp-mode-hook
 (lambda ()
   (define-key fsharp-mode-map (kbd "M-RET") 'fsharp-eval-region)))

;;configure auto-complete
(require 'auto-complete)
(define-key ac-mode-map (kbd "M-p") 'auto-complete)
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "M-e") 'ac-next)
(define-key ac-menu-map (kbd "M-u") 'ac-previous)

;;configure erc
(require 'erc)
(set-face-attribute 'erc-notice-face nil :height 100)

;;configure-ryk-mode
(add-to-list 'load-path "~/.emacs.d/ryk-mode/")
(require 'ryk-mode)
(add-hook 'cider-mode-hook 'ryk-mode)

;;configure yasnippet
(require 'yasnippet)
(yas-global-mode 1)
