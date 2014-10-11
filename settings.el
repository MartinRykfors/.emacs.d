;;configure environment
(setq default-directory "~/")
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(setq initial-frame-alist '((top . 0) (left . 0) (width . 202) (height . 48)))
(setq menu-bar-mode nil)
(scroll-bar-mode -1)
(setq echo-keystrokes 0.01)
(global-set-key (kbd "<C-tab>") 'other-window)
(setq scroll-margin 4)
(setq scroll-conservatively 1)
(setq show-paren-delay 0)
(show-paren-mode)

;;configure theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'granger t)

;;configure evil
(add-to-list 'load-path "~/.emacs.d/powerline-evil")
(require 'evil)
(require 'powerline)
(require 'powerline-evil)
(powerline-evil-center-color-theme)
(global-set-key (kbd "<f6>") 'evil-local-mode) 
(setq powerline-default-separator (quote arrow-fade))
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
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
(exec-path-from-shell-initialize)

;;start in scratch
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;;use evil local when visiting a file
(add-hook 'find-file-hook 'evil-local-mode)

;;angry police captain
(global-set-key (kbd "<f2>") 'angry-police-captain)

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

;;configure company
(require 'company)
(global-company-mode)
(global-set-key (kbd "M-e") 'company-select-next)
(global-set-key (kbd "M-u") 'company-select-previous)
(define-key evil-insert-state-map (kbd "M-p") 'company-select-previous)
(set-face-background 'company-tooltip (face-attribute 'default :background))
(set-face-foreground 'company-tooltip (face-attribute 'font-lock-constant-face :foreground))
(set-face-foreground 'company-tooltip-selection (face-attribute 'default :background))
(set-face-background 'company-tooltip-selection (face-attribute 'font-lock-constant-face :foreground))
(set-face-background 'company-scrollbar-bg (face-attribute 'default :background))
(set-face-background 'company-scrollbar-fg (face-attribute 'font-lock-variable-name-face :foreground))
(set-face-foreground 'company-tooltip-common (face-attribute 'font-lock-variable-name-face :foreground))
(set-face-foreground 'company-tooltip-common-selection (face-attribute 'default :background))
