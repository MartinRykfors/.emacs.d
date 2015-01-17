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

;;start in scratch
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

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

;;configure-ryk-mode
(add-to-list 'load-path "~/.emacs.d/ryk-mode/")
(require 'ryk-mode)
(add-hook 'cider-mode-hook 'ryk-mode)

;;configure yasnippet
(require 'yasnippet)
(yas-global-mode 1)
