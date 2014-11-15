(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/settings.el")))

; list the packages you want
(setq package-list '(angry-police-captain 
                     evil 
                     powerline 
                     powerline-evil 
                     evil-numbers 
                     ace-jump-mode
                     haskell-mode 
                     undo-tree 
                     highlight-parentheses 
                     company 
                     clojure-mode 
                     cider 
                     exec-path-from-shell 
                     paredit 
                     glsl-mode 
                     aggressive-indent
                     narrow-indirect
                     expand-region
                     evil-surround))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))






