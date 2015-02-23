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
