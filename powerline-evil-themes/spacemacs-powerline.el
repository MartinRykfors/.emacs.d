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
