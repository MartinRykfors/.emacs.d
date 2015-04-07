(require 'ryk-mode)
(require 'ert)
(require 'el-mock)

(setq fader-line "(fader 0 1 \"-#-\")")
(setq fader-line-upped "(fader 0 1 \"--#\")")
(setq fader-line-downed "(fader 0 1 \"#--\")")

(ert-deftest increases-fader ()
  "it increases the fader"
  (should (string= fader-line-upped
                   (with-temp-buffer
                     (insert fader-line)
                     (ryk-increase-fader)
                     (buffer-string)))))

(ert-deftest decreases-fader ()
  "it decreases the fader"
  (should (string= fader-line-downed
                   (with-temp-buffer
                     (insert fader-line)
                     (ryk-decrease-fader)
                     (buffer-string)))))

(ert-deftest no-op-trying-to-increase-past-max ()
  "It will not call cider-eval when the fader is already at max"
  (with-mock
    (not-called ryk--cider-eval)
    (with-temp-buffer
      (insert fader-line-upped)
      (ryk-increase-fader))))

(ert-deftest no-op-trying-to-decrease-past-min ()
  "It will not call cider-eval when the fader is already at min"
  (with-mock
    (not-called ryk--cider-eval)
    (with-temp-buffer
      (insert fader-line-downed)
      (ryk-decrease-fader))))

(setq synthdef
"(defsynth [foo 3]
    (filter |))")

(setq synthdef-with-parameter
"(defsynth [foo 3 freq 40]
    (filter freq))")

(defun replace-pipe-with-point ()
  "Utility for moving the point to the '|' char of the buffer and deleting it"
                     (beginning-of-buffer)
                     (buffer-string)
                     (search-forward "|")
                     (delete-char -1))

(ert-deftest test-add-synth-parameter ()
  (should (string= synthdef-with-parameter
                   (with-temp-buffer
                     (insert synthdef)
                     (replace-pipe-with-point)
                     (ryk-add-synth-parameter "freq" 40)
                     (buffer-string)))))

(setq single-parameter-string "[foo 20]")
(setq single-parameter-list (list (cons "foo" "20")))
(ert-deftest test-get-single-parameter-from-string ()
  (with-temp-buffer
    (insert single-parameter-string)
    (goto-char (point-min))
    (should (equal single-parameter-list
                   (ryk--get-parameters-at-point)))))

(setq nice-parameter-string "[foo 20 bar-baz 30 zyx 70]")
(setq nice-parameter-list '(("foo" . "20") ("bar-baz" . "30") ("zyx" . "70")))
(ert-deftest test-get-parameters-from-string ()
  (with-temp-buffer
    (insert nice-parameter-string)
    (goto-char (point-min))
    (should (equal nice-parameter-list
                   (ryk--get-parameters-at-point)))))

(setq nasty-parameter-string
"[foo   20 
    bar-baz 30 zyx 
70]")
(setq nasty-parameter-list '(("foo" . "20") ("bar-baz" . "30") ("zyx" . "70")))
(ert-deftest test-get-nasty-parameters-from-string ()
  "it handles finding synth parameters that are formatted badly"
  (with-temp-buffer
    (insert nasty-parameter-string)
    (goto-char (point-min))
    (should (equal nasty-parameter-list
                   (ryk--get-parameters-at-point)))))

(setq before-synth-call-insert
"(defsynth hello [foo2 20 bar5 80])
(hello |)")
(setq after-synth-call-insert
"(defsynth hello [foo2 20 bar5 80])
(hello :foo2 20 :bar5 80)")

(ert-deftest test-insert-synth-parameters ()
  "When calling insert-synth-params it will insert the parameters of the specified synth"
  (should (string= after-synth-call-insert
                   (with-temp-buffer
                     (insert before-synth-call-insert)
                     (replace-pipe-with-point)
                     (ryk-insert-synth-call "hello")
                     (buffer-string)))))

(ert-deftest insert-synth-parameters-messages-on-not-found ()
  "It prints an error message if trying to add parameters of a synth that does not exist"
  (with-mock
    (mock (princ *) :times 1)
    (with-temp-buffer
      (insert before-synth-call-insert)
      (replace-pipe-with-point)
      (ryk-insert-synth-call "non-existing-synth"))))
