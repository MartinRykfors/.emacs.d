(require 'ryk-mode)
(require 'ert)
(require 'el-mock)

(setq ryk-mode-test--fader-line "(fader 0 1 \"-#-\")")
(setq ryk-mode-test--fader-line-upped "(fader 0 1 \"--#\")")
(setq ryk-mode-test--fader-line-downed "(fader 0 1 \"#--\")")

(ert-deftest increases-fader ()
  "it increases the fader"
  (should (string= ryk-mode-test--fader-line-upped
                   (with-temp-buffer
                     (insert ryk-mode-test--fader-line)
                     (ryk-increase-fader)
                     (buffer-string)))))

(ert-deftest decreases-fader ()
  "it decreases the fader"
  (should (string= ryk-mode-test--fader-line-downed
                   (with-temp-buffer
                     (insert ryk-mode-test--fader-line)
                     (ryk-decrease-fader)
                     (buffer-string)))))

(ert-deftest no-op-trying-to-increase-past-max ()
  "It will not call cider-eval when the fader is already at max"
  (with-mock
    (not-called ryk--cider-eval)
    (with-temp-buffer
      (insert ryk-mode-test--fader-line-upped)
      (ryk-increase-fader))))

(ert-deftest no-op-trying-to-decrease-past-min ()
  "It will not call cider-eval when the fader is already at min"
  (with-mock
    (not-called ryk--cider-eval)
    (with-temp-buffer
      (insert ryk-mode-test--fader-line-downed)
      (ryk-decrease-fader))))

(setq ryk-mode-test--synthdef
"(defsynth [foo 3]
    (filter |))")

(setq ryk-mode-test--synthdef-with-parameter
"(defsynth [foo 3 freq 40]
    (filter freq))")

(ert-deftest test-add-synth-parameter ()
  (should (string= ryk-mode-test--synthdef-with-parameter
                   (with-temp-buffer
                     (insert ryk-mode-test--synthdef)
                     (beginning-of-buffer)
                     (search-forward "|")
                     (delete-char -1)
                     (ryk-add-synth-parameter "freq" 40)
                     (buffer-string)))))

(setq ryk-mode-test--single-parameter-string "[foo 20]")
(setq ryk-mode-test--single-parameter-list (list (cons "foo" "20")))
(ert-deftest test-get-single-parameter-from-string ()
  (with-temp-buffer
    (insert ryk-mode-test--single-parameter-string)
    (goto-char (point-min))
    (should (equal ryk-mode-test--single-parameter-list
                   (ryk--get-parameters-at-point)))))

(setq ryk-mode-test--nice-parameter-string "[foo 20 bar-baz 30 zyx 70]")
(setq ryk-mode-test--nice-parameter-list '(("foo" . "20") ("bar-baz" . "30") ("zyx" . "70")))

(ert-deftest test-get-parameters-from-string ()
  (with-temp-buffer
    (insert ryk-mode-test--nice-parameter-string)
    (goto-char (point-min))
    (should (equal ryk-mode-test--nice-parameter-list
                   (ryk--get-parameters-at-point)))))
