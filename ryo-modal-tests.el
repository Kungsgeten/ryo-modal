(require 'ert)
(require 'ryo-modal)

(defmacro rmt--with-clean-keymap (map &rest body)
  `(unwind-protect
       (progn
         (setq ,map (make-sparse-keymap))
         ,@body)
     (setq ,map (make-sparse-keymap))))

(defmacro rmt--with-clean-ryo-modal-mode-keymap (&rest body)
  `(rmt--with-clean-keymap ryo-modal-mode-map ,@body))

(defmacro rmt--with-ryo-modal-mode-enabled (&rest body)
  `(unwind-protect
       (progn
         (ryo-modal-mode 1)
         ,@body)
     (ryo-modal-mode 0)))

(defvar rmt--mock-function-calls nil)
(defmacro rmt--expect-mock-calls (expected-calls &rest body)
  `(unwind-protect
       (progn
         (setq rmt--mock-function-calls nil)
         ,@body
         (should (equal (reverse rmt--mock-function-calls) ,expected-calls)))
     (setq rmt--mock-function-called nil)))

(defmacro rmt--with-temp-buffer (&rest body)
  `(with-temp-buffer
     (save-window-excursion
       (switch-to-buffer (current-buffer))
       (set-window-buffer nil (current-buffer))
       ,@body)))

(defmacro rmt--with-temporary-global-binding (key binding &rest body)
  `(unwind-protect
       (progn
         (global-set-key ,key ,binding)
         ,@body)
     (global-unset-key ,key)))

(defun rmt--dummy-function-1 () (interactive))
(defun rmt--dummy-function-2 () (interactive))
(defun rmt--dummy-function-3 () (interactive))
(defun rmt--dummy-function-4 () (interactive))
(defun rmt--dummy-function-5 () (interactive))
(defun rmt--dummy-function-6 () (interactive))
(defun rmt--dummy-function-7 () (interactive))
(defun rmt--dummy-function-8 () (interactive))
(defun rmt--dummy-function-9 () (interactive))

(defun rmt--mock-function-1 () (interactive) (push "mock-1" rmt--mock-function-calls))
(defun rmt--mock-function-2 () (interactive) (push "mock-2" rmt--mock-function-calls))
(defun rmt--mock-function-3 () (interactive) (push "mock-3" rmt--mock-function-calls))
(defun rmt--mock-function-4 () (interactive) (push "mock-4" rmt--mock-function-calls))

(defvar rmt--predefined-custom-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'rmt--dummy-function-1)
    (define-key map (kbd "s") #'rmt--dummy-function-2)
    (define-key map (kbd "C-d") #'rmt--dummy-function-3)
    (define-key map (kbd "C-f") #'rmt--dummy-function-4)
    map))

(ert-deftest rmt--ryo-modal-key--once ()
  (let ((expected-map (make-sparse-keymap)))
    (define-key expected-map (kbd "G") #'rmt--dummy-function-1)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key "G" 'rmt--dummy-function-1)
     (should (equal ryo-modal-mode-map expected-map)))))

(ert-deftest rmt--ryo-modal-key--multiple-times ()
  (let ((expected-map (make-sparse-keymap)))
    (define-key expected-map (kbd "H") #'rmt--dummy-function-1)
    (define-key expected-map (kbd "I") #'rmt--dummy-function-2)
    (define-key expected-map (kbd "J") #'rmt--dummy-function-3)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key "H" 'rmt--dummy-function-1)
     (ryo-modal-key "I" 'rmt--dummy-function-2)
     (ryo-modal-key "J" 'rmt--dummy-function-3)
     (should (equal ryo-modal-mode-map expected-map)))))

(ert-deftest rmt--ryo-modal-key--multi-key-keybinding ()
  (let ((expected-map (make-sparse-keymap)))
    (define-key expected-map (kbd "K L M") #'rmt--dummy-function-1)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key "K L M" 'rmt--dummy-function-1)
     (should (equal ryo-modal-mode-map expected-map)))))

(ert-deftest rmt--ryo-modal-key--multiple-bindings-with-common-prefix-key ()
  (let ((expected-map (make-sparse-keymap)))
    (define-key expected-map (kbd "SPC q") #'rmt--dummy-function-1)
    (define-key expected-map (kbd "SPC w") #'rmt--dummy-function-2)
    (define-key expected-map (kbd "SPC e") #'rmt--dummy-function-3)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key
      "SPC" '(("q" rmt--dummy-function-1)
              ("w" rmt--dummy-function-2)
              ("e" rmt--dummy-function-3)))
     (should (equal ryo-modal-mode-map expected-map)))))

(ert-deftest rmt--ryo-modal-key--nested-prefix-keys ()
  (let ((expected-map (make-sparse-keymap)))
    (define-key expected-map (kbd "SPC q a") #'rmt--dummy-function-1)
    (define-key expected-map (kbd "SPC q s") #'rmt--dummy-function-2)
    (define-key expected-map (kbd "SPC w d") #'rmt--dummy-function-3)
    (define-key expected-map (kbd "SPC w f") #'rmt--dummy-function-4)
    (define-key expected-map (kbd "SPC e g") #'rmt--dummy-function-5)
    (define-key expected-map (kbd "SPC e h") #'rmt--dummy-function-6)
    (define-key expected-map (kbd "SPC r j") #'rmt--dummy-function-7)
    (define-key expected-map (kbd "SPC r t k") #'rmt--dummy-function-8)
    (define-key expected-map (kbd "SPC r t l") #'rmt--dummy-function-9)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key
      "SPC"
      '(
        ("q"
         (("a" rmt--dummy-function-1)
          ("s" rmt--dummy-function-2)))
        ("w"
         (("d" rmt--dummy-function-3)
          ("f" rmt--dummy-function-4)))
        ("e"
         (("g" rmt--dummy-function-5)
          ("h" rmt--dummy-function-6)))
        ("r"
         (("j" rmt--dummy-function-7)
          ("t"
           (("k" rmt--dummy-function-8)
            ("l" rmt--dummy-function-9)))))))
     (should (equal ryo-modal-mode-map expected-map)))))

(ert-deftest rmt--ryo-modal-key--translate-single-keypress ()
  (let ((expected-map (make-sparse-keymap)))
    (define-key expected-map (kbd "b") #'backward-char)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key "b" "C-b")
     (should (equal ryo-modal-mode-map expected-map)))))

(ert-deftest rmt--ryo-modal-key--translate-multiple-keypresses ()
  (let ((expected-map (make-sparse-keymap)))
    (define-key expected-map (kbd "Q") #'save-buffers-kill-terminal)
    (define-key expected-map (kbd "W") #'widen)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key "Q" "C-x C-c")
     (ryo-modal-key "W" "C-x n w")
     (should (equal ryo-modal-mode-map expected-map)))))

(ert-deftest rmt--ryo-modal-key--mode-keyword ()
  (let ((expected-ryo-map (make-sparse-keymap))
        (expected-mode-map (make-sparse-keymap)))
    (define-key expected-ryo-map (kbd "A") #'rmt--dummy-function-1)
    (define-key expected-mode-map (kbd "A") #'rmt--dummy-function-2)
    (rmt--with-clean-ryo-modal-mode-keymap
     (rmt--with-clean-keymap
      ryo-org-mode-map
      (ryo-modal-key "A" 'rmt--dummy-function-1)
      (ryo-modal-key "A" 'rmt--dummy-function-2 :mode 'org-mode)
      (should (equal ryo-modal-mode-map expected-ryo-map))
      (should (equal ryo-org-mode-map expected-mode-map))))))

(ert-deftest rmt--ryo-modal-key--norepeat-keyword ()
  (let ((expected-map (make-sparse-keymap)))
    (define-key expected-map (kbd "A") #'rmt--dummy-function-1)
    (define-key expected-map (kbd "S") #'rmt--dummy-function-2)
    (define-key expected-map (kbd "D") #'rmt--dummy-function-3)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key "A" 'rmt--dummy-function-1)
     (ryo-modal-key "S" 'rmt--dummy-function-2 :norepeat t)
     (ryo-modal-key "D" 'rmt--dummy-function-3 :norepeat nil)
     (should (equal ryo-modal-mode-map expected-map))
     (should-not (seq-contains-p ryo-modal--non-repeating-commands 'rmt--dummy-function-1))
     (should (seq-contains-p ryo-modal--non-repeating-commands 'rmt--dummy-function-2))
     (should-not (seq-contains-p ryo-modal--non-repeating-commands 'rmt--dummy-function-3)))))

(ert-deftest rmt--ryo-modal-key--name-keyword ()
  (rmt--with-clean-ryo-modal-mode-keymap
   (ryo-modal-key "A" 'rmt--mock-function-1 :name "cooler-function-name")
   (should (= (seq-length (cdr ryo-modal-mode-map)) 1))
   (let ((bound-function (lookup-key ryo-modal-mode-map (kbd "A"))))
     (should (string-match "^ryo:.*:cooler-function-name$" (symbol-name bound-function)))
     (rmt--expect-mock-calls
      '("mock-1")
      (call-interactively bound-function)))))

(ert-deftest rmt--ryo-modal-key--exit-keyword ()
  (rmt--with-clean-ryo-modal-mode-keymap
   (ryo-modal-key "A" 'rmt--mock-function-1)
   (ryo-modal-key "S" 'rmt--mock-function-1 :exit t)
   (ryo-modal-key "D" 'rmt--mock-function-1 :exit nil)
   (let* ((a-function (lookup-key ryo-modal-mode-map (kbd "A")))
          (s-function (lookup-key ryo-modal-mode-map (kbd "S")))
          (d-function (lookup-key ryo-modal-mode-map (kbd "D"))))
     (rmt--with-ryo-modal-mode-enabled
      (should ryo-modal-mode)
      (rmt--expect-mock-calls
       '("mock-1")
       (call-interactively a-function))
      (should ryo-modal-mode))
     (rmt--with-ryo-modal-mode-enabled
      (should ryo-modal-mode)
      (rmt--expect-mock-calls
       '("mock-1")
       (call-interactively s-function))
      (should-not ryo-modal-mode))
     (rmt--with-ryo-modal-mode-enabled
      (should ryo-modal-mode)
      (rmt--expect-mock-calls
       '("mock-1")
       (call-interactively d-function))
      (should ryo-modal-mode)))))

(ert-deftest rmt--ryo-modal-key--then-keyword ()
  (rmt--with-clean-ryo-modal-mode-keymap
   (ryo-modal-key "A" 'rmt--mock-function-1)
   (ryo-modal-key "S" 'rmt--mock-function-1
                  :then '(rmt--mock-function-2))
   (ryo-modal-key "D" 'rmt--mock-function-1
                  :then '((lambda () (interactive) (rmt--mock-function-2))))
   (ryo-modal-key "F" 'rmt--mock-function-1
                  :then '(rmt--mock-function-2
                          rmt--mock-function-4
                          rmt--mock-function-3))
   (let* ((a-function (lookup-key ryo-modal-mode-map (kbd "A")))
          (s-function (lookup-key ryo-modal-mode-map (kbd "S")))
          (d-function (lookup-key ryo-modal-mode-map (kbd "D")))
          (f-function (lookup-key ryo-modal-mode-map (kbd "F"))))
     (rmt--expect-mock-calls
      '("mock-1")
      (call-interactively a-function))
     (rmt--expect-mock-calls
      '("mock-1" "mock-2")
      (call-interactively s-function))
     (rmt--expect-mock-calls
      '("mock-1" "mock-2")
      (call-interactively d-function))
     (rmt--expect-mock-calls
      '("mock-1" "mock-2" "mock-4" "mock-3")
      (call-interactively f-function)))))

(ert-deftest rmt--ryo-modal-key--first-keyword ()
  (rmt--with-clean-ryo-modal-mode-keymap
   (ryo-modal-key "A" 'rmt--mock-function-1)
   (ryo-modal-key "S" 'rmt--mock-function-1
                  :first '(rmt--mock-function-2))
   (ryo-modal-key "D" 'rmt--mock-function-1
                  :first '((lambda () (interactive) (rmt--mock-function-2))))
   (ryo-modal-key "F" 'rmt--mock-function-1
                  :first '(rmt--mock-function-2
                           rmt--mock-function-4
                           rmt--mock-function-3))
   (let* ((a-function (lookup-key ryo-modal-mode-map (kbd "A")))
          (s-function (lookup-key ryo-modal-mode-map (kbd "S")))
          (d-function (lookup-key ryo-modal-mode-map (kbd "D")))
          (f-function (lookup-key ryo-modal-mode-map (kbd "F"))))
     (rmt--expect-mock-calls
      '("mock-1")
      (call-interactively a-function))
     (rmt--expect-mock-calls
      '("mock-2" "mock-1")
      (call-interactively s-function))
     (rmt--expect-mock-calls
      '("mock-2" "mock-1")
      (call-interactively d-function))
     (rmt--expect-mock-calls
      '("mock-2" "mock-4" "mock-3" "mock-1")
      (call-interactively f-function)))))

(ert-deftest rmt--ryo-modal-key--read-keyword ()
  (rmt--with-clean-ryo-modal-mode-keymap
   (ryo-modal-key "A" 'beginning-of-buffer)
   (ryo-modal-key "S" 'beginning-of-buffer :read t)
   (ryo-modal-key "D" 'beginning-of-buffer :read nil)
   (let ((invoke-map (kbd "C-c ryo"))
         (a-key-presses (kbd "Abar RET"))
         (s-key-presses (kbd "Sbar RET"))
         (d-key-presses (kbd "Dbar RET")))
     (rmt--with-temporary-global-binding
      invoke-map
      ryo-modal-mode-map ; make ryo-modal-mode-map invokable
      (rmt--with-temp-buffer
       (insert "foo")
       (execute-kbd-macro a-key-presses)
       (should (equal (buffer-string) "fooAbar\n")))
      (rmt--with-temp-buffer
       (insert "foo")
       (execute-kbd-macro (vconcat invoke-map a-key-presses))
       (should (equal (buffer-string) "bar\nfoo")))
      (rmt--with-temp-buffer
       (insert "foo")
       (execute-kbd-macro s-key-presses)
       (should (equal (buffer-string) "fooSbar\n")))
      (rmt--with-temp-buffer
       (insert "foo")
       (execute-kbd-macro (vconcat invoke-map s-key-presses))
       (should (equal (buffer-string) "barfoo"))) ; <- notice lack of newline
      (rmt--with-temp-buffer
       (insert "foo")
       (execute-kbd-macro d-key-presses)
       (should (equal (buffer-string) "fooDbar\n")))
      (rmt--with-temp-buffer
       (insert "foo")
       (execute-kbd-macro (vconcat invoke-map d-key-presses))
       (should (equal (buffer-string) "bar\nfoo")))))))

(ert-deftest rmt--ryo-modal-key--mc-all-keyword ()
  (rmt--with-clean-ryo-modal-mode-keymap
   (should (equal mc/cmds-to-run-once nil))
   (should (equal mc/cmds-to-run-for-all nil))
   (ryo-modal-key "A" 'rmt--dummy-function-1)
   (ryo-modal-key "S" 'rmt--dummy-function-2 :mc-all t)
   (ryo-modal-key "D" 'rmt--dummy-function-3 :mc-all 0)
   (ryo-modal-key "F" 'rmt--dummy-function-4 :mc-all nil)
   (should (equal mc/cmds-to-run-once '(rmt--dummy-function-3)))
   (should (equal mc/cmds-to-run-for-all '(rmt--dummy-function-2)))))

(ert-deftest rmt--ryo-modal-command-then-ryo--switches-to-ryo-modal-mode ()
  (rmt--with-clean-keymap
   rmt--custom-map
   (ryo-modal-command-then-ryo "A" 'rmt--mock-function-1 rmt--custom-map)
   (let ((invoke-map (kbd "C-c custom")))
     (rmt--with-temporary-global-binding
      invoke-map
      rmt--custom-map
      (rmt--with-temp-buffer
       (should-not ryo-modal-mode)
       (rmt--expect-mock-calls
        '("mock-1")
        (execute-kbd-macro (vconcat invoke-map (kbd "A"))))
       (should ryo-modal-mode))))))

(ert-deftest rmt--ryo-modal-keys--several-keys ()
  (let ((expected-map (make-sparse-keymap)))
    (define-key expected-map (kbd "q") #'rmt--dummy-function-1)
    (define-key expected-map (kbd "w") #'rmt--dummy-function-2)
    (define-key expected-map (kbd "e a") #'rmt--dummy-function-3)
    (define-key expected-map (kbd "e s") #'rmt--dummy-function-4)
    (define-key expected-map (kbd "r d z") #'rmt--dummy-function-5)
    (define-key expected-map (kbd "r d x") #'rmt--dummy-function-6)
    (define-key expected-map (kbd "r f c") #'rmt--dummy-function-7)
    (define-key expected-map (kbd "r f v") #'rmt--dummy-function-8)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-keys
      ("q" rmt--dummy-function-1)
      ("w" rmt--dummy-function-2)
      ("e"
       (("a" rmt--dummy-function-3)
        ("s" rmt--dummy-function-4)))
      ("r"
       (("d"
         (("z" rmt--dummy-function-5)
          ("x" rmt--dummy-function-6)))
        ("f"
         (("c" rmt--dummy-function-7)
          ("v" rmt--dummy-function-8))))))
     (should (equal ryo-modal-mode-map expected-map)))))

(ert-deftest rmt--ryo-modal-keys--with-keywords ()
  (let ((lhs nil)
        (rhs nil))
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key "A" 'rmt--dummy-function-1)
     (ryo-modal-key "S" 'rmt--dummy-function-2
                    :first '(end-of-buffer)
                    :norepeat t)
     (ryo-modal-key "D" 'rmt--dummy-function-3
                    :first '(end-of-buffer)
                    :norepeat t)
     (ryo-modal-key "F" 'rmt--dummy-function-4
                    :then '(beginning-of-buffer end-of-line)
                    :norepeat nil
                    :exit t)
     (ryo-modal-key "G" 'rmt--dummy-function-5
                    :then '(beginning-of-buffer end-of-line)
                    :norepeat nil
                    :exit t)
     (setq lhs ryo-modal-mode-map))
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-keys
      ("A" rmt--dummy-function-1))
     (ryo-modal-keys
      (:first '(end-of-buffer) :norepeat t)
      ("S" rmt--dummy-function-2)
      ("D" rmt--dummy-function-3))
     (ryo-modal-keys
      (:then '(beginning-of-buffer end-of-line) :norepeat nil :exit t)
      ("F" rmt--dummy-function-4)
      ("G" rmt--dummy-function-5))
     (setq rhs ryo-modal-mode-map))
    (should (equal lhs rhs))))

(ert-deftest rmt--ryo-modal-key--keymap-as-target ()
  (let ((expected-map (make-sparse-keymap)))
    (define-key expected-map (kbd "Q") rmt--predefined-custom-map)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key "Q" 'rmt--predefined-custom-map)
     (should (equal ryo-modal-mode-map expected-map)))))

(ert-deftest rmt--ryo-modal-key--keymap-with-nested-keymaps-as-target ()
  (let ((custom-map-top (make-sparse-keymap))
        (custom-map-1 (make-sparse-keymap))
        (custom-map-2 (make-sparse-keymap))
        (expected-map (make-sparse-keymap)))
    (define-key custom-map-top (kbd "a") #'rmt--dummy-function-1)
    (define-key custom-map-top (kbd "s") #'rmt--dummy-function-2)
    (define-key custom-map-1 (kbd "q") #'rmt--dummy-function-3)
    (define-key custom-map-1 (kbd "w") #'rmt--dummy-function-4)
    (define-key custom-map-top (kbd "C-d") custom-map-1)
    (define-key custom-map-2 (kbd "M-z") #'rmt--dummy-function-5)
    (define-key custom-map-2 (kbd "M-y") #'rmt--dummy-function-6)
    (define-key custom-map-top (kbd "C-f") custom-map-2)
    (define-key expected-map (kbd "Q") custom-map-top)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key "Q" 'custom-map-top)
     (should (equal ryo-modal-mode-map expected-map)))))

(ert-deftest rmt--ryo-modal-key--keymap-with-nested-keymaps-as-target-and-keywords ()
  "Keymap is transformed so that every entry inherits features specified by keywords."
  (let ((custom-map-top (make-sparse-keymap))
        (custom-map-1 (make-sparse-keymap))
        (custom-map-2 (make-sparse-keymap))
        (lhs nil)
        (rhs nil))
    (define-key custom-map-top (kbd "a") #'rmt--dummy-function-1)
    (define-key custom-map-top (kbd "s") #'rmt--dummy-function-2)
    (define-key custom-map-1 (kbd "q") #'rmt--dummy-function-3)
    (define-key custom-map-1 (kbd "w") #'rmt--dummy-function-4)
    (define-key custom-map-top (kbd "C-d") custom-map-1)
    (define-key custom-map-2 (kbd "M-z") #'rmt--dummy-function-5)
    (define-key custom-map-2 (kbd "M-y") #'rmt--dummy-function-6)
    (define-key custom-map-top (kbd "C-f") custom-map-2)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key "Q a" 'rmt--dummy-function-1
                    :exit t
                    :norepeat t
                    :first '(end-of-buffer)
                    :then '(beginning-of-buffer))
     (ryo-modal-key "Q s" 'rmt--dummy-function-2
                    :exit t
                    :norepeat t
                    :first '(end-of-buffer)
                    :then '(beginning-of-buffer))
     (ryo-modal-key "Q C-d q" 'rmt--dummy-function-3
                    :exit t
                    :norepeat t
                    :first '(end-of-buffer)
                    :then '(beginning-of-buffer))
     (ryo-modal-key "Q C-d w" 'rmt--dummy-function-4
                    :exit t
                    :norepeat t
                    :first '(end-of-buffer)
                    :then '(beginning-of-buffer))
     (ryo-modal-key "Q C-f M-z" 'rmt--dummy-function-5
                    :exit t
                    :norepeat t
                    :first '(end-of-buffer)
                    :then '(beginning-of-buffer))
     (ryo-modal-key "Q C-f M-y" 'rmt--dummy-function-6
                    :exit t
                    :norepeat t
                    :first '(end-of-buffer)
                    :then '(beginning-of-buffer))
     (setq lhs ryo-modal-mode-map))
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key "Q" 'custom-map-top
                    :exit t
                    :norepeat t
                    :first '(end-of-buffer)
                    :then '(beginning-of-buffer))
     (setq rhs ryo-modal-mode-map))
    (should (equal lhs rhs))))

(ert-deftest rmt--ryo-modal-key--translate-binding-to-keymap ()
  (let ((expected-map (make-sparse-keymap)))
    (define-key expected-map (kbd "Q") rmt--predefined-custom-map)
    (rmt--with-temporary-global-binding
     (kbd "C-c custom")
     rmt--predefined-custom-map
     (rmt--with-clean-ryo-modal-mode-keymap
      (ryo-modal-key "Q" "C-c custom")
      (should (equal ryo-modal-mode-map expected-map))))))

(ert-deftest rmt--ryo-modal-key--translate-binding-to-keymap-which-has-associated-function ()
  "This use-case is similar to keymap bound to C-x 4 (`ctl-x-4-map') or `projectile-command-map'."
  (let ((expected-map (make-sparse-keymap)))
    (defalias 'rmt--custom-map-associated-function rmt--predefined-custom-map)
    (define-key expected-map (kbd "Q") rmt--predefined-custom-map)
    (rmt--with-temporary-global-binding
     (kbd "C-c custom")
     #'rmt--custom-map-associated-function
     (rmt--with-clean-ryo-modal-mode-keymap
      (ryo-modal-key "Q" "C-c custom")
      (should (equal ryo-modal-mode-map expected-map))))))

(ert-deftest rmt--ryo-modal-key--function-associated-to-keymap-as-target ()
  (let ((expected-map (make-sparse-keymap)))
    (defalias 'rmt--custom-map-associated-function rmt--predefined-custom-map)
    (define-key expected-map (kbd "Q") rmt--predefined-custom-map)
    (rmt--with-clean-ryo-modal-mode-keymap
     (ryo-modal-key "Q" 'rmt--custom-map-associated-function)
     (should (equal ryo-modal-mode-map expected-map)))))

(ert-deftest rmt--ryo-modal-keys--translate-binding-to-keymap ()
  (let ((expected-map (make-sparse-keymap)))
    (defalias 'rmt--custom-map-associated-function rmt--predefined-custom-map)
    (define-key expected-map (kbd "Q") rmt--predefined-custom-map)
    (rmt--with-temporary-global-binding
     (kbd "C-c custom")
     #'rmt--custom-map-associated-function
     (rmt--with-clean-ryo-modal-mode-keymap
      (ryo-modal-keys
       ("Q" "C-c custom"))
      (should (equal ryo-modal-mode-map expected-map))))))
