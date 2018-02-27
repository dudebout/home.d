;;; clocked-bucket-tests.el --- Tests for clocked-bucket.el

;;; Commentary:

;;; Code:

(require 'ert)
(require 'clocked-bucket)

;; temporary
(require 'home.d-org)

(ert-deftest tautology ()
  (should t)
  (should-not nil))

(ert-deftest my-test ()
  (find-file "./clocked-bucket-tests.org")
  (clocked-bucket-get-clocked-tasks-if (lambda () (home.d/has-property "bucket" "a")))
  (clocked-bucket-get-clocked-tasks-if (lambda () (home.d/has-property "bucket" "b"))))

;;; clocked-bucket-tests.el ends here
