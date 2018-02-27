;;; home.d-org-tests.el --- Tests for home.d-org.el

;;; Commentary:

;;; Code:

(require 'ert)
(require 'home.d-org)

(ert-deftest tautology ()
  (should t)
  (should-not nil))

(ert-deftest my-test ()
  (find-file "./home.d-org-tests.org")
  (home.d/get-clocked-tasks-if (lambda () (home.d/has-property "bucket" "a")))
  (home.d/get-clocked-tasks-if (lambda () (home.d/has-property "bucket" "b"))))

;;; home.d-org-tests.el ends here
