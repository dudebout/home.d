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
  (clocked-bucket-get-clocked-tasks-if (lambda () (home.d/has-property "bucket" "b")))
  (clocked-bucket-get-clocked-tasks-in-buckets '((lambda () (home.d/has-property "bucket" "a"))
                                                 (lambda () (home.d/has-property "bucket" "b")))))

(ert-deftest my-test2 ()
  (find-file "./clocked-bucket-tests.org")
  (let ((result "category I\n  context 1\n    task A\n    task B\n  context 2\n    task A\n")
        (tasks (mapcar #'car (clocked-bucket-get-clocked-tasks-if (lambda () (home.d/has-property "bucket" "a"))))))
    (should (equal result (clocked-bucket-display-task-trees (clocked-bucket-compute-task-trees tasks))))))

(ert-deftest display-test ()
  (let ((t1 (task-create :category "category 1"
                         :context "context 1"
                         :name "task 1")))
    (should (equal "category 1\n  context 1\n    task 1" (clocked-bucket-display-tasks (list t1))))))

(ert-deftest task-trees-test ()
  (let* ((t1 (task-create :category "category 1"
                          :context "context 1"
                          :name "task 1"))
         (t2 (task-create :category "category 1"
                          :context "context 2"
                          :name "task 1"))
         (t3 (task-create :category "category 1"
                          :context "context 1"
                          :name "task 2"))
         (l (list t1 t2 t3))
         (result "category 1\n  context 1\n    task 1\n    task 2\n  context 2\n    task 1\n"))
    (should (equal result (clocked-bucket-display-task-trees (clocked-bucket-compute-task-trees l))))))

;;; clocked-bucket-tests.el ends here
