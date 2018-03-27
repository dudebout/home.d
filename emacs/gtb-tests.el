;;; gtb-tests.el --- Tests for gtb.el

;;; Commentary:

;;; Code:

(require 'ert)
(require 'gtb)

;; temporary
(require 'home.d-org)

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
    (should (equal result (gtb-display-task-trees (gtb-compute-task-trees l))))))

(ert-deftest end-to-end-test ()
  (find-file "./gtb-tests.org")
  (let ((result "category I\n  context 1\n    task A\n    task B\n  context 2\n    task A\n")
        (tasks (mapcar #'car (gtb-get-clocked-tasks-if (lambda () (home.d/has-property "bucket" "a"))))))
    (should (equal result (gtb-display-task-trees (gtb-compute-task-trees tasks)))))
  (let ((result "category I\n  context 2\n    task B\ncategory II\n  context 1\n    task A\n")
        (tasks (mapcar #'car (gtb-get-clocked-tasks-if (lambda () (home.d/has-property "bucket" "b"))))))
    (should (equal result (gtb-display-task-trees (gtb-compute-task-trees tasks))))))

;;; gtb-tests.el ends here
