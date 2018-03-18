(load "/home/ddb/.home.d/emacs/clocked-bucket.el")
(require 'home.d-org "/home/ddb/.home.d/emacs/home.d-org.el")

(defun quick-test ()
  (interactive)
  (find-file "/home/ddb/.home.d/emacs/clocked-bucket-tests.org")
  (total-buckets
    (list
     (clocked-bucket-bucket-create :name "bucket a (10% A + 90% B)"
                                   :allocations '((:A . 0.1) (:B . 0.9))
                                   :headline-filter (lambda () (home.d/has-property "bucket" "a")))
     (clocked-bucket-bucket-create :name "bucket b (A)"
                                   :allocations :A
                                   :headline-filter (lambda () (home.d/has-property "bucket" "b"))))
    '((:A . "Allocation A")
      (:B . "Allocation B")))
  (bury-buffer))
