(load "/home/ddb/.home.d/emacs/clocked-bucket.el")

(defun quick-test ()
  (interactive)
  (find-file "/home/ddb/.home.d/emacs/clocked-bucket-tests.org")
  (total-buckets
    (list
     (clocked-bucket-bucket-create :name "bucket a (10% A + 90% B)"
                                   :allocations '((:A . 0.1) (:B . 0.9))
                                   :headline-filter (lambda () (clocked-bucket-has-bucket-property "a"))
                                   :is-overhead nil)
     (clocked-bucket-bucket-create :name "bucket b (A)"
                                   :allocations :A
                                   :headline-filter (lambda () (clocked-bucket-has-bucket-property "b"))
                                   :is-overhead t))
    '((:A . "Allocation A")
      (:B . "Allocation B")))
  (bury-buffer))
