(load "/home/ddb/.home.d/emacs/clocked-bucket.el")

(defun quick-test ()
  (interactive)
  (find-file "/home/ddb/.home.d/emacs/clocked-bucket-tests.org")
  (clocked-bucket-total-buckets
   '(("bucket a" ((:A . 0.1) (:B . 0.9)) (lambda () (clocked-bucket-has-bucket-property "a")))
     ("bucket b" :A (lambda () (clocked-bucket-has-bucket-property "b")) t))
   '((:A . "Allocation A")
     (:B . "Allocation B")))
  (bury-buffer))
