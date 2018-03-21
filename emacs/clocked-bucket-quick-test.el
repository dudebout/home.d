(load "/home/ddb/.home.d/emacs/clocked-bucket.el")

(defun quick-test ()
  (interactive)
  (find-file "/home/ddb/.home.d/emacs/clocked-bucket-tests.org")
  (clocked-bucket-total-buckets
   '(("bucket a" ((Project1 . 1) (Project2 . 9)) (lambda () (clocked-bucket-has-bucket-property "a")))
     ("bucket b" Project1 (lambda () (clocked-bucket-has-bucket-property "b")))
     ("bucket c" Overhead (lambda () (clocked-bucket-has-bucket-property "c")) t)))
  (bury-buffer))
