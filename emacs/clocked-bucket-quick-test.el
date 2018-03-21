(load "/home/ddb/.home.d/emacs/clocked-bucket.el")

(defun quick-test ()
  (interactive)
  (find-file "/home/ddb/.home.d/emacs/clocked-bucket-tests.org")
  (clocked-bucket-total-buckets
   '(("bucket a" ((project1 . 1) (project2 . 9)) (lambda () (clocked-bucket-has-bucket-property "a")))
     ("bucket b" project1 (lambda () (clocked-bucket-has-bucket-property "b")))
     ("bucket c" overhead (lambda () (clocked-bucket-has-bucket-property "c")) t)))
  (bury-buffer))
