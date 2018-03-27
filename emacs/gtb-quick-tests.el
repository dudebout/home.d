(load "/home/ddb/.home.d/emacs/gtb.el")

(defun quick-test ()
  (interactive)
  (find-file "/home/ddb/.home.d/emacs/gtb-tests.org")
  (gtb-total-buckets
   '(("bucket a" ((project1 . 1) (project2 . 9)) (lambda () (gtb-has-bucket-property "a")))
     ("bucket b" project1 (lambda () (gtb-has-bucket-property "b")))
     ("bucket c" overhead (lambda () (gtb-has-bucket-property "c")) t)))
  (bury-buffer))
