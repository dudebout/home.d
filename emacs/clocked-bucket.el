;;; clocked-bucket.el ---- FIXME

;;; Commentary:

;; FIXME

;;; Code:

(require 'cl)
(require 'org)
(require 'org-clock)

(defun clocked-bucket-at-level (level)
  "FIXME LEVEL."
  (eq level (org-element-property :level (org-element-at-point))))

(defun clocked-bucket-categoryp ()
  "FIXME."
  (clocked-bucket-at-level 1))

(defun clocked-bucket-contextp ()
  "FIXME."
  (clocked-bucket-at-level 2))

(defun clocked-bucket-taskp ()
  "FIXME."
  (clocked-bucket-at-level 3))

(defun clocked-bucket-get-heading ()
  "FIXME."
  (substring-no-properties (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment)))

(cl-defstruct (task (:constructor task-create)
                    (:copier nil))
  category context name)

(defun clocked-bucket-task-at-point ()
  "FIXME."
  (save-excursion
    (let ((name
           (clocked-bucket-get-heading))
          (context
           (progn
             (outline-up-heading 1 t)
             (clocked-bucket-get-heading)))
          (category
           (progn
             (outline-up-heading 1 t)
             (clocked-bucket-get-heading))))
      (task-create :category category
                   :context context
                   :name name))))

(defun clocked-bucket-org-clock-sum-current-item (&optional tstart tend)
  "Return time, clocked on current item in total.
FIXME TSTART TEND"
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-clock-sum tstart tend)
      org-clock-file-total-minutes)))

(defun clocked-bucket-get-clocked-tasks-if (headline-filter)
  "FIXME HEADLINE-FILTER."
  (let ((result ()))
    (save-excursion
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        (when
            (and
             (funcall headline-filter)
             (clocked-bucket-taskp))
          (let ((time (clocked-bucket-org-clock-sum-current-item))
                (task (clocked-bucket-task-at-point)))
            (add-to-list 'result `(,task . ,time))
            (message (format "%s %d" task time))))
        (outline-next-heading)))
    result))

;; (format-seconds "%h:%02m" (* 60 (* 60 25)))

;; (defun clocked-bucket-get-clocked-tasks-in-bucket (bucket)
;;   "FIXME BUCKET."
;;   (clocked-bucket-get-clocked-tasks-if
;;    (lambda ()
;;      (clocked-bucket-has-property "bucket" bucket))))

;; (defun clocked-bucket-display-tasks (tasks)
;;   "FIXME TASKS.")

(provide 'clocked-bucket)

;; Local Variables:
;; indent-tabs-mode: nil
;; lexical-binding: t
;; End:

;;; clocked-bucket.el ends here
