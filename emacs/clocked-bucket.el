;;; clocked-bucket.el --- FIXME -*- lexical-binding: t; -*-

;;; Commentary:

;; FIXME

;;; Code:

(eval-when-compile 'cl)
(require 'seq)
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

(cl-defstruct (context-tree (:constructor context-tree-create)
                            (:copier nil))
  name tasks)

(cl-defstruct (task-tree (:constructor task-tree-create)
                         (:copier nil))
  category context-trees)

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
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        (when
            (and
             (funcall headline-filter)
             (clocked-bucket-taskp))
          (let ((time (clocked-bucket-org-clock-sum-current-item))
                (task (clocked-bucket-task-at-point)))
            (push-end `(,task . ,time) result)))
        (outline-next-heading)))
    result))

(defun clocked-bucket-get-clocked-tasks-in-buckets (headline-filters)
  "FIXME HEADLINE-FILTERS."
  (mapcar #'clocked-bucket-get-clocked-tasks-if headline-filters))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
;; (format-seconds "%h:%02m" (* 60 (* 60 25)))

;; (defun clocked-bucket-get-clocked-tasks-in-bucket (bucket)
;;   "FIXME BUCKET."
;;   (clocked-bucket-get-clocked-tasks-if
;;    (lambda ()
;;      (clocked-bucket-has-property "bucket" bucket))))

;; (defun clocked-bucket-display-tasks (tasks)
;;   "FIXME TASKS.")

(defun clocked-bucket-display-tasks (tasks)
  "FIXME TASKS."
  (let ((task (car tasks)))
    (format "%s\n  %s\n    %s"
            (task-category task)
            (task-context task)
            (task-name task))))

(defmacro push-end (newelt place)
  "Add NEWELT to the list stored in the generalized variable PLACE.
This is morally equivalent to (setf PLACE (nconc PLACE (list NEWELT))),
except that PLACE is only evaluated once (after NEWELT)."
  (declare (debug (form gv-place)))
  (if (symbolp place)
      ;; Important special case, to avoid triggering GV too early in
      ;; the bootstrap.
      (list 'setq place
            (list 'nconc place (list 'list newelt)))
    (require 'macroexp)
    (macroexp-let2 macroexp-copyable-p v newelt
      (gv-letplace (getter setter) place
        (funcall setter `(nconc ,getter (list ,v)))))))

(defun clocked-bucket-compute-task-trees (tasks)
  "FIXME TASKS."
  (let (task-trees)
    ;; FIXME fix the order in which the tasks are added
    (dolist (task tasks task-trees)
      (let* ((category (task-category task))
             (task-tree-pos (seq-position task-trees category (lambda (tt c) (equal (task-tree-category tt) c)))))
        (unless task-tree-pos
          (setq task-tree-pos (length task-trees))
          (push-end (task-tree-create :category category :context-trees nil) task-trees))
        (let* ((task-tree (nth task-tree-pos task-trees))
               (context (task-context task))
               (context-trees (task-tree-context-trees task-tree))
               (context-tree-pos (seq-position context-trees context (lambda (ct c) (equal (context-tree-name ct) c)))))
          (unless context-tree-pos
            (setq context-tree-pos (length context-trees))
            (push-end (context-tree-create :name context :tasks ()) (task-tree-context-trees (nth task-tree-pos task-trees))))
          (push-end task (context-tree-tasks (nth context-tree-pos (task-tree-context-trees (nth task-tree-pos task-trees))))))))))

(defun clocked-bucket-display-task-tree (task-tree)
  "FIXME TASK-TREE."
  (let ((result ""))
    (setq result (concat result (format "%s\n" (task-tree-category task-tree))))
    (dolist (context-tree (task-tree-context-trees task-tree) result)
      (setq result (concat result (format "  %s\n" (context-tree-name context-tree))))
      (dolist (task (context-tree-tasks context-tree))
        (setq result (concat result (format "    %s\n" (task-name task))))))))

(defun clocked-bucket-display-task-trees (task-trees)
  "FIXME TASK-TREES."
  (apply #'concat (mapcar #'clocked-bucket-display-task-tree task-trees)))

(provide 'clocked-bucket)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; clocked-bucket.el ends here
