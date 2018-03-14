;;; clocked-bucket.el --- FIXME -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Nicolas Dudebout

;; Author: Nicolas Dudebout <nicolas.dudebout@gmail.com>
;; Maintainer: Nicolas Dudebout <nicolas.dudebout@gmail.com>
;; Created: 23 Feb 2018
;; Modified: 13 Mar 2018
;; Version: 0.1
;; Package-Requires: FIXME
;; Keywords: FIXME
;; URL: https://github.com/dudebout/home.d FIXME

;;; Commentary:

;; FIXME
;;
;; 3 levels:
;;   + category
;;   + context
;;   + task

;; TODO
;;   + make a display toggle to go from percentages to minutes
;;   + compute the percentages in the bucket name automatically

;;; Code:

(eval-when-compile 'cl)
(require 'dash)
(require 'org)
(require 'org-clock)
(require 'seq)

(defvar clocked-bucket-buffer-name "*clocked bucket*")

;;; Utilities

(defun clocked-bucket-buffer ()
  "FIXME."
  (or
   (get-buffer clocked-bucket-buffer-name)
   (generate-new-buffer clocked-bucket-buffer-name)))

(defmacro push-end (newelt place)
  "Add NEWELT to the end of the list stored in the generalized variable PLACE.

This is morally equivalent to (setf PLACE (nconc PLACE (list
NEWELT))), except that PLACE is only evaluated once (after
NEWELT)."
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

;;; Data structures

(cl-defstruct (task (:constructor task-create)
                    (:copier nil))
  category context name clocked)

(cl-defstruct (context-tree (:constructor context-tree-create)
                            (:copier nil))
  name tasks clocked)

(cl-defstruct (task-tree (:constructor task-tree-create)
                         (:copier nil))
  category context-trees clocked)

(cl-defstruct (clocked (:constructor clocked-create)
                       (:copier nil))
  minutes percentage)

(cl-defstruct (bucket (:constructor bucket-create)
                      (:copier nil))
  name headline-filter allocations task-trees clocked)

;;; FIXME

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

;;; FIXME

(defun clocked-bucket-get-heading ()
  "FIXME."
  (substring-no-properties (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment)))


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

(defun clocked-bucket-get-clocked-tasks-if (headline-filter &optional tstart tend)
  "FIXME HEADLINE-FILTER TSTART TEND."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        (when
            (and
             (funcall headline-filter)
             (clocked-bucket-taskp))
          (let ((time (clocked-bucket-org-clock-sum-current-item tstart tend))
                (task (clocked-bucket-task-at-point)))
            (when (> time 0)
              (setf (task-clocked task) (clocked-create :minutes time))
              (push-end task result))))
        (outline-next-heading)))
    result))

(defun clocked-bucket-compute-task-trees (tasks)
  "FIXME TASKS."
  (let (task-trees)
    (dolist (task tasks task-trees)
      (let* ((category (task-category task))
             (task-tree-pos (seq-position task-trees category (lambda (tt c)
                                                                (equal (task-tree-category tt) c)))))
        (unless task-tree-pos
          (setq task-tree-pos (length task-trees))
          (push-end (task-tree-create :category category
                                      :context-trees nil
                                      :clocked (clocked-create :minutes 0))
                    task-trees))
        (let* ((task-tree (nth task-tree-pos task-trees))
               (context (task-context task))
               (context-trees (task-tree-context-trees task-tree))
               (context-tree-pos (seq-position context-trees context (lambda (ct c)
                                                                       (equal (context-tree-name ct) c)))))
          (unless context-tree-pos
            (setq context-tree-pos (length context-trees))
            (push-end (context-tree-create :name context
                                           :tasks ()
                                           :clocked (clocked-create :minutes 0))
                      (task-tree-context-trees (nth task-tree-pos task-trees))))
          (push-end task
                    (context-tree-tasks (nth context-tree-pos (task-tree-context-trees (nth task-tree-pos task-trees))))))))))

(defun clocked-bucket-propagate-clocked-minutes (task-tree)
  "FIXME TASK-TREE.  Return the total time."
  (let ((total-time 0))
    (dolist (context-tree (task-tree-context-trees task-tree) total-time)
      (dolist (task (context-tree-tasks context-tree))
        (let ((minutes (clocked-minutes (task-clocked task))))
          (cl-incf (clocked-minutes (context-tree-clocked context-tree)) minutes)
          (cl-incf (clocked-minutes (task-tree-clocked task-tree)) minutes)
          (cl-incf total-time minutes))))))

(defun clocked-bucket-compute-clocked-percentages (total-minutes task-tree)
  "FIXME TOTAL-MINUTES TASK-TREE."
  (message "%d" total-minutes)
  (dolist (context-tree (task-tree-context-trees task-tree))
    (dolist (task (context-tree-tasks context-tree))
      (setf (clocked-percentage (task-tree-clocked task-tree)) (/ (* 100.0 (clocked-minutes (task-tree-clocked task-tree))) total-minutes))
      (setf (clocked-percentage (context-tree-clocked context-tree)) (/ (* 100.0 (clocked-minutes (context-tree-clocked context-tree))) total-minutes))
      (setf (clocked-percentage (task-clocked task)) (/ (* 100.0 (clocked-minutes (task-clocked task))) total-minutes)))))

(defun clocked-bucket-display-task-tree (task-tree)
  "FIXME TASK-TREE."
  (let ((result))
    (setq result (concat result (format "|%s|%.2f%%|||\n"
                                        (task-tree-category task-tree)
                                        (clocked-percentage (task-tree-clocked task-tree)))))
    (dolist (context-tree (task-tree-context-trees task-tree) result)
      (setq result (concat result (format "|    %s||%.2f%%||\n"
                                          (context-tree-name context-tree)
                                          (clocked-percentage (context-tree-clocked context-tree)))))
      (dolist (task (context-tree-tasks context-tree))
        (setq result (concat result (format "|        %s|||%.2f%% (%s)|\n"
                                            (task-name task)
                                            (clocked-percentage (task-clocked task))
                                            (clocked-bucket-format-time (clocked-minutes (task-tree-clocked task-tree))))))))))

(defun clocked-bucket-display-task-trees (task-trees)
  "FIXME TASK-TREES."
  (concat "|-|\n" (apply #'concat (mapcar #'clocked-bucket-display-task-tree task-trees)) "|-|\n"))

(defun clocked-bucket-format-time (minutes)
  "FIXME MINUTES."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
  (format-seconds "%h:%02m" (* 60 minutes)))

(defun display-bucket (bucket)
  "FIXME BUCKET."
  (let* ((result (clocked-bucket-display-task-trees (bucket-task-trees bucket))))
    (with-current-buffer (clocked-bucket-buffer)
      (insert (format "* %s: %.2s%%\n" (bucket-name bucket) (clocked-percentage (bucket-clocked bucket))))
      (insert result)
      (org-table-align))
    (display-buffer (clocked-bucket-buffer))))

(defun total-buckets (buckets translations &optional tstart tend)
  "FIXME BUCKETS TRANSLATIONS TSTART TEND."
  (interactive)
  (with-current-buffer (clocked-bucket-buffer)
    (org-mode)
    (erase-buffer))

  (let ((total-minutes 0)
        allocations)
    (dolist (bucket buckets)
      (let* ((tasks (clocked-bucket-get-clocked-tasks-if (bucket-headline-filter bucket) tstart tend))
             (task-trees (clocked-bucket-compute-task-trees tasks))
             (minutes (-sum (mapcar #'clocked-bucket-propagate-clocked-minutes task-trees))))
        (setf (bucket-task-trees bucket) task-trees)
        (setf (bucket-clocked bucket) (clocked-create :minutes minutes))
        (cl-incf total-minutes minutes)))

    (dolist (bucket buckets)
      (mapc (apply-partially #'clocked-bucket-compute-clocked-percentages total-minutes) (bucket-task-trees bucket))
      (let ((percentage (/ (* 100.0 (clocked-minutes (bucket-clocked bucket))) total-minutes)))
        (setf (clocked-percentage (bucket-clocked bucket)) percentage)
        (let ((allocs (bucket-allocations bucket)))
          (unless (listp allocs)
            (setq allocs `((,allocs . 1))))
          (dolist (alloc allocs)
            (cl-incf (alist-get (car alloc) allocations 0) (* percentage (cdr alloc)))))
        (display-bucket bucket)))

    (with-current-buffer (clocked-bucket-buffer)
      (whitespace-cleanup)
      (goto-char (point-min))
      (insert "* Allocations\n")
      (dolist (translation translations)
        (insert (format "+ %s :: %.2f%%\n"  (cdr translation) (alist-get (car translation) allocations)))))))

(provide 'clocked-bucket)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; clocked-bucket.el ends here
