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

(defvar clocked-bucket-percentage-fmt "%.1f%%")

(defvar clocked-bucket-indent-fmt "    ")

;;; Utilities

(defun clocked-bucket-buffer ()
  "Determine the clocked-bucket output buffer.

Get the buffer named `clocked-bucket-buffer-name', and create it
if it does not exist."
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

(cl-defstruct (context (:constructor context-create)
                            (:copier nil))
  name tasks clocked)

(cl-defstruct (category (:constructor category-create)
                         (:copier nil))
  name contexts clocked)

(cl-defstruct (clocked (:constructor clocked-create)
                       (:copier nil))
  minutes percentage)

(cl-defstruct (bucket (:constructor bucket-create)
                      (:copier nil))
  name headline-filter allocations categories clocked)

;;; Display

(defun clocked-bucket-minutes-str (minutes)
  "FIXME MINUTES."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
  (format-seconds "%h:%02m" (* 60 minutes)))

(defun clocked-bucket-percentage-str (percentage)
  "FIXME PERCENTAGE."
  (format clocked-bucket-percentage-fmt percentage))

;;; Identifying tasks in the org-mode file

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

(defun clocked-bucket-compute-categories (tasks)
  "FIXME TASKS."
  (let (categories)
    (dolist (task tasks categories)
      (let* ((category (task-category task))
             (category-pos (seq-position categories category (lambda (tt c)
                                                                (equal (category-name tt) c)))))
        (unless category-pos
          (setq category-pos (length categories))
          (push-end (category-create :name category
                                     :contexts nil
                                     :clocked (clocked-create :minutes 0))
                    categories))
        (let* ((category (nth category-pos categories))
               (context (task-context task))
               (contexts (category-contexts category))
               (context-pos (seq-position contexts context (lambda (ct c)
                                                                       (equal (context-name ct) c)))))
          (unless context-pos
            (setq context-pos (length contexts))
            (push-end (context-create :name context
                                      :tasks ()
                                      :clocked (clocked-create :minutes 0))
                      (category-contexts (nth category-pos categories))))
          (push-end task
                    (context-tasks (nth context-pos (category-contexts (nth category-pos categories))))))))))

(defun clocked-bucket-propagate-clocked-minutes (category)
  "FIXME CATEGORY.  Return the total time."
  (let ((total-time 0))
    (dolist (context (category-contexts category) total-time)
      (dolist (task (context-tasks context))
        (let ((minutes (clocked-minutes (task-clocked task))))
          (cl-incf (clocked-minutes (context-clocked context)) minutes)
          (cl-incf (clocked-minutes (category-clocked category)) minutes)
          (cl-incf total-time minutes))))))

;;; The following needs to be generalized

(defun clocked-bucket-compute-clocked-percentages (total-minutes category)
  "FIXME TOTAL-MINUTES CATEGORY."
  (message "%d" total-minutes)
  (dolist (context (category-contexts category))
    (dolist (task (context-tasks context))
      (setf (clocked-percentage (category-clocked category)) (/ (* 100.0 (clocked-minutes (category-clocked category))) total-minutes))
      (setf (clocked-percentage (context-clocked context)) (/ (* 100.0 (clocked-minutes (context-clocked context))) total-minutes))
      (setf (clocked-percentage (task-clocked task)) (/ (* 100.0 (clocked-minutes (task-clocked task))) total-minutes)))))

(defun clocked-bucket-assemble-fmt (&rest columns-fmt)
  "FIXME COLUMNS-FMT."
  (concat "|"
          (mapconcat 'identity columns-fmt "|")
          "|\n"))

(defun clocked-bucket-indent-fmt (num)
  "FIXME NUM."
  (concat (apply #'concat (make-list num clocked-bucket-indent-fmt)) "%s"))

(defun clocked-bucket-display-category (category)
  "FIXME CATEGORY."
  (let ((result)
        (category-fmt (clocked-bucket-assemble-fmt
                       "%s"
                       (clocked-bucket-indent-fmt 0)
                       ""
                       ""))
        (context-fmt (clocked-bucket-assemble-fmt
                      (clocked-bucket-indent-fmt 1)
                      ""
                      "%s"
                      ""))
        (task-fmt (clocked-bucket-assemble-fmt
                   (clocked-bucket-indent-fmt 2)
                   ""
                   ""
                   "%s (%s)")))
    (setq result (concat result (format category-fmt
                                        (category-name category)
                                        (clocked-bucket-percentage-str (clocked-percentage (category-clocked category))))))
    (dolist (context (category-contexts category) result)
      (setq result (concat result (format context-fmt
                                          (context-name context)
                                          (clocked-bucket-percentage-str (clocked-percentage (context-clocked context))))))
      (dolist (task (context-tasks context))
        (setq result (concat result (format task-fmt
                                            (task-name task)
                                            (clocked-bucket-percentage-str (clocked-percentage (task-clocked task)))
                                            (clocked-bucket-minutes-str (clocked-minutes (task-clocked task))))))))))

(defun clocked-bucket-display-categories (categories)
  "FIXME CATEGORIES."
  (concat "|-|\n" (apply #'concat (mapcar #'clocked-bucket-display-category categories)) "|-|\n"))

(defun display-bucket (bucket)
  "FIXME BUCKET."
  (let* ((result (clocked-bucket-display-categories (bucket-categories bucket))))
    (with-current-buffer (clocked-bucket-buffer)
      (insert (format "* %s: %s\n"
                      (bucket-name bucket)
                      (clocked-bucket-percentage-str (clocked-percentage (bucket-clocked bucket)))))
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
             (categories (clocked-bucket-compute-categories tasks))
             (minutes (-sum (mapcar #'clocked-bucket-propagate-clocked-minutes categories))))
        (setf (bucket-categories bucket) categories)
        (setf (bucket-clocked bucket) (clocked-create :minutes minutes))
        (cl-incf total-minutes minutes)))

    (dolist (bucket buckets)
      (mapc (apply-partially #'clocked-bucket-compute-clocked-percentages total-minutes) (bucket-categories bucket))
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
