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

(defmacro clocked-bucket--push-end (newelt place)
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

(defun clocked-bucket-has-bucket-property (bucket-name)
  "Return t if the entry at point has the bucket property BUCKET-NAME."
  (equal bucket-name (org-entry-get (point) "bucket" t)))


;;; Data structures

(cl-defstruct (clocked-bucket-task
               (:constructor clocked-bucket-task-create)
               (:copier nil))
  category context name clocked)

(cl-defstruct (clocked-bucket-context
               (:constructor clocked-bucket-context-create)
               (:copier nil))
  name tasks clocked)

(cl-defstruct (clocked-bucket-category
               (:constructor clocked-bucket-category-create)
               (:copier nil))
  name contexts clocked)

(cl-defstruct (clocked-bucket-clocked
               (:constructor clocked-bucket-clocked-create)
               (:copier nil))
  minutes percentage)

(cl-defstruct (clocked-bucket-bucket
               (:constructor clocked-bucket-bucket-create)
               (:copier nil))
  name headline-filter allocations categories clocked is-billable)

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
      (clocked-bucket-task-create :category category
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
              (setf (clocked-bucket-task-clocked task) (clocked-bucket-clocked-create :minutes time))
              (clocked-bucket--push-end task result))))
        (outline-next-heading)))
    result))

(defun clocked-bucket-compute-categories (tasks)
  "FIXME TASKS."
  (let (categories)
    (dolist (task tasks categories)
      (let* ((category (clocked-bucket-task-category task))
             (category-pos (seq-position categories category (lambda (tt c)
                                                                (equal (clocked-bucket-category-name tt) c)))))
        (unless category-pos
          (setq category-pos (length categories))
          (clocked-bucket--push-end (clocked-bucket-category-create :name category
                                                    :contexts nil
                                                    :clocked (clocked-bucket-clocked-create :minutes 0))
                    categories))
        (let* ((category (nth category-pos categories))
               (context (clocked-bucket-task-context task))
               (contexts (clocked-bucket-category-contexts category))
               (context-pos (seq-position contexts context (lambda (ct c)
                                                                       (equal (clocked-bucket-context-name ct) c)))))
          (unless context-pos
            (setq context-pos (length contexts))
            (clocked-bucket--push-end (clocked-bucket-context-create :name context
                                                     :tasks ()
                                                     :clocked (clocked-bucket-clocked-create :minutes 0))
                      (clocked-bucket-category-contexts (nth category-pos categories))))
          (clocked-bucket--push-end task
                    (clocked-bucket-context-tasks (nth context-pos (clocked-bucket-category-contexts (nth category-pos categories))))))))))

(defun clocked-bucket-propagate-clocked-minutes (category)
  "FIXME CATEGORY.  Return the total time."
  (let ((total-time 0))
    (dolist (context (clocked-bucket-category-contexts category) total-time)
      (dolist (task (clocked-bucket-context-tasks context))
        (let ((minutes (clocked-bucket-clocked-minutes (clocked-bucket-task-clocked task))))
          (cl-incf (clocked-bucket-clocked-minutes (clocked-bucket-context-clocked context)) minutes)
          (cl-incf (clocked-bucket-clocked-minutes (clocked-bucket-category-clocked category)) minutes)
          (cl-incf total-time minutes))))))

;;; The following needs to be generalized

(defun clocked-bucket-compute-clocked-percentages (total-minutes category)
  "FIXME TOTAL-MINUTES CATEGORY."
  (message "%d" total-minutes)
  (dolist (context (clocked-bucket-category-contexts category))
    (dolist (task (clocked-bucket-context-tasks context))
      (setf (clocked-bucket-clocked-percentage (clocked-bucket-category-clocked category)) (/ (* 100.0 (clocked-bucket-clocked-minutes (clocked-bucket-category-clocked category))) total-minutes))
      (setf (clocked-bucket-clocked-percentage (clocked-bucket-context-clocked context)) (/ (* 100.0 (clocked-bucket-clocked-minutes (clocked-bucket-context-clocked context))) total-minutes))
      (setf (clocked-bucket-clocked-percentage (clocked-bucket-task-clocked task)) (/ (* 100.0 (clocked-bucket-clocked-minutes (clocked-bucket-task-clocked task))) total-minutes)))))

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
                   "%-5s (%s)")))
    (setq result (concat result (format category-fmt
                                        (clocked-bucket-category-name category)
                                        (clocked-bucket-percentage-str (clocked-bucket-clocked-percentage (clocked-bucket-category-clocked category))))))
    (dolist (context (clocked-bucket-category-contexts category) result)
      (setq result (concat result (format context-fmt
                                          (clocked-bucket-context-name context)
                                          (clocked-bucket-percentage-str (clocked-bucket-clocked-percentage (clocked-bucket-context-clocked context))))))
      (dolist (task (clocked-bucket-context-tasks context))
        (setq result (concat result (format task-fmt
                                            (clocked-bucket-task-name task)
                                            (clocked-bucket-percentage-str (clocked-bucket-clocked-percentage (clocked-bucket-task-clocked task)))
                                            (clocked-bucket-minutes-str (clocked-bucket-clocked-minutes (clocked-bucket-task-clocked task))))))))))

(defun clocked-bucket-display-categories (categories)
  "FIXME CATEGORIES."
  (concat "|-|\n" (apply #'concat (mapcar #'clocked-bucket-display-category categories)) "|-|\n"))

(defun clocked-bucket-display-bucket (bucket)
  "FIXME BUCKET."
  (let* ((result (clocked-bucket-display-categories (clocked-bucket-bucket-categories bucket))))
    (with-current-buffer (clocked-bucket-buffer)
      (insert (format "|%s (%s): %s||||\n"
                      (clocked-bucket-bucket-name bucket)
                      (clocked-bucket-display-allocations (clocked-bucket-bucket-allocations bucket))
                      (clocked-bucket-percentage-str (clocked-bucket-clocked-percentage (clocked-bucket-bucket-clocked bucket)))))
      (insert result)
      (org-table-align))
    (display-buffer (clocked-bucket-buffer))))

(defun clocked-bucket-display-allocations (allocations)
  "FIXME ALLOCATIONS."
  (if (= 1 (length allocations))
      (format "%s" (caar allocations))
    (mapconcat (lambda (allocation) (format "%s: %.1f" (car allocation) (cdr allocation))) allocations ", ")))

(defun clocked-bucket-total-buckets (bucket-specs &optional tstart tend)
  "FIXME BUCKETS TSTART TEND."
  (interactive)
  (with-current-buffer (clocked-bucket-buffer)
    (org-mode)
    (erase-buffer))

  (let ((buckets (mapcar (lambda (spec-list) (apply #'clocked-bucket-assemble-bucket spec-list)) bucket-specs))
        (billable-minutes 0)
        (non-billable-minutes 0)
        (total-minutes (org-clock-sum tstart tend))
        allocations)
    (dolist (bucket buckets)
      (let* ((tasks (clocked-bucket-get-clocked-tasks-if (clocked-bucket-bucket-headline-filter bucket) tstart tend))
             (categories (clocked-bucket-compute-categories tasks))
             (minutes (-sum (mapcar #'clocked-bucket-propagate-clocked-minutes categories))))
        (setf (clocked-bucket-bucket-categories bucket) categories)
        (setf (clocked-bucket-bucket-clocked bucket) (clocked-bucket-clocked-create :minutes minutes))
        (if (clocked-bucket-bucket-is-billable bucket)
            (cl-incf non-billable-minutes minutes)
          (cl-incf billable-minutes minutes))))
    (with-current-buffer (clocked-bucket-buffer)
      (insert (format "* billable time: %s\n" (clocked-bucket-minutes-str billable-minutes)))
      (insert "|-|-|-|-|\n"))
    (dolist (bucket buckets)
      (unless (clocked-bucket-bucket-is-billable bucket)
        (mapc (apply-partially #'clocked-bucket-compute-clocked-percentages billable-minutes) (clocked-bucket-bucket-categories bucket))
        (let ((percentage (/ (* 100.0 (clocked-bucket-clocked-minutes (clocked-bucket-bucket-clocked bucket))) billable-minutes)))
          (setf (clocked-bucket-clocked-percentage (clocked-bucket-bucket-clocked bucket)) percentage)
          (let ((allocs (clocked-bucket-bucket-allocations bucket)))
            (dolist (alloc allocs)
              (cl-incf (alist-get (car alloc) allocations 0) (* percentage (cdr alloc)))))
          (clocked-bucket-display-bucket bucket))))

    (with-current-buffer (clocked-bucket-buffer)
      (whitespace-cleanup)
      (goto-char (point-min))
      (insert "* billed\n")
      (dolist (allocation (cl-sort allocations #'string-lessp :key #'car))
        (insert (format "+ %s :: %.2f%%\n"  (car allocation) (cdr allocation))))
      (unless (= 0 non-billable-minutes)
        (goto-char (point-max))
        (insert (format "* non-billable time: %s\n" (clocked-bucket-minutes-str non-billable-minutes))))
      (let ((unaccounted-minutes (- total-minutes billable-minutes non-billable-minutes)))
        (unless (= 0 unaccounted-minutes)
          (goto-char (point-min))
          (insert (format "* ERROR unaccounted time: %s\n" (clocked-bucket-minutes-str unaccounted-minutes))))))))

(defun clocked-bucket-assemble-bucket (name allocations headline-filter &optional is-billable)
  "FIXME NAME ALLOCATIONS HEADLINE-FILTER."
  (clocked-bucket-bucket-create :name name
                                :allocations (clocked-bucket-normalize-allocations allocations)
                                :headline-filter headline-filter
                                :is-billable is-billable))

(defun clocked-bucket-normalize-allocations (allocations)
  "FIXME ALLOCATIONS."
  (let ((sum 0))
    (unless (listp allocations)
      (setq allocations `((,allocations . 1))))
    (dolist (allocation allocations)
      (let ((share (cdr allocation)))
        (when (<= share 0)
          (error "An allocation share has to be positive"))
        (incf sum share)))
    (dolist (allocation allocations allocations)
      (let ((share (cdr allocation)))
        (setf (cdr allocation) (/ share (float sum)))))))

(provide 'clocked-bucket)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; clocked-bucket.el ends here
