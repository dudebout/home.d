;;; gtb.el --- Getting Things Billed (GTB) -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Nicolas Dudebout

;; Author: Nicolas Dudebout <nicolas.dudebout@gmail.com>
;; Maintainer: Nicolas Dudebout <nicolas.dudebout@gmail.com>
;; Created: 23 Feb 2018
;; Modified: 28 Mar 2018
;; Version: 0.1
;; Package-Requires: (dash org seq)
;; Keywords: clock gtd org
;; URL: https://github.com/dudebout/gtb

;;; Commentary:

;; This package
;;
;;   #+PROPERTY: bucket_ALL a b
;;   * category I
;;   :PROPERTIES:
;;   :bucket:   a
;;   :END:
;;   ** context 1
;;   *** task A
;;   :LOGBOOK:
;;   CLOCK: [2018-02-23 Fri 12:00]--[2018-02-23 Fri 13:00] =>  1:00
;;   :END:
;;   *** task B
;;   :PROPERTIES:
;;   :bucket:   b
;;   :END:
;;   :LOGBOOK:
;;   CLOCK: [2018-02-23 Fri 13:00]--[2018-02-23 Fri 14:00] =>  1:00
;;   :END:
;;
;; groups things as follows
;;
;;   |------------------------------+-------+-------+-------+------|
;;   | bucket a                     | 50.0% |       |       |      |
;;   |------------------------------+-------+-------+-------+------|
;;   | category I                   | 50.0% |       |       |      |
;;   |     context 1                |       | 50.0% |       |      |
;;   |         task A               |       |       | 50.0% | 1:00 |
;;   |------------------------------+-------+-------+-------+------|
;;   | bucket b                     | 50.0% |       |       |      |
;;   |------------------------------+-------+-------+-------+------|
;;   | category I                   | 50.0% |       |       |      |
;;   |     context 1                |       | 50.0% |       |      |
;;   |         task B               |       |       | 50.0% | 1:00 |
;;   |------------------------------+-------+-------+-------+------|
;;
;; 3 levels:
;;   + category
;;   + context
;;   + task
;; And a notion of bucket
;;
;; TODO
;;   + make sure hitting "g" in the gtb buffer does refresh
;;     - store the filter invocation in a property
;;     - store the file info and/or use org-agenda files
;;   + remove all FIXMEs
;;   + add overlap detection

;;; Code:

(eval-when-compile 'cl)
(require 'dash)
(require 'org)
(require 'org-clock)
(require 'seq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom gtb-buffer-name "*gtb*"
  "The name of the output buffer."
  :type 'string
  :group 'gtb)

(defcustom gtb-percentage-fmt "%.1f%%"
  "Format string to display percentages."
  :type 'string
  :group 'gtb)

(defcustom gtb-indent-str "    "
  "String used to indent categories from contexts and tasks from contexts."
  :type 'string
  :group 'gtb)

(defcustom gtb-hide-categories nil
  "If not nil, do not display the categories, but only the contexts and tasks."
  :type 'boolean
  :group 'gtb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (gtb-task
               (:constructor gtb-task-create)
               (:copier nil))
  category context name clocked)

(cl-defstruct (gtb-context
               (:constructor gtb-context-create)
               (:copier nil))
  name tasks clocked)

(cl-defstruct (gtb-category
               (:constructor gtb-category-create)
               (:copier nil))
  name contexts clocked)

(cl-defstruct (gtb-clocked
               (:constructor gtb-clocked-create)
               (:copier nil))
  minutes percentage)

(cl-defstruct (gtb-bucket
               (:constructor gtb-bucket-create)
               (:copier nil))
  name headline-filter allocations categories clocked is-billable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro gtb--push-end (newelt place)
  "Add NEWELT to the end of the list stored in the generalized variable PLACE.

This is morally equivalent to (setf PLACE (nconc PLACE (list
NEWELT))), except that PLACE is only evaluated once (after
NEWELT).

This is a variation of the `push' macro."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extracting tasks from org file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gtb-taskp ()
  "FIXME."
  (eq 3 (org-element-property :level (org-element-at-point))))

(defun gtb-get-heading ()
  "FIXME."
  (substring-no-properties (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment)))

(defun gtb-task-at-point ()
  "FIXME."
  (save-excursion
    (let ((name
           (gtb-get-heading))
          (context
           (progn
             (outline-up-heading 1 t)
             (gtb-get-heading)))
          (category
           (progn
             (outline-up-heading 1 t)
             (gtb-get-heading))))
      (gtb-task-create :category category
                                  :context context
                                  :name name))))

(defun gtb-org-clock-sum-current-item (&optional tstart tend)
  "Return time, clocked on current item in total.
FIXME TSTART TEND"
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-clock-sum tstart tend)
      org-clock-file-total-minutes)))

(defun gtb-get-clocked-tasks-if (headline-filter &optional tstart tend)
  "FIXME HEADLINE-FILTER TSTART TEND."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        (when
            (and
             (funcall headline-filter)
             (gtb-taskp))
          (let ((time (gtb-org-clock-sum-current-item tstart tend))
                (task (gtb-task-at-point)))
            (when (> time 0)
              (setf (gtb-task-clocked task) (gtb-clocked-create :minutes time))
              (gtb--push-end task result))))
        (outline-next-heading)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Building up categories from tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gtb-compute-categories (tasks)
  "FIXME TASKS."
  (let (categories)
    (dolist (task tasks categories)
      (let* ((category (gtb-task-category task))
             (category-pos (seq-position categories category (lambda (tt c)
                                                                (equal (gtb-category-name tt) c)))))
        (unless category-pos
          (setq category-pos (length categories))
          (gtb--push-end (gtb-category-create :name category
                                                    :contexts nil
                                                    :clocked (gtb-clocked-create :minutes 0))
                    categories))
        (let* ((category (nth category-pos categories))
               (context (gtb-task-context task))
               (contexts (gtb-category-contexts category))
               (context-pos (seq-position contexts context (lambda (ct c)
                                                                       (equal (gtb-context-name ct) c)))))
          (unless context-pos
            (setq context-pos (length contexts))
            (gtb--push-end (gtb-context-create :name context
                                                     :tasks ()
                                                     :clocked (gtb-clocked-create :minutes 0))
                      (gtb-category-contexts (nth category-pos categories))))
          (gtb--push-end task
                    (gtb-context-tasks (nth context-pos (gtb-category-contexts (nth category-pos categories))))))))))

(defun gtb-propagate-clocked-minutes (category)
  "FIXME CATEGORY.  Return the total time."
  (let ((total-time 0))
    (dolist (context (gtb-category-contexts category) total-time)
      (dolist (task (gtb-context-tasks context))
        (let ((minutes (gtb-clocked-minutes (gtb-task-clocked task))))
          (cl-incf (gtb-clocked-minutes (gtb-context-clocked context)) minutes)
          (cl-incf (gtb-clocked-minutes (gtb-category-clocked category)) minutes)
          (cl-incf total-time minutes))))))

(defun gtb-compute-clocked-percentages (total-minutes category)
  "FIXME TOTAL-MINUTES CATEGORY."
  (dolist (context (gtb-category-contexts category))
    (dolist (task (gtb-context-tasks context))
      (let* ((task-pct (/ (* 100.0 (gtb-clocked-minutes (gtb-task-clocked task))) total-minutes))
             (context-pct (/ (* 100.0 (gtb-clocked-minutes (gtb-context-clocked context))) total-minutes))
             (category-pct (/ (* 100.0 (gtb-clocked-minutes (gtb-category-clocked category))) total-minutes)))
        (setf (gtb-clocked-percentage (gtb-task-clocked task)) task-pct)
        (setf (gtb-clocked-percentage (gtb-context-clocked context)) context-pct)
        (setf (gtb-clocked-percentage (gtb-category-clocked category)) category-pct)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gtb-buffer ()
  "Return the gtb output buffer.

Get the buffer named `gtb-buffer-name', and create it
if it does not exist."
  (or
   (get-buffer gtb-buffer-name)
   (generate-new-buffer gtb-buffer-name)))

(defun gtb-minutes-str (minutes)
  "FIXME MINUTES."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
  (format-seconds "%h:%02m" (* 60 minutes)))

(defun gtb-percentage-str (percentage)
  "FIXME PERCENTAGE."
  (format gtb-percentage-fmt percentage))

(defun gtb-assemble-fmt (&rest columns-fmt)
  "FIXME COLUMNS-FMT."
  (concat "|"
          (mapconcat 'identity columns-fmt "|")
          "|\n"))

(defun gtb-indent-str (num)
  "FIXME NUM."
  (concat (apply #'concat (make-list num gtb-indent-str)) "%s"))

(defun gtb-display-categories (categories)
  "FIXME CATEGORIES."
  (concat "|-|\n" (apply #'concat (mapcar #'gtb-display-category categories)) "|-|\n"))

(defun gtb-display-category (category)
  "FIXME CATEGORY."
  (let ((result)
        (category-fmt (gtb-assemble-fmt
                       "%s"
                       (gtb-indent-str 0)))
        (context-fmt (gtb-assemble-fmt
                      (gtb-indent-str (if gtb-hide-categories 0 1))
                      ""
                      "%s"))
        (task-fmt (gtb-assemble-fmt
                   (gtb-indent-str (if gtb-hide-categories 1 2))
                   ""
                   ""
                   "%s"
                   "%s")))


    (unless gtb-hide-categories
      (setq result (concat result (format category-fmt
                                          (gtb-category-name category)
                                          (gtb-percentage-str (gtb-clocked-percentage (gtb-category-clocked category)))))))
    (dolist (context (gtb-category-contexts category) result)
      (setq result (concat result (format context-fmt
                                          (gtb-context-name context)
                                          (gtb-percentage-str (gtb-clocked-percentage (gtb-context-clocked context))))))
      (dolist (task (gtb-context-tasks context))
        (setq result (concat result (format task-fmt
                                            (gtb-task-name task)
                                            (gtb-percentage-str (gtb-clocked-percentage (gtb-task-clocked task)))
                                            (gtb-minutes-str (gtb-clocked-minutes (gtb-task-clocked task))))))))))

(defun gtb-display-bucket (bucket)
  "FIXME BUCKET."
  (let* ((result (gtb-display-categories (gtb-bucket-categories bucket))))
    (with-current-buffer (gtb-buffer)
      (insert (format "|%s|%s|\n"
                      (gtb-bucket-name bucket)
                      (gtb-percentage-str (gtb-clocked-percentage (gtb-bucket-clocked bucket)))))
      (insert (format "|%s|\n"
                      (gtb-display-allocations (gtb-bucket-allocations bucket))))
      (insert result)
      (org-table-align))
    (display-buffer (gtb-buffer))))

(defun gtb-display-allocations (allocations)
  "FIXME ALLOCATIONS."
  (if (= 1 (length allocations))
      (format "%s" (caar allocations))
    (mapconcat (lambda (allocation) (format "%s: %.1f" (car allocation) (cdr allocation))) allocations ", ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIXME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gtb--assemble-bucket (name allocations headline-filter &optional is-billable)
  "FIXME NAME ALLOCATIONS HEADLINE-FILTER IS-BILLABLE."
  (gtb-bucket-create :name name
                     :allocations (gtb--normalize-allocations allocations)
                     :headline-filter headline-filter
                     :is-billable is-billable))

(defun gtb--normalize-allocations (allocations)
  "FIXME ALLOCATIONS."
  (let ((sum 0))
    (unless (listp allocations)
      (setq allocations `((,allocations . 1))))
    (dolist (allocation allocations)
      (let ((share (cdr allocation)))
        (when (<= share 0)
          (error "An allocation share has to be positive"))
        (cl-incf sum share)))
    (dolist (allocation allocations allocations)
      (let ((share (cdr allocation)))
        (setf (cdr allocation) (/ share (float sum)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gtb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun gtb (bucket-specs &optional tstart tend)
  "FIXME BUCKET-SPECS TSTART TEND."
  (interactive)
  (with-current-buffer (gtb-buffer)
    (setq buffer-read-only nil)
    (org-mode)
    (erase-buffer))

  (let ((buckets (mapcar (lambda (spec-list) (apply #'gtb--assemble-bucket spec-list)) bucket-specs))
        (billable-minutes 0)
        (non-billable-minutes 0)
        (total-minutes (org-clock-sum tstart tend))
        allocations)
    (dolist (bucket buckets)
      (let* ((tasks (gtb-get-clocked-tasks-if (gtb-bucket-headline-filter bucket) tstart tend))
             (categories (gtb-compute-categories tasks))
             (minutes (-sum (mapcar #'gtb-propagate-clocked-minutes categories))))
        (setf (gtb-bucket-categories bucket) categories)
        (setf (gtb-bucket-clocked bucket) (gtb-clocked-create :minutes minutes))
        (if (gtb-bucket-is-billable bucket)
            (cl-incf non-billable-minutes minutes)
          (cl-incf billable-minutes minutes))))
    (with-current-buffer (gtb-buffer)
      (insert (format "* billable time: %s\n" (gtb-minutes-str billable-minutes)))
      (insert "|-|\n"))
    (dolist (bucket buckets)
      (unless (gtb-bucket-is-billable bucket)
        (mapc (apply-partially #'gtb-compute-clocked-percentages billable-minutes) (gtb-bucket-categories bucket))
        (let ((percentage (/ (* 100.0 (gtb-clocked-minutes (gtb-bucket-clocked bucket))) billable-minutes)))
          (setf (gtb-clocked-percentage (gtb-bucket-clocked bucket)) percentage)
          (let ((allocs (gtb-bucket-allocations bucket)))
            (dolist (alloc allocs)
              (cl-incf (alist-get (car alloc) allocations 0) (* percentage (cdr alloc)))))
          (gtb-display-bucket bucket))))

    (with-current-buffer (gtb-buffer)
      (whitespace-cleanup)
      (goto-char (point-min))
      (insert "* billed\n")
      (dolist (allocation (cl-sort allocations #'string-lessp :key #'car))
        (insert (format "+ %s :: %.2f%%\n"  (car allocation) (cdr allocation))))
      (unless (= 0 non-billable-minutes)
        (goto-char (point-max))
        (insert (format "* non-billable time: %s\n" (gtb-minutes-str non-billable-minutes))))
      (let ((unaccounted-minutes (- total-minutes billable-minutes non-billable-minutes)))
        (unless (= 0 unaccounted-minutes)
          (goto-char (point-min))
          (insert (format "* ERROR unaccounted time: %s\n" (gtb-minutes-str unaccounted-minutes)))))
      (setq buffer-read-only t))))

(provide 'gtb)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; gtb.el ends here
