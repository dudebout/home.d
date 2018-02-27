;;; home.d-org.el ---- home.d org mode components -* lexical-binding: t; -*-

;;; Commentary:

;; Set of org-agenda functions.

;;; Code:

(require 'dash)
(require 'org)

(defun home.d/org-agenda-skip-if (condition)
  "`org-agenda-skip-function' to discard headings matching CONDITION."
  (when condition
    (save-excursion
      (or
       (outline-next-heading)
       (point-max)))))

(defun home.d/org-agenda-skip-not-next-action ()
  "Determines next actions as an `org-agenda-skip-function'."
  (home.d/org-agenda-skip-if (not (home.d/org-is-next-action))))

(defun home.d/org-is-next-action ()
  "Determines if the heading at point is a next action.
A heading is a next action if it is in a notdone TODO state, and
is unscheduled."
  (when (home.d/org-is-unscheduled-notdone)
    (let (found-earlier-action)
      (save-excursion
        (while (and
                (not found-earlier-action)
                (org-goto-sibling t))
          (when (home.d/org-is-unscheduled-notdone)
            (setq found-earlier-action t))))
    (not found-earlier-action))))

(defun home.d/org-is-unscheduled-notdone ()
  "Is heading at point unscheduled and notdone."
  (and
   (-non-nil (mapcar (apply-partially #'string= (org-get-todo-state)) org-not-done-keywords))
   (not (org-get-scheduled-time (point)))))

(defun home.d/org-agenda-project-prefix-format ()
  "Agenda prefix determining the project a TODO item is part of."
  (org-up-heading-safe)
  (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment))

(defun home.d/has-property (name value)
  (equal value (org-entry-get (point) name t)))

(defun home.d/has-tag (tag)
  (member tag (org-get-tags-at (point))))

(defun home.d/org-agenda-skip-if-property (name value)
  "`org-agenda-skip-function' to discard headings whose property NAME is VALUE."
  (home.d/org-agenda-skip-if (home.d/has-property name value)))

(defun home.d/org-agenda-skip-unless-property (name value)
  "`org-agenda-skip-function' to discard headings whose property NAME is not VALUE."
  (home.d/org-agenda-skip-if (not (home.d/has-property name value))))

(defun home.d/org-agenda-skip-if-tag (tag)
  "`org-agenda-skip-function' to discard headings tagged with TAG."
  (home.d/org-agenda-skip-if (home.d/has-tag tag)))

(provide 'home.d-org)

;;; home.d-org.el ends here
