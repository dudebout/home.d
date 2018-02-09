;;; homed-org.el ---- home.d org mode components -* lexical-binding: t; -*-

;;; Commentary:
;;
;; Set of org-agenda functions.
;;
;;; Code:


(require 'dash)
(require 'org)

(defun home.d/org-agenda-skip-not-next-action ()
  "Determines next actions as an org-agenda-skip-function."
  (unless (home.d/org-is-next-action)
    (save-excursion
      (outline-next-heading)
      (point))))

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

(provide 'home.d-org)

;;; home.d-org.el ends here
