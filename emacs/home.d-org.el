(defun home.d/org-agenda-skip-not-next-action ()
  (unless (home.d/org-is-next-action)
    (save-excursion
      (outline-next-heading)
      (point))))

(defun home.d/org-is-next-action ()
  ;; For the current heading to be a next action, it needs:
  ;;   + to be an unscheduled TODO item
  ;;   + that there are no previous sibling like it
  (when (home.d/org-is-unscheduled-todo)
    (let (found-earlier-action)
      (save-excursion
        (while (and
                (not found-earlier-action)
                (org-goto-sibling t))
          (when (home.d/org-is-unscheduled-todo)
            (setq found-earlier-action t))))
    (not found-earlier-action))))

(defun home.d/org-is-unscheduled-todo ()
  (and
   (string= "TODO" (org-get-todo-state))
   (not (org-get-scheduled-time (point)))))

(defun home.d/org-agenda-prefix-format ()
  (org-up-heading-safe)
  (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment))

(provide 'home.d-org)
