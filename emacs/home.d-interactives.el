(fset 'home.d/gnus-follow-gmane-link
           [?h ?\M-> S-iso-lefttab return ?h])

(defun home.d/display-org-agenda (&optional arg)
  "Display the org agenda."
  (interactive "P")
  (let ((org-agenda-window-setup (if arg
                                     'other-window
                                   'current-window)))
    (org-agenda-list)))

(defun home.d/create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun home.d/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun home.d/sudo-edit (&optional other-file)
  "Edit the current file (or another if prefix given) with root privileges by using sudo."
  (interactive "P")
  (if (or other-file
          (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun home.d/find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(defun home.d/unfill-paragraph ()
  "Unfill current paragraph or selected region."
  (interactive)
  (let ((fill-paragraph-function nil)
        (fill-column (point-max)))
    (if (use-region-p)
        (fill-paragraph nil t)
      (fill-paragraph nil))))

(defun home.d/delete-current-buffer-and-delete-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun home.d/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun home.d/dired-toggle-show-all ()
  "Toggle the -A flag for ls in dired."
  (interactive)
  (dired-sort-other
   (if (equal (substring dired-actual-switches -1) "A")
       (substring dired-actual-switches 0 -1)
     (concat dired-actual-switches "A"))))

(defun home.d/dired-open-file (arg)
  "Abuse org-open-file to open a file at point externally."
  (interactive "P")
  (require 'org)
  (require 'ffap)
  (org-open-file (or (ffap-guesser)
                     (error "No filename at point")) arg))

(defun home.d/toggle-selective-display (&optional column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(defun home.d/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (require 'linum)
  (if linum-mode
      (call-interactively 'goto-line)
    (unwind-protect
        (progn
          (linum-mode 1)
          (call-interactively 'goto-line))
      (linum-mode -1))))

(defun home.d/rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun home.d/beginning-of-line-or-indentation ()
  "Move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(defun home.d/start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

(defun home.d/visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (home.d/start-or-switch-to (lambda ()
                         (ansi-term (getenv "SHELL")))
                      "*ansi-term*"))

(defun home.d/visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (home.d/start-or-switch-to 'ielm "*ielm*"))

(defun home.d/describe-symbol-at-point-in-popup ()
  "Show help for the symbol at point in a popup window."
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (description (save-window-excursion
                        (with-temp-buffer
                          (help-mode)
                          (help-xref-interned thing)
                          (buffer-string)))))
    (popup-tip description
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(defun home.d/toggle-trailing-whitespace-cleanup ()
  "Toggle the inclustion of 'trailing in whitespace-style."
  (interactive)
  (require 'whitespace)
  (make-local-variable 'whitespace-style)
  (if (member 'trailing whitespace-style)
      (setq whitespace-style (delete 'trailing whitespace-style))
    (add-to-list 'whitespace-style 'trailing)))

(provide 'home.d-interactives)
