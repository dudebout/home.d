(package-initialize)

(let ((pre-init (concat (getenv "HOME_D") "/profile/emacs/pre-init.el")))
  (when (file-exists-p pre-init)
    (load-file pre-init)))

(add-to-list 'load-path (concat (getenv "HOME_D") "/emacs"))

(use-package home.d-interactives)
(use-package home.d-org)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-auto-revert-mode t)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      make-backup-files nil
      visible-bell t
      column-number-mode t
      size-indication-mode t
      vc-follow-symlinks t
      disabled-command-function nil)

(setq-default fill-column 80
              indent-tabs-mode nil
              require-final-newline t)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(defun tmux-default-directory ()
  (interactive)
  (call-process "ct" nil nil nil default-directory))

(bind-key "C-x M-w" 'home.d/rename-current-buffer-file)
(bind-key "C-x M-k" 'home.d/delete-current-buffer-and-delete-file)
(bind-key "M-g M-g" 'home.d/goto-line-with-feedback)
(bind-key "C-c r" 'replace-string)
;; Make sure that the last letter of the keybinding does not
;; correspond to the letter of the XMonad binding called by ct, else
;; xdotool key does not seem to be called properly / there is an
;; overlap problem. That problem can be fixed with a sleep (not ideal)
;;
;; If using "C-c t" and "Super+t", you would get an error saying that "C-S-t is not defined"
(bind-key "C-c d" 'tmux-default-directory)

;; Not sure that this is such a good idea anyway
;; When this is turned on, we get a message about the latest loaded module instead...
;; (defmacro inhibit-startup-echo-area-message ()
;;   (list 'setq 'inhibit-startup-echo-area-message (getenv "USER")))
;; (inhibit-startup-echo-area-message)


(setq use-package-verbose t)
(require 'use-package)

(use-package ace-window
  :bind ("M-o" . ace-window)
  :init (progn
          ;; colemak home row, minus n, and o which are reserved
          ;; should consider ordering them by finger strength
          (setq aw-keys '(?a ?r ?s ?t ?d ?h ?e ?i)
                aw-dispatch-always t)
          (ace-window-display-mode)))

(use-package avy
  :bind (("C-=" . avy-goto-char-2)
         ("M-g g" . avy-goto-line)
         ("C-c W" . avy-org-refile-as-child))
  :init (progn
           ;; colemak home row
          (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))
          ;; setup C-' in isearch
          (avy-setup-default)))

(use-package company
  :init (global-company-mode))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-h b" . counsel-descbinds)
         ("C-x C-f" . counsel-find-file)
         ("C-s" . counsel-grep-or-swiper)
         ("C-c s f" . counsel-git)
         ("C-c s i" . counsel-git-grep)
         ("C-c s g" . rgrep)
         ("C-c s r" . counsel-rg)
         ("C-c s s" . counsel-ag)
         ("C-c s z" . zrgrep)))

(use-package counsel-projectile
  :init (counsel-projectile-mode))

;; (use-package dante
;;   :init  (progn
;;            (put 'dante-project-root 'safe-local-variable #'stringp)
;;            (add-hook 'haskell-mode-hook 'dante-mode)))
;;
;; .dir-locals.el example
;; ((haskell-mode . ((dante-project-root . "/codemill/dudebout/repos/arc-systems/"))))

(use-package ediff
  :defer t
  :init (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package elisp-slime-nav
  :defer t
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
          (add-hook 'c-mode-hook 'elisp-slime-nav-mode)))

(use-package fill-column-indicator)
  ;; :config
  ;; (define-globalized-minor-mode ddb/global-fci-mode fci-mode turn-on-fci-mode)
  ;; (ddb/global-fci-mode 1)

;; TODO integrate flycheck-color-mode-line and flycheck-pos-tip
(use-package flycheck
  :defer t
  :init (use-package flycheck-haskell
          :defer t
          :init (progn
                  (add-hook 'haskell-mode-hook 'flycheck-mode)
                  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
                  (global-flycheck-mode))))

;;; https://emacs.stackexchange.com/questions/7281/how-to-modify-face-for-a-specific-buffer
;;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html
(defun ddb/switch-mode-line-background-on-god-mode ()
  (if god-local-mode
      (progn
        (setq ddb/mode-line-relative-cookie
              (face-remap-add-relative 'mode-line `((:background ,(zenburn-with-color-variables zenburn-red-4)))))
        (setq ddb/mode-line-inactive-relative-cookie
              (face-remap-add-relative 'mode-line-inactive `((:background ,(zenburn-with-color-variables zenburn-red-3))))))
    (progn
      (face-remap-remove-relative ddb/mode-line-relative-cookie)
      (face-remap-remove-relative ddb/mode-line-inactive-relative-cookie))))

(use-package god-mode
  :init (progn
          (global-set-key (kbd "<escape>") 'god-local-mode)
          (eval-after-load 'zenburn-theme
            '(progn
              (add-hook 'god-mode-enabled-hook 'ddb/switch-mode-line-background-on-god-mode)
              (add-hook 'god-mode-disabled-hook 'ddb/switch-mode-line-background-on-god-mode)))))

(use-package git-gutter
  :init (global-git-gutter-mode))

(use-package haskell-mode
  :interpreter ("stack" . haskell-mode)
  :init (progn
          (setq haskell-stylish-on-save t)
          (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
          ;; dante has conflicting install instructions with
          ;; haskell-mode. Ensure the haskell-mode instructions are
          ;; followed when dante is not installed.
          (unless (locate-library "dante")
            (add-hook 'haskell-mode-hook 'interactive-haskell-mode))))

(defvar ddb/hs-state nil
  "Current state of hideshow ddb/hs-toggle-all.")

  ;;;###autoload
(defun ddb/hs-toggle-all ()
  "Toggle hideshow all."
    (interactive)
    (setq ddb/hs-state (not ddb/hs-state))
    (if ddb/hs-state
        (hs-hide-all)
      (hs-show-all)))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h o" . helpful-symbol)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h C" . helpful-command)))

(use-package hideshow
  :commands hs-minor-mode
  :init (add-hook 'emacs-lisp-mode-hook #'hs-minor-mode)
  ;; could be bound specifically for the emacs-lisp mode map
  :bind ("<C-tab>" . ddb/hs-toggle-all))

(use-package hydra)

(use-package ibuffer
  :bind ("C-x C-b" . 'ibuffer))

(use-package ivy
  :bind ("C-x b" . ivy-switch-buffer)
  :init (setq ivy-use-virtual-buffers t))

(use-package macrostep
  :bind ("C-c e" . macrostep-expand))

(use-package magit
  :bind (("C-c i" . magit-status)
         ("C-c f" . magit-file-popup))
  :init (progn
          (require 'helm-mode)
          (setq magit-delete-by-moving-to-trash nil
                magit-diff-refine-hunk 'all
                magit-completing-read-function 'helm--completing-read-default)))

(use-package menu-bar
  :bind ("C-c m" . menu-bar-mode))

(use-package org
  :defer t
  :init
  (setq org-log-into-drawer t
        org-startup-indented t
        org-use-property-inheritance t
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil))
        org-todo-keywords '((type "TODO(t!)"
                                  "WAITING(w@/@)"
                                  "DELEGATED(D@/!)"
                                  "READ(r)"
                                  "WATCH(w)"
                                  "|"
                                  "DONE(d!)"
                                  "CANCELED(c@)"))))

(use-package org-agenda
  :bind ("C-c a" . org-agenda)
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-files (or org-agenda-files
                             (list org-directory))
        org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3)
        org-agenda-custom-commands '(("f" "Future events"
                                      tags "TIMESTAMP>=\"<now>\""
                                      ((org-agenda-sorting-strategy '((tags ts-up)))))
                                     ("y" "Year's events" agenda ""
                                      ((org-agenda-span 'year)
                                       (org-agenda-show-all-dates nil)
                                       (org-agenda-skip-function
                                        (org-agenda-skip-entry-if 'scheduled 'deadline))))
                                     ("d" "Daily" agenda ""
                                      ((org-agenda-start-with-clockreport-mode t)
                                       (org-agenda-start-with-log-mode 'clockcheck)
                                       (org-agenda-use-time-grid nil)
                                       (org-agenda-span 'day)
                                       (org-agenda-start-on-weekday nil)))
                                     ("n" "Next actions"
                                      ((agenda ""
                                               ((org-agenda-use-time-grid nil)
                                                (org-agenda-span 'day)
                                                (org-agenda-start-on-weekday nil)))
                                       (tags-todo "interruptions"
                                                  ((org-agenda-overriding-header "Interruptions")))
                                       (agenda ""
                                               ((org-agenda-use-time-grid nil)
                                                (org-agenda-span 'week)
                                                (org-agenda-start-day "+1d")
                                                (org-agenda-start-on-weekday nil)))
                                       (tags "projects"
                                             ((org-agenda-overriding-header "Next actions")
                                              (org-agenda-skip-function #'home.d/org-agenda-skip-not-next-action)
;; FIXME
;; There is a bug when applying this to agenda
;; It results in an error, when log mode is activated and there is a clock-in activity for the current day.
;; This is probably due to the way on the current day a grid with all the hours of the day is displayed.
                                              (org-agenda-prefix-format '((tags . "%-32(home.d/org-agenda-project-prefix-format) ")))))
                                       (agenda ""
                                               ((org-agenda-span 'month)
                                                (org-agenda-overriding-header "Next month")
                                                (org-agenda-show-all-dates nil)
                                                (org-agenda-skip-function
                                                 '(or
                                                   (org-agenda-skip-entry-if 'scheduled 'deadline)
                                                   (home.d/org-agenda-skip-if-tag "recurring")))))))))
  (add-hook 'org-agenda-mode-hook
            (lambda () (setq default-directory org-directory)))
  ;; FIXME this should not be in a hook because:
  ;; + it gets set every time an org-mode buffer is open
  ;; + it only works after one buffer has been open (if you run
  ;;   org-agenda-list-stuck-projects before opening any org buffer,
  ;;   you end up using the default value
  (add-hook 'org-mode-hook
            (lambda ()
              (setq org-stuck-projects `("projects&LEVEL=2" ,org-not-done-keywords nil "")))))

(use-package org-capture
  :bind ("C-c c" . org-capture)
  :init
  ;; consider merging the inbox into a single tree (what is the purpose of tasks vs notes?)
  (setq org-capture-templates '(("t" "task" entry
                                 (file+olp home.d/capture-file "inbox" "tasks")
                                 "* TODO %?\n:LOGBOOK:\n- Created on %U\n:END:")
                                ("r" "read" entry
                                 (file+olp home.d/capture-file "inbox" "tasks")
                                 "* READ [[%:link][%:description]]\n:LOGBOOK:\n- Created on %U\n:END:"
                                 :immediate-finish t)
                                ("w" "watch" entry
                                 (file+olp home.d/capture-file "inbox" "tasks")
                                 "* WATCH [[%:link][%:description]]\n:LOGBOOK:\n- Created on %U\n:END:"
                                 :immediate-finish t)
                                ("n" "note" entry
                                 (file+olp home.d/capture-file "inbox" "notes")
                                 "* %?\n:LOGBOOK:\n- Created on %U\n:END:")
                                ("m" "meeting" entry
                                 (file+olp home.d/capture-file "inbox" "meetings")
                                 "* meeting with %? about \n:LOGBOOK:\n- Created on %U\n:END:"
                                 :clock-in t :clock-resume t))))

(use-package org-clock
  :defer t
  :init
  (setq org-clock-report-include-clocking-task t))

(use-package org-protocol)

(use-package org-pdfview)

(use-package pdf-tools)

(use-package paren
  :init (progn
          (setq show-paren-delay 0)
          (show-paren-mode 1)))

(use-package paredit
  :commands paredit-mode
  :init (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package recentf
  :init (setq recentf-max-saved-items nil
              recentf-auto-cleanup 60))

(use-package smart-mode-line
  ;; consider using powerline and smart-mode-line-powerline-theme
  :init (progn
          (setq sml/no-confirm-load-theme t
                sml/theme nil
                rm-blacklist '(" God"))
          (sml/setup)))

(defun ddb/add-shell-extension (shell &optional ext)
  (let* ((ext (or ext shell))
         (rx (format "\\.%s\\'" ext)))
    (add-to-list 'auto-mode-alist `(,rx . sh-mode))
    ;; Usually do not quote lambda in a hook, as a lambda is self-quoting.
    ;; Here we want to quote it, to force the "evaluation" of the let-bound variables
    (add-hook 'sh-mode-hook `(lambda ()
                               (when (string-match ,rx buffer-file-name)
                                 (sh-set-shell ,shell))))))

(use-package sh-script
  :defer t
  :init (ddb/add-shell-extension "zsh"))

(use-package whitespace
  :bind ("C-c w" . whitespace-mode))

(use-package yaml-mode
  :defer t)

(use-package zenburn-theme
  :init (progn
          (load-theme 'zenburn t)
          (zenburn-with-color-variables
            (set-face-attribute 'sml/minor-modes nil :foreground zenburn-orange))))

;; ;; https://emacs.stackexchange.com/questions/34670/how-does-one-set-input-decode-map-on-gui-frames-in-emacsclient
;;
;; None of the following is enough to reclaim C-m in a new emacsclient

;; (defun ~/setup-C-m (&rest args)
;;   (define-key input-decode-map [?\C-m] [C-m]))

;; (~/setup-C-m)
;; (add-hook 'tty-setup-hook #'~/setup-C-m)
;; (add-hook 'window-setup-hook #'~/setup-C-m)
;; (add-hook 'terminal-init-xterm-hook #'~/setup-C-m)
;; (add-hook 'after-make-frame-functions #'~/setup-C-m)
;;
;; had to eval the code in each emacsclient invocation

(use-package selected
  :init (selected-global-mode))

(use-package multiple-cursors
  ;; - Sometimes you end up with cursors outside of your view. You can scroll
  ;;   the screen to center on each cursor with `C-v` and `M-v`.
  ;;
  ;; - If you get out of multiple-cursors-mode and yank - it will yank only
  ;;   from the kill-ring of main cursor. To yank from the kill-rings of every
  ;;   cursor use yank-rectangle, normally found at C-x r y.

  :bind (("C-'" . set-rectangular-region-anchor)

         ("<C-m> ^"     . mc/edit-beginnings-of-lines)
         ("<C-m> `"     . mc/edit-beginnings-of-lines)
         ("<C-m> $"     . mc/edit-ends-of-lines)
         ("<C-m> '"     . mc/edit-ends-of-lines)
         ("<C-m> R"     . mc/reverse-regions)
         ("<C-m> S"     . mc/sort-regions)
         ("<C-m> W"     . mc/mark-all-words-like-this)
         ("<C-m> Y"     . mc/mark-all-symbols-like-this)
         ("<C-m> a"     . mc/mark-all-like-this-dwim)
         ("<C-m> c"     . mc/mark-all-dwim)
         ("<C-m> l"     . mc/insert-letters)
         ("<C-m> n"     . mc/insert-numbers)
         ("<C-m> r"     . mc/mark-all-in-region-regexp)
         ("<C-m> t"     . mc/mark-sgml-tag-pair)
         ("<C-m> w"     . mc/mark-next-like-this-word)
         ("<C-m> x"     . mc/mark-more-like-this-extended)
         ("<C-m> y"     . mc/mark-next-like-this-symbol)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> ("     . mc/mark-all-symbols-like-this-in-defun)
         ("<C-m> C-("   . mc/mark-all-words-like-this-in-defun)
         ("<C-m> M-("   . mc/mark-all-like-this-in-defun)
         ("<C-m> ["     . mc/vertical-align-with-space)
         ("<C-m> {"     . mc/vertical-align))

  :bind (:map selected-keymap
              ("C-'" . mc/edit-lines)
              ("c"   . mc/edit-lines)
              ("."   . mc/mark-next-like-this)
              ("<"   . mc/unmark-next-like-this)
              ("C->" . mc/skip-to-next-like-this)
              (","   . mc/mark-previous-like-this)
              (">"   . mc/unmark-previous-like-this)
              ("C-<" . mc/skip-to-previous-like-this)
              ("y"   . mc/mark-next-symbol-like-this)
              ("Y"   . mc/mark-previous-symbol-like-this)
              ("w"   . mc/mark-next-word-like-this)
              ("W"   . mc/mark-previous-word-like-this)))

(use-package proof-site
  :defer t)

(use-package company-coq
  :defer t
  :init (add-hook 'coq-mode-hook #'company-coq-mode))


;; https://github.com/abo-abo/swiper/issues/776

(defun counsel-env-res (res path)
  (let ((apath (abbreviate-file-name path)))
    (list (car res)
          (if (file-accessible-directory-p path)
              (file-name-as-directory apath)
            apath))))

(defun counsel-env ()
  (delq nil
        (mapcar
         (lambda (s)
           (let* ((res (split-string s "=" t))
                  (path (cadr res)))
             (when (stringp path)
               (cond ((file-exists-p path)
                      (counsel-env-res res path))
                     ((file-exists-p (expand-file-name path ivy--directory))
                      (counsel-env-res
                       res (expand-file-name path ivy--directory)))
                     (t nil)))))
         process-environment)))

(defun counsel-expand-env ()
  (interactive)
  (if (equal ivy-text "")
      (progn
        (let ((enable-recursive-minibuffers t)
              (history (symbol-value (ivy-state-history ivy-last)))
              (old-last ivy-last)
              (ivy-recursive-restore nil))
          (ivy-read "Env: " (counsel-env)
                    :action (lambda (x)
                              (ivy--reset-state (setq ivy-last old-last))
                              (setq ivy--directory "")
                              (delete-minibuffer-contents)
                              (insert (cadr x))))))
    (insert "$")))
(eval-after-load 'counsel
      '(define-key counsel-find-file-map (kbd "$") 'counsel-expand-env))

;; (require 'haskell-interactive-mode)
;; (require 'haskell-process)
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; ;;; does not seem to work
;; (define-key haskell-interactive-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
;; (define-key haskell-interactive-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

;;; https://scripter.co/converting-org-keywords-to-lower-case/
(defun modi/lower-case-org-keywords ()
  "Lower case Org keywords and block identifiers.

Example: \"#+TITLE\" -> \"#+title\"
         \"#+BEGIN_EXAMPLE\" -> \"#+begin_example\"

Inspiration:
https://code.orgmode.org/bzg/org-mode/commit/13424336a6f30c50952d291e7a82906c1210daf0."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (count 0))
      ;; Match examples: "#+FOO bar", "#+FOO:", "=#+FOO=", "~#+FOO~",
      ;;                 "‘#+FOO’", "“#+FOO”", ",#+FOO bar",
      ;;                 "#+FOO_bar<eol>", "#+FOO<eol>".
      (while (re-search-forward "\\(?1:#\\+[A-Z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)" nil :noerror)
        (setq count (1+ count))
        (replace-match (downcase (match-string-no-properties 1)) :fixedcase nil nil 1))
      (message "Lower-cased %d matches" count))))

(use-package org-agenda
  :bind (:map org-agenda-mode-map
              ("v" . 'hydra-org-agenda-view/body)))

(defun org-agenda-cts ()
  (let ((args (get-text-property
               (min (1- (point-max)) (point))
               'org-last-args)))
    (nth 2 args)))

(defhydra hydra-org-agenda-view (:hint none)
  "
_d_: ?d? day        _g_: time grid=?g? _a_: arch-trees
_w_: ?w? week       _[_: inactive      _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?    _r_: report=?r?
_m_: ?m? month      _e_: entry =?e?    _D_: diary=?D?
_y_: ?y? year       _q_: quit          _L__l__c_: ?l?"
  ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view
       (if (eq 'day (org-agenda-cts))
           "[x]" "[ ]"))
  ("w" org-agenda-week-view
       (if (eq 'week (org-agenda-cts))
           "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view
       (if (eq 'fortnight (org-agenda-cts))
           "[x]" "[ ]"))
  ("m" org-agenda-month-view
       (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ("y" org-agenda-year-view
       (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode
       (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode
       (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode
       (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode
       (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid
       (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary
       (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("["
   (let ((org-agenda-include-inactive-timestamps t))
     (org-agenda-check-type t 'timeline 'agenda)
     (org-agenda-redo)))
  ("q" (message "Abort") :exit t))
