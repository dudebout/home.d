(package-initialize)

(let ((pre-init (concat (getenv "HOME_D") "/profile/emacs/pre-init.el")))
  (when (file-exists-p pre-init)
    (load-file pre-init)))

(require 'home.d-interactives (concat (getenv "HOME_D") "/emacs/home.d-interactives.el"))

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

(setq-default indent-tabs-mode nil
              require-final-newline t)

(add-hook 'before-save-hook 'whitespace-cleanup)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(defun tmux-default-directory ()
  (interactive)
  (call-process "ct" nil nil nil default-directory))

(bind-key "C-x M-w" 'home.d/rename-current-buffer-file)
(bind-key "C-x M-k" 'home.d/delete-current-buffer-and-delete-file)
(bind-key "C-c r" 'replace-string)
(bind-key "C-c g" 'rgrep)
;; Make sure that the last letter of the keybinding does not
;; correspond to the letter of the XMonad binding called by ct, else
;; xdotool key does not seem to be called properly / there is an
;; overlap problem. That problem can be fixed with a sleep (not ideal)
;;
;; If using "C-c t" and "Super+t", you would get an error saying that "C-S-t is not defined"
(bind-key "C-c d" 'tmux-default-directory)

;; Not sure that this is such a good idea anyway
;; When this is turned on, we get a message about the latest loade module instead...
;; (defmacro inhibit-startup-echo-area-message ()
;;   (list 'setq 'inhibit-startup-echo-area-message (getenv "USER")))
;; (inhibit-startup-echo-area-message)


(setq use-package-verbose t)
(require 'use-package)

(use-package avy
  :bind (("C-=" . avy-goto-word-1)
         ("M-g g" . avy-goto-line))
  :init (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o)))

(use-package company
  :init (global-company-mode))

(use-package cursor-chg
  :init (progn
          (toggle-cursor-type-when-idle 1)
          (change-cursor-mode 1)))

(use-package ediff
  :init (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package elisp-slime-nav
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
          (add-hook 'c-mode-hook 'elisp-slime-nav-mode)))

;; TODO integrate flycheck-color-mode-line and flycheck-pos-tip
(use-package flycheck
  :init (use-package flycheck-haskell
          :init (progn
                  (add-hook 'haskell-mode-hook 'flycheck-mode)
                  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
                  (global-flycheck-mode))))

(use-package haskell-mode
  :init (progn
          (setq haskell-stylish-on-save t)
          (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
          (add-hook 'haskell-mode-hook 'interactive-haskell-mode)))

(use-package helm
  :init (progn
          (require 'helm-config)
          (setq ido-use-virtual-buffers t
                helm-mode-fuzzy-match t
                helm-completion-in-region-fuzzy-match t))
  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("M-s o" . helm-occur)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)))

(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds))

(use-package helm-git-ls
  :bind ("C-x C-d" . helm-browse-project))

(use-package macrostep
  :bind ("C-c e" . macrostep-expand))

(use-package magit
  :bind (("C-c i" . magit-status)
         ;; TODO make C-c I chose the directory first
         ("C-c I" . magit-status))
  :init (progn
          (require 'helm-mode)
          (setq magit-delete-by-moving-to-trash nil
                magit-diff-refine-hunk 'all
                magit-completing-read-function 'helm--completing-read-default)))

(use-package menu-bar
  :bind ("C-c m" . menu-bar-mode))

(use-package org-agenda
  :bind ("C-c a" . org-agenda)
  :init (progn
          (setq org-agenda-skip-scheduled-if-done t
                org-agenda-skip-deadline-if-done t
                org-startup-indented t)
          (add-hook 'org-agenda-mode-hook (lambda () (setq default-directory org-directory)))))

(use-package org-capture
  :bind ("C-c c" . org-capture))

(use-package paren
  :init (progn
          (setq show-paren-delay 0)
          (show-paren-mode 1)))

(use-package paredit
  :init (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package recentf
  :init (setq recentf-max-saved-items nil
              recentf-auto-cleanup 10))

(defun add-shell-extension (shell &optional ext)
  (let* ((ext (or ext shell))
         (rx (format "\\.%s\\'" ext)))
    (add-to-list 'auto-mode-alist `(,rx . sh-mode))
    ;; Usually do not quote lambda in a hook, as a lambda is self-quoting.
    ;; Here we want to quote it, to force the "evaluation" of the let-bound variables
    (add-hook 'sh-mode-hook `(lambda ()
                               (when (string-match ,rx buffer-file-name)
                                 (sh-set-shell ,shell))))))

(use-package sh-script
  :init (add-shell-extension "zsh"))

(use-package whitespace
  :bind ("C-c w" . whitespace-mode))

(use-package yaml-mode)

(use-package zenburn-theme
  :init (load-theme 'zenburn t))
