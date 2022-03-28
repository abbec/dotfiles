;;; init.el --- Do it!
;;; Commentary:

;;; Code:

;; global stuff
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode)
(setq backup-directory-alist `(("." . "~/.saves")))

(setq gc-cons-threshold (* 256 1024 1024)) ;; 256 MiB
(setq-default read-process-output-max (* 10 1024 1024)) ;; 10 MiB

;; install straight
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(eval-and-compile
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package))

;; Visual stuff
(add-to-list 'default-frame-alist
             '(font . "Spleen 8x16-12:antialias=false"))
(set-frame-font "Spleen 8x16-12:antialias=false" nil t)

(setq-default column-number-mode t)

;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(defun init/set-emoji-font ()
  "Enable colorful emojis."
  (setq-default use-default-font-for-symbols nil)
  (set-fontset-font t 'symbol "Apple Color Emoji")
  (set-fontset-font t 'symbol "Twitter Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Twemoji" nil 'append)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "Symbola" nil 'append))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (init/set-emoji-font)))
  (init/set-emoji-font))

(use-package doom-themes
  :straight t
  :functions doom-themes-org-config
  :config
  (setq doom-themes-padded-modeline t)
  (load-theme 'doom-one t)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package diminish
  :straight t)

(use-package all-the-icons
  :straight t)

(use-package dashboard
  :straight t
  :commands dashboard-setup-startup-hook
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (dashboard-setup-startup-hook))

(use-package which-key
  :straight t
  :commands which-key-mode
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)))

;; colors in compilation buffer
(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (setq-default compilation-environment '("TERM=xterm-256color"))
  :hook (compilation-filter . my-colorize-compilation-buffer))

;; icomplete and fido 🐩
(use-package icomplete
  :init
  (icomplete-mode)
  (fido-mode))

(use-package ace-window
  :straight t
  :bind ("M-o" . ace-window))

;; as it turns out, we are not insane
(prefer-coding-system 'utf-8)
(setq-default default-buffer-file-coding-system 'utf-8)

;; Languages

;; global language config
(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :commands (exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-arguments nil)
  (setq exec-path-from-shell-variables '("SSH_AUTH_SOCK" "PATH" "SHELL" "NIX_PATH"))
  (exec-path-from-shell-initialize))

;; some sane line length defaults
(setq-default fill-column 90)
(setq-default auto-fill-function 'do-auto-fill)
(setq-default comment-auto-fill-only-comments t)
(diminish 'auto-fill-function)

(use-package company
  :straight t
  :diminish company-mode
  :commands global-company-mode
  :init (global-company-mode))

(use-package project
  :straight t
  :config
  (setq project-switch-commands 'project-find-file))

(use-package eldoc
  :straight t
  :diminish eldoc-mode
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (diminish 'eldoc-mode))

(use-package flymake
  :straight t
  :init
  (define-prefix-command 'flymake-command-map)
  :bind-keymap ("C-c !" . flymake-command-map)
  :bind (:map flymake-command-map
              ("n" . flymake-goto-next-error)
              ("p" . flymake-goto-prev-error)
              ("l" . flymake-show-buffer-diagnostics)
              ("a" . flymake-show-project-diagnostics)))

(use-package flymake-diagnostic-at-point
  :after flymake
  :straight t
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package eglot
  :straight t
  :after direnv
  :diminish eldoc-mode
  :init
  (define-prefix-command 'eglot-command-map)
  :bind-keymap ("C-c e" . eglot-command-map)
  :bind (:map eglot-command-map
              ("r" . eglot-rename)
              ("a" . eglot-code-actions)
              ("h" . eldoc)))

(use-package yasnippet
  :straight t
  :demand t
  :diminish yas-minor-mode
  :functions yas-global-mode
  :config
  (yas-global-mode 1))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(setq-default indent-tabs-mode nil) ; no thanks
(setq-default whitespace-style '(face spaces space-mark tabs newline))
(global-whitespace-mode t)
(setq-default require-final-newline t)
(diminish 'global-whitespace-mode)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/lang")
  (require 'nix)
  (require 'markdown)
  (require 'lang-python)
  (require 'rust)
  (require 'racket)
  (require 'terraform)
  (require 'protobuf)
  (require 'clojure)
  (require 'csharp)
  (require 'cpp)
  (require 'cmake))

(use-package yaml-mode
  :straight t
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (setq auto-revert-check-vc-info t)
  (setq auto-revert-avoid-polling t)
  (global-auto-revert-mode t))

(use-package magit
  :straight t
  :functions git-commit-turn-on-auto-fill
  :config
  (setq git-commit-summary-max-length 50)
  :hook
  (git-commit-mode
   .
   (lambda ()
     (setq fill-column 72)
     )))

(use-package forge
  :straight t
  :after magit)

(use-package elcord
  :straight t
  :commands elcord-mode
  :init
  (setq elcord-quiet t)
  (elcord-mode))

(use-package direnv
  :straight t
  :commands (direnv-mode direnv-update-environment)
  :init
  ;; make sure direnv gets to run
  (add-hook 'prog-mode-hook #'direnv--maybe-update-environment)
  (advice-add 'eglot-ensure :before #'direnv-update-environment)
  (setq direnv-always-show-summary nil)
  (direnv-mode))

;; installed with nix
(use-package vterm)

(provide 'init)
;;; init.el ends here
