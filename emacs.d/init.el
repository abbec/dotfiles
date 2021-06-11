;;; init.el --- Do it!
;;; Commentary:

;;; Code:

;; global stuff
(tool-bar-mode -1)
(toggle-scroll-bar -1)
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
(set-frame-font "Spleen 8x16-11:antialias=false" nil t)

(setq-default column-number-mode t)

;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(setq-default use-default-font-for-symbols nil)
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Twitter Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Twemoji" nil 'append)
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

(use-package doom-themes
  :straight t
  :functions doom-themes-treemacs-config doom-themes-org-config
  :config
  (load-theme 'doom-one t)

  (setq-default doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ; (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

(use-package treemacs
  :straight t
  :bind (:map global-map
         ("C-x t t" . treemacs)))

(use-package dashboard
  :straight t
  :commands dashboard-setup-startup-hook
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (dashboard-setup-startup-hook))

(use-package which-key
  :straight t
  :commands which-key-mode
  :init
  (which-key-mode))

(use-package helm
  :straight t
  :demand t
  :functions helm-mode
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("M-s o" . helm-occur))
  :config
  (helm-mode 1))

(use-package projectile
  :straight t
  :commands projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode))

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

(use-package direnv
  :straight t
  :commands (direnv-mode direnv-update-environment)
  :init
  ;; make sure direnv gets to run
  (advice-add 'lsp :before #'direnv-update-environment)
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package flycheck
  :straight t
  :commands global-flycheck-mode
  :init (global-flycheck-mode))

(use-package company
  :straight t
  :commands global-company-mode
  :init (global-company-mode))

(use-package lsp-mode
 :straight t
 :hook (lsp-mode . lsp-enable-which-key-integration)
 :commands lsp
 :config
 (setq lsp-signature-render-documentation nil)
 (setq lsp-idle-delay 0.500))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package lsp-treemacs
 :straight t)

(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)

(use-package yasnippet
  :straight t
  :demand t
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

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/lang")
  (require 'nix)
  (require 'markdown)
  (require 'lang-python)
  (require 'rust)
  (require 'racket)
  (require 'clojure))

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
  :straight t)

(use-package forge
  :straight t
  :after magit)

(use-package elcord
  :straight t
  :commands elcord-mode
  :init
  (elcord-mode))

;; installed with nix
(use-package vterm)

(provide 'init)
;;; init.el ends here
