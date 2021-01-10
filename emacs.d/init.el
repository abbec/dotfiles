;;; init.el --- Do it!
;;; Commentary:

;;; Code:

;; global stuff
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-display-line-numbers-mode)
(setq backup-directory-alist `(("." . "~/.saves")))

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
(set-frame-font "Fixedsys Excelsior 16" nil t)
;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Twemoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

(use-package doom-themes
  :straight t
  :functions doom-themes-treemacs-config doom-themes-org-config
  :config
  (load-theme 'doom-one t)

  (setq-default doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

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

;; Languages

;; global language config
(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :commands (exec-path-from-shell-initialize exec-path-from-shell-copy-env)
  :init
  (setq exec-path-from-shell-arguments '("-l")))

(use-package direnv
  :straight t
  :commands (direnv-mode direnv-update-environment)
  :init
  ;; make sure direnv gets to run
  (advice-add 'lsp :before #'direnv-update-environment)
  :config
  (setq direnv-always-show-summary nil)
  (exec-path-from-shell-initialize)
  (direnv-mode))

(use-package flycheck
  :straight t
  :commands global-flycheck-mode
  :init (global-flycheck-mode))

(use-package company
  :straight t
  :commands global-company-mode
  :init (global-company-mode))

;; (use-package company-box
;;   :straight t
;;   :hook (company-mode . company-box-mode))

(use-package lsp-mode
 :straight t
 :hook (lsp-mode . lsp-enable-which-key-integration)
 :commands lsp)

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

(setq-default indent-tabs-mode nil) ; no thanks
(setq-default whitespace-style '(face spaces space-mark tabs newline))
(global-whitespace-mode t)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/lang")
  (require 'nix)
  (require 'markdown)
  (require 'rust))

(use-package magit
  :straight t
  :defer t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package elcord
  :straight t
  :commands elcord-mode
  :init
  (elcord-mode))

(provide 'init)
;;; init.el ends here
