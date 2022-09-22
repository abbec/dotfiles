;;; rust.el --- Rust config stuff
;;; Commentary:

;;; Code:
(require 'use-package)

;; TODO: Workspaces
(defun find-rust-roots (dir)
  (let ((root (locate-dominating-file dir "Cargo.toml")))
    (and root (cons 'vc root))))

(defun rust-mode-setup ()
  (setq eglot-workspace-configuration
        '((:rust-analyzer . (:checkOnSave (:command "clippy")))))
  (eglot-ensure))

(use-package rust-mode
  :mode "\\.rs\\'"
  :straight t
  :init
  (add-hook 'project-find-functions 'find-rust-roots)

  :config
  (add-hook 'rust-mode-hook 'rust-mode-setup)
  ;; format on save
  (add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                           (eglot-format-buffer))))
  )

(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))


(provide 'rust)
;;; rust.el ends here
