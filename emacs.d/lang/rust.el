;;; rust.el --- Rust config stuff
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package rust-mode
   :mode "\\.rs\\'"
   :straight t
   :hook (rust-mode . lsp)
   :config

   ;; new and shiny it is
   (setq lsp-rust-server 'rust-analyzer)

   ;; give all the lints
   (setq lsp-rust-analyzer-cargo-watch-command "clippy")
   (setq lsp-rust-analyzer-cargo-watch-enable t)

   ;; we want errors for everything please
   (setq lsp-rust-analyzer-cargo-all-targets t)
   (setq lsp-rust-all-features t)
   (setq lsp-rust-all-targets t)

   ;; format on save
   (add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                            (lsp-format-buffer))))

   ;; display inlay hints
   (setq lsp-rust-analyzer-server-display-inlay-hints t)
   (add-hook 'lsp-mode (lambda () (lsp-rust-analyzer-inlay-hints-mode))))

(provide 'rust)
;;; rust.el ends here
