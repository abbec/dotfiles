;;; rust.el --- Rust config stuff
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package rustic
   :mode ("\\.rs\\'" . rustic-mode)
   :straight t
   :config
   (setq rustic-format-on-save nil)
   (setq rustic-lsp-format t)
   (setq rustic-format-trigger 'on-save)

   (setq lsp-semantic-tokens-enable t)
   (setq lsp-rust-analyzer-server-display-inlay-hints t)
   (add-hook 'lsp-mode (lambda () (lsp-rust-analyzer-inlay-hints-mode))))

(provide 'rust)
;;; rust.el ends here
