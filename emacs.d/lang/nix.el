;;; nix.el --- Nix lang stuff
;;; Commentary:

;;; Code:
(require 'use-package)

(defun nix-mode-setup ()
  (setq tab-width 2)
  (when (executable-find "rnix-lsp")
    (eglot-ensure)
    (add-hook 'before-save-hook (lambda () (when (eq 'nix-mode major-mode)
                                             (eglot-format-buffer))))))

(use-package nix-mode
  :straight t
  :hook (nix-mode . nix-mode-setup))

(provide 'nix)
;;; nix.el ends here
