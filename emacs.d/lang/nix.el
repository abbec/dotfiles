;;; nix.el --- Nix lang stuff
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package nix-mode
  :straight t
  :config
  (setq tab-width 2))

(provide 'nix)
;;; nix.el ends here
