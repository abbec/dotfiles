;;; racket.el --- Racket config
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package racket-mode
  :straight t
  :mode "\\.rkt\\'"
  :init
  (electric-pair-mode)
  (add-hook 'racket-repl-mode-hook 'goto-address-mode))

(provide 'racket)
;;; racket.el ends here
