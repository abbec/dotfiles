;;; lang-python.el --- Python config
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package blacken
  :straight t
  :hook (python-mode . blacken-mode))

(use-package python
  :straight t
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook (python-mode . lsp))

(provide 'lang-python)
;;; lang-python.el ends here
