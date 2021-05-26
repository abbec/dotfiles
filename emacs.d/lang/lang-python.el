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
  :init
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'python-mode-hook
            (lambda ()
              (message "asdasd")
              (lsp-workspace-folders-add
               (locate-dominating-file default-directory "setup.py")))))

(provide 'lang-python)
;;; lang-python.el ends here
