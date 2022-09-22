;;; lang-python.el --- Python config
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package blacken
  :straight t
  :hook (python-mode . blacken-mode))

(defun find-python-roots (dir)
  (let ((root (or
               (locate-dominating-file dir "setup.py")
               (locate-dominating-file dir "setup.cfg"))))
    (and root (cons 'vc root))))

(use-package python
  :straight t
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook (
         (python-mode . eglot-ensure)
         ;; format on save
         (before-save . (lambda () (when (eq 'python-mode major-mode)
                                     (eglot-format-buffer)))))
  :init
  (add-hook 'project-find-functions 'find-python-roots))

(provide 'lang-python)
;;; lang-python.el ends here
