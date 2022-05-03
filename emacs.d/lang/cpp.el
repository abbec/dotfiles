;;; cpp.el --- C++ config
;;; Commentary:

;;; Code:
(require 'use-package)

(defun find-c-roots (dir)
  (let ((root (or
               (locate-dominating-file dir "Makefile")
               (locate-dominating-file dir "meson.build")
               (locate-dominating-file dir "CMakeLists.txt"))))
    (and root (cons 'vc root))))

(use-package cc-mode
  :defer t
  :hook (c-mode-common . eglot-ensure)
  :config
  ;; format on save
  (add-hook 'before-save-hook (lambda () (when (or
                                                (eq 'cc-mode major-mode)
                                                (eq 'c++-mode major-mode)
                                                (eq 'c-mode major-mode))
                                           (eglot-format-buffer))))
  (add-hook 'project-find-functions 'find-c-roots)
  (setq-default c-basic-offset 4))

(provide 'cpp)
;;; cpp.el ends here
