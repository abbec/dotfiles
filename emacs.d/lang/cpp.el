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

(defun c-mode-setup ()
  (setq c-basic-offset 4)
  (eglot-ensure))

(use-package cc-mode
  :defer t
  :hook (c-mode-common . c-mode-setup)
  :init
  (add-hook 'project-find-functions 'find-c-roots)
  :config
  ;; format on save
  (add-hook 'before-save-hook (lambda () (when (or
                                                (eq 'cc-mode major-mode)
                                                (eq 'c++-mode major-mode)
                                                (eq 'c-mode major-mode))
                                           (eglot-format-buffer)))))

(provide 'cpp)
;;; cpp.el ends here
