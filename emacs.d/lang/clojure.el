;;; clojure.el --- Clojure config
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package lispy
  :straight t
  :hook (
         (clojure-mode . lispy-mode)
         (clojurescript-mode . lispy-mode)))

(use-package clojure-mode
  :straight t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :hook (clojure-mode . lsp)
  :config

  ;; apparently we need to start the clojure lsp through a shell
  (setq lsp-clojure-custom-server-command '("/bin/sh" "-c" "clojure-lsp"))

  ;; format on save
  (add-hook 'before-save-hook
            (lambda ()
              (when (or
                     (eq 'clojure-mode major-mode)
                     (eq 'clojurescript-mode major-mode))
                (lsp-format-buffer)))))

(use-package cider
  :straight t)

(provide 'clojure)
;;; clojure.el ends here
