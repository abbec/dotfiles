;;; java.el --- Java config stuff
;;; Commentary:

;;; Code:
(require 'use-package)

;; TODO: Other build systems
(defun find-mvn-roots (dir)
  (let ((root (locate-dominating-file dir "pom.xml")))
    (and root (cons 'vc root))))

(defun java-mode-setup ()
  (setq c-basic-offset 2)
  (setq tab-width 2)
  (setq eglot-workspace-configuration
        '((:java . (:format (:settings (:url "./style.xml" :profile "GoogleStyle"))))))
  (eglot-ensure))

(use-package java-mode
  :mode "\\.java\\'"
  :hook (java-mode . java-mode-setup)
  :init
  (add-hook 'project-find-functions 'find-mvn-roots)

  ;; format on save
  (add-hook 'before-save-hook (lambda () (when (eq 'java-mode major-mode)
                                           (eglot-format-buffer))))

  ;; eclipse-jdt breaks the spec which in turn breaks code actions
  ;; This behaviour can't be disabled and needs to be worked around
  (cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
    "Eclipse JDT breaks spec and replies with edits as arguments."
    (mapc #'eglot--apply-workspace-edit arguments)))

(provide 'java)
;;; java.el ends here
