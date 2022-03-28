;;; cmake.el --- CMake configuration
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package cmake-mode
  :mode (("\\.cmake\\'" . cmake-mode)
         ("CMakeLists.txt" . cmake-mode))
  :straight t)

(provide 'cmake)
;;; cmake.el ends here
