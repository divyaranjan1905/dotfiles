;; guix.el -- Divya's Guix Configurations for Emacs --- -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(use-package guix
  :straight t)

(global-set-key (kbd "C-x C-g") 'guix)

(provide 'guix.el)
;;; guix.el ends here
