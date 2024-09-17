;; guix.el -- Divya's Guix Configurations for Emacs --- -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(use-package guix
  :straight t)

(provide 'guix.el)

(global-set-key (kbd "C-x C-g") 'guix)

;;; guix.el ends here
