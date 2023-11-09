;;; bookmarks.el --- Bookmarks Configuration File --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Divya's Emacs Bookmarking Config file

;;; Code:

;; (setq bookmark-default-file "~/.dotfiles/.emacs.d/bookmarks")

(global-set-key (kbd "C-x r b") 'consult-bookmark)

(require 'bookmark+)

(provide 'bookmarks)
;;; bookmarks.el ends here

