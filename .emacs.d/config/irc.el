;; irc.el --- Divya's Emacs IRC Config --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; ERC

(require 'erc)

;; Connecting to server

;; (use-package erc
;;   :bind (:map erc-mode-map
;; 	      ("RET" . nil)
;; 	      ("C-c C-c" . #'erc-send-current-line))
;;   :config
;;   (setopt erc-modules
;; 	  (seq-union '(sasl nicks irccontrols bufbar nickbar scrolltobottom)
;; 		     erc-modules))
;;   ;; Setting nick
;;   (setq
;;    erc-nick "div"
;;    erc-user-full-name "Divya Ranjan")

;;   :init
;;   (erc-tls :server "irc.libera.chat"
;; 	   :port "6697")

;;   :custom-face (erc-notice-face ((t (:slant italic :weight unspecified)))))

;; (setq erc-autojoin-channels-alist '((libera.chat "#emacs" "#erc" "#fsf")))


;;; ERC
(require 'erc)
;; Server configurations
(erc-tls :server "irc.libera.chat"
	 :nick "divya")
(erc-tls :server "irc.freenode.net"
	 :port 6667
	 :nick "divya")
(erc-tls :server "irc.tilde.chat"
	 :port 6697
	 :nick "divya")
(erc-tls :server "ctrl-c.tilde.chat"
	 :port 6667
	 :nick "divya")

(setopt erc-modules
	(seq-union '(nicks scrolltobottom)
		   erc-modules))

;; Rebinding 'send' to something other than RET.
(define-key erc-mode-map (kbd "C-c C-c") 'erc-send-current-line)

;; Prevent from being notified for joining and other stuff
(require 'erc-track)
(setopt erc-track-faces-priority-list
	(remq 'erc-notice-face erc-track-faces-priority-list))

(setq erc-server "irc.libera.chat"
      erc-nick "divya"
      erc-user-full-name "Divya Ranjan"
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat"
				     "#emacs"
				     "#haskell"
				     "#guix"
				     "#guile"
				     "#fsf"
				     "#fsf-sys"
				     "#racket"
				     "#rust"
				     "#lisp")))


(provide 'irc)
;;; irc.el ends here
