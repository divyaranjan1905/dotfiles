;;; mail.el --- Email Configuration --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Mu4E Configuration
;;; Code:
;; adding to load-path
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")

;;; SMTP settins for sending mail
(use-package smtpmail
  :straight t
  :config
  ;; (setq sendmail-program "/usr/bin/msmtp")
  (setq send-mail-function 'smtpmail-send-it)
  (setq message-send-mail-function 'smtpmail-send-it))

;;; My email address

					;(setq user-mail-address "divya@subvertising.org")

;; Deleting evil collection for mu4e
					;(delete 'mu4e evil-collection-mode-list)
					;(delete 'mu4e-conversation evil-collection-mode-list)

(use-package mu4e
  :straight t
  :hook
  (mu4e-compose-mode-hook . 'turn-off-auto-fill)
  :custom
  (mu4e-change-filenames-when-moving t)
  (mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  (mu4e-attachment-dir "~/Downloads/mail/")

  ;; Refreshing mail with isync
  (mu4e-update-interval 500)
  (mu4e-get-mail-command "mbsync -a && mu index")
  (mu4e-headers-auto-update t)
  (mu4e-maildir "~/.local/share/email/")
  :config
  ;; Shortcuts
  (setq mu4e-maildir-shortcuts
	'(("autistici/INBOX" . ?i)
	  ("/autistici/Sent" . ?s)
	  ("/autistici/Trash" . ?t)
	  ("/autistici/Drafts" . ?d))))
;; :hook
;; (mu4e-view-mode . visual-line-mode)
;; (mu4e-view-mode . divya/enable-focus))

;; Contexts

(with-eval-after-load 'mu4e
  (setq mu4e-contexts
	`(
	  ,(make-mu4e-context
	    :name "Autistici"
	    :match-func
	    (lambda (msg)
	      (when msg
		(string-prefix-p "/autistici" (mu4e-message-field msg :maildir))))
	    :vars '((user-mail-address . "divya@subvertising.org")
		    (user-full-name . "Divya Ranjan")
		    (mu4e-drafts-folder . "/autistici/Drafts")
		    (mu4e-sent-folder . "/autistici/Sent")
		    (mu4e-refile-folder . "/autistici/Archive")
		    (mu4e-trash-folder . "/autistici/Trash")
		    (smtpmail-smtp-server . "smtp.autistici.org")
		    (smtpmail-default-smtp-server . "smtp.autistici.org")
		    (smtpmail-smtp-user . "divya@subvertising.org")
		    (smtpmail-smtp-authenticate . t)
		    (smtpmail-smtp-service . 465)
		    (smtpmail-debug-info . t)
		    (smtpmail-debug-verb . t)
		    (smtpmail-stream-type . ssl)))

	  ,(make-mu4e-context
	    :name "tilde.club"
	    :match-func (lambda (msg)
			  (when msg
			    (string-prefix-p "/tilde.club" (mu4e-message-field msg :maildir))))
	    :vars '((user-mail-address . "divya@tilde.club")
		    (user-full-name . "Divya Ranjan")
		    (mu4e-drafts-folder . "/tilde.club/Drafts")
		    (mu4e-sent-folder . "/tilde.club/Sent")
		    (mu4e-refile-folder . "/tilde.club/Archive")
		    (mu4e-trash-folder . "/tilde.club/Trash")
		    (smtpmail-smtp-server . "smtp.tilde.club")
		    (smtpmail-smtp-user . "divya")
		    (smtpmail-smtp-authenticate . t)
		    (smtpmail-smtp-service . 587)
		    (smtpmail-debug-info . t)
		    (smtpmail-debug-verb . t)
		    (smtpmail-stream-type . starttls))))))


;; For gettting mail notifications

(defun divya-mu4e-notif ()
  "Display both mode line and desktop alerts for incoming new emails."
  (interactive)
  (mu4e-update-mail-and-index 1)

  (mu4e-alert-enable-notifications))

(defun divya-mu4e-refresh-mode-line ()
  (interactive)
					;  (mu4e~proc-kill)
  (mu4e-alert-enable-mode-line-display))

(use-package mu4e-alert
  :straight t
  :after mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
	(concat
	 "flag:unread maildir:/autistici/INBOX "
	 "OR "
	 "flag:unread maildir:/tilde.club/INBOX"))
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications)
  (divya-mu4e-notif)
  (divya-mu4e-refresh-mode-line)
  :custom
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications))

;; Some hooks
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; auth-source

(use-package auth-source)
(setq auth-sources '("~/.authinfo.gpg"))


;;; Other cool stuff that works with mu4e
;; (use-package mu4e-conversation
;;   :straight t)

;; (use-package evil-mu4e
;;   :straight t)

;; (use-package mu4e-marker-icons
;;   :straight t)

;; (use-package mu4e-overview
;;   :straight t)

;; (use-package mu4e-maildirs-extension
;;   :straight t)

;;; Setting defaults
					;(setq mail-user-agent 'mu4e-user-agent)
					;(set-variable 'read-mail-command 'mu4e)

;; Set the first context as the default
(setq mu4e-context-policy 'pick-first)
(mu4e) 

;; Global bingind for mu4e
(define-key global-map (kbd "C-c m") 'mu4e)


(provide 'mail.el)

;;; mail.el ends here