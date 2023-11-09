;;; feed.el --- Feeds configuration file --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Divya's feed configuration foe Emacs

;;; Code:

;; Elfeed


(use-package elfeed
  :straight t
  :config
  (global-set-key (kbd "C-x f") 'divya/elfeed-load))

;; (use-package elfeed-org
;;   :straight t
;;   :init
;;   (elfeed-org)
;;   :config
;;   (setq rmh-elfeed-org-files (list "~/.dotfiles/.emacs.d/feed.org")))

;; Showing a divided pane of the entry

(setq elfeed-show-entry-switch #'elfeed-display-buffer)
(defun elfeed-display-buffer (buf &optional act)
  (pop-to-buffer buf)
  (set-window-text-height (get-buffer-window) (round (* 0.6 (frame-height)))))

(defun elfeed-search-show-entry-pre (&optional lines)
  "Returns a function to scroll forward or back in the Elfeed
  search results, displaying entries without switching to them."
  (lambda (times)
    (interactive "p")
    (forward-line (* times (or lines 0)))
    (recenter)
    (call-interactively #'elfeed-search-show-entry)
    (select-window (previous-window))
    (unless elfeed-search-remain-on-entry (forward-line -1))))

(define-key elfeed-search-mode-map (kbd "M-n") (elfeed-search-show-entry-pre +1))
(define-key elfeed-search-mode-map (kbd "M-p") (elfeed-search-show-entry-pre -1))
(define-key elfeed-search-mode-map (kbd "M-RET") (elfeed-search-show-entry-pre))

;; Navigating with SPC
(defun elfeed-scroll-up-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed"
  (interactive "^P")
  (let ((scroll-error-top-bottom nil))
    (condition-case-unless-debug nil
        (scroll-up-command arg)
      (error (elfeed-show-next)))))

(defun elfeed-scroll-down-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed"
  (interactive "^P")
  (let ((scroll-error-top-bottom nil))
    (condition-case-unless-debug nil
        (scroll-down-command arg)
      (error (elfeed-show-prev)))))

(define-key elfeed-show-mode-map (kbd "SPC") 'elfeed-scroll-up-command)
(define-key elfeed-show-mode-map (kbd "S-SPC") 'elfeed-scroll-down-command)


;; Opening in EWW

(defun elfeed-show-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-show-visit use-generic-p)))

(defun elfeed-search-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-search-browse-url use-generic-p)))

(define-key elfeed-show-mode-map (kbd "M-b") 'elfeed-show-eww-open)
(define-key elfeed-search-mode-map (kbd "M-B") 'elfeed-search-eww-open)

;; Opening YT links in MPV
(setq browse-url-browser-function
      '(("https:\\/\\/www\\.youtu\\.*be." . browse-url-mpv)
        ("." . browse-url-generic)))

(defun browse-url-mpv (url &optional single)
  (start-process "mpv" nil "mpv" (shell-quote-argument url)))

;; Some initializations
(defun divya/elfeed-load ()
  "Function to load elfeed database"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force)
  (display-line-numbers-mode 0))

(defun divya/elfeed-quit ()
  "Writes to disk and quits."
  (interactive)
  (elfeed-db-save)
  (elfeed-export-opml "~/Sync/feeds.opml")
  (quit-window))
(define-key elfeed-search-mode-map (kbd "q") 'divya/elfeed-quit)

;; Updating feeds every 8 hours
(defun divya/elfeed-update ()
  "Updates feeds and adds new ones."
  (interactive)
  (elfeed-load-opml "~/Sync/feeds.opml")
  (elfeed-update))

(define-key elfeed-search-mode-map (kbd "C-u") 'divya/elfeed-update)

;; (run-at-time nil (* 4 60 60) #'divya/elfeed-update)


;;; Elfeed Dashboard

;; (use-package elfeed-dashboard
;;   :straight t
;;   :config
;;   (setq elfeed-dashboard-file "~/.dotfiles/.emacs.d/elfeed-dashboard.org")
;;   (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(provide 'feed.el)
;;; feed.el ends here
