;;; ui.el --- UI Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;;; Divya's Emacs UI Configurations

;;; Code:
;; Making sure icons work
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

;;; Icons in dired
(use-package all-the-icons-dired
  :straight t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package crux
  :straight t

  :bind
  (("C-x w v" . crux-swap-windows)
   ("C-o"     . crux-smart-open-line)
   ("C-x B"   . divya/org-scratch)
   :map dired-mode-map
   ("O" . crux-open-with))
  :config
  (defun divya/org-scratch ()
    (interactive)
    (let ((initial-major-mode 'org-mode))
      (crux-create-scratch-buffer))))

;; We don't want the welcome page
(setq inhibit-startup-message t)

;; Disabling Menus and Other things
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;No Fringes & weird breaks
(fringe-mode 0)
(global-visual-line-mode 1)

;; Adding Numbers
(global-display-line-numbers-mode 1)


;; Themes
;; (load-theme 'modus-vivendi t)
(use-package ef-themes
  :straight t)

(global-set-key (kbd "C-c t") 'consult-theme)

;(load-theme 'ef-dark t)
;; Change theme depending on the time of the day
(setq calendar-longitude 85.69
      calendar-latitude 20.16)

(use-package circadian
  :straight t
  :after solar
  :config
  (setq circadian-themes '((:sunrise . modus-operandi-tinted)
			   (:sunset . ef-dark)))
  (circadian-setup))

;; Highlighting the current line
(global-hl-line-mode 1)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; An eternally blinking cursor
(blink-cursor-mode 1)

;; Recent files
(recentf-mode 1)
(global-set-key (kbd "C-c r") 'consult-recent-file)
;; (divya/leader-keys
;;  "rf" 'crux-recentf-find-file)

;; Saves history of commands
(setq history-length 300)
(savehist-mode 1)

;; Remember where you were last time in the file
(save-place-mode 1)

;; No GUI dialog boxes
(setq use-dialog-box nil)

;; Emacs updates file changes on the fly
(global-auto-revert-mode 1)

;; Do the same with dired
(setq global-auto-revert-non-file-buffers t)

;;;; Display hex colors in emacs
(use-package rainbow-mode
  :straight t
  :init
  (rainbow-mode 1))

;; Notifications

(use-package alert
  :straight t
  :commands alert
  :config
  (setq alert-default-style 'notifications))


;; A nice Doom-like modeline
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 8)))

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)


;; Multiple time zones
;; (setq display-time-world-list
;;       '(("Etc/UTC" "UTC")
;; 	("America/New_York" "New York")
;; 	("Europe/London" "London")))
;; (setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

;; Disabling line numbers for some modes

(dolist (mode '(org-mode-hook
		eshell-mode-hook
		term-mode-hook
		pdf-view-mode-hook
		doc-view-mode-hook
		dired-mode-hook
		nov-mode-hook
		writeroom-mode-hook
;		eww-mode-hook
;		elfeed-search-mode-hook
		elfeed-show-mode-hook
		image-mode-hook
		org-agenda-mode-hook
		mu4e-view-mode-hook
		mu4e-compose-mode-hook
                mu4e-main-mode-hook
		olivetti-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Disabling blinking cursor when reading things.

(dolist (mode '(pdf-view-mode-hook
		doc-view-mode-hook))
  (add-hook mode (lambda () (blink-cursor-mode -1))))

;;; Setting the font

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Spline Sans Mono-12"))
;; Fontify current frame
(fontify-frame nil)

;;(set-face-attribute 'default nil :font "Spline Sans Mono" :weight 'regular :height 130)

;; ;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

;; For changing font sizes
;; (use-package company-posframe
;;   :straight t
;;   :config
;;   (company-posframe-mode 1))

;; Unicode, Emojis UTF-8
(use-package unicode-fonts
  :straight t
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyps))
  (unicode-fonts-setup))

(prefer-coding-system 'utf-8)

(use-package emojify
  :straight t
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

;; Relative Numbers

(menu-bar--display-line-numbers-mode-relative)

;; Smartparens
(use-package smartparens
  :straight t
  :init (smartparens-global-mode 1)
  :hook (prog-mode . smartparens-mode))

;; which-key
(use-package which-key
  :straight t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;;; For some focus in life (Writeroom-mode)
(use-package writeroom-mode
  :straight t
					;  :init (writeroom-mode 1)
  :config
  (setq writeroom-width 90
	writeroom-fullscreen-effect 1
	writeroom-mode-line t
	writeroom-major-modes '(text-mode org-mode markdown-mode nov-mode Info-mode)))

(defun divya/enable-focus ()
  (interactive)
  (writeroom-mode 1)
  (display-line-numbers-mode 0))

(defun divya/disable-focus ()
  (interactive)
  (writeroom-mode 0)
  (display-line-numbers-mode 1))

(defun divya/toggle-focus ()
  (interactive)
  (if (symbol-value writeroom-mode)
      (divya/disable-focus)
    (divya/enable-focus)))

;; (divya/leader-keys
;;   "tf" '(divya/toggle-focus :which-key "Focus mode"))


;; File Icons
(use-package all-the-icons-dired
  :straight t
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Some Tweaks
(setq auto-window-vscroll nil)

;;; IVY : Minibuffer Completion

;; Currently using Vertico

;; (use-package ivy
;;   :straight t
;;   :bind (("C-s" . swiper)
;; 	 :map ivy-minibuffer-map
;; 	 ("TAB" . ivy-alt-done)
;; 	 ("C-l" . ivy-alt-done)
;; 	 ("C-j" . ivy-next-line)
;; 	 ("C-k" . ivy-previous-line)
;; 	 :map ivy-switch-buffer-map
;; 	 ("C-k" . ivy-previous-line)
;; 	 ("C-l" . ivy-done)
;; 	 ("C-d" . ivy-switch-buffer-kill)
;; 	 :map ivy-reverse-i-search-map
;; 	 ("C-k" . ivy-previous-line)
;; 	 ("C-d" . ivy-reverse-i-search-kill))
;;   :config
;;   (ivy-mode 1))

;; (use-package ivy-bibtex
;;   :straight t)


;; Vertico : Minibuffer Completion

(use-package vertico
  :straight t
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :straight t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))      ; Use orderless
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles orderless)))))


(use-package consult
  :straight t)


;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
	(apply (if vertico-mode
		   #'consult-completion-in-region
		 #'completion--in-region)
	       args)))

;;; Emacs Dashboard

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  ;; Set the title
  (setq dashboard-banner-logo-title "Live to the point of tears!")
  ;; Set the banner
  (setq dashboard-startup-banner "~/.dotfiles/.emacs.d/Mx-butterfly-edited.png")
  ;; Value can be
  ;; - nil to display no banner
  ;; - 'official which displays the official emacs logo
  ;; - 'logo which displays an alternative emacs logo
  ;; - 1, 2 or 3 which displays one of the text banners
  ;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
  ;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts t)

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t))

(define-key dashboard-mode-map (kbd "M-r") 'dashboard-refresh-buffer)

(setq dashboard-projects-backend 'project.el)

;;; Some fancy focusing

(use-package solaire-mode
  :straight t
  :init
  (solaire-global-mode +1))

(use-package dimmer
  :straight t
  :config
  (dimmer-configure-which-key)
  (dimmer-mode t))

;; Managing window widths automatically
;; (use-package zoom
;;   :straight t
;;   :init
;;   (zoom-mode +1))

;; (global-set-key (kbd "C-x +") 'zoom)

;; ;; The golden ratio
;; ;; (custom-set-variables
;; ;;  '(zoom-size '(0.618 . 0.618)))

;; (defun size-callback ()
;;   (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
;;         (t                            '(0.5 . 0.5))))

;; (custom-set-variables
;;  '(zoom-size 'size-callback))

;; ; Excluding some buffers
;; (custom-set-variables
;;  '(zoom-ignored-major-modes '(dired-mode markdown-mode))
;;  '(zoom-ignored-buffer-names '("zoom.el" "init.el"))
;;  '(zoom-ignored-buffer-name-regexps '("^*calc"))
;;  '(zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20)))))

;; The above is commented because zoom actually doesn't work well with which-key


;; Golden ratio

;; (use-package golden-ratio
;;   :straight t
;;   :init
;;   (golden-ratio-mode 1))

;; ;; Hyperbole : Managing hypertexts on buffers
;; (use-package hyperbole
;;   :straight t
;;   :init
;;   (hyperbole-mode 1))


;;; Justify-kp

(use-package justify-kp
  :straight (:host github :repo "Fuco1/justify-kp"))


;;; Drag and drop
;(use-package org-download
;  :straight t
;  :config
;  (setq org-download-backend "curl"))

;(add-hook 'dired-mode-hook 'org-download-enable)

(provide 'ui.el)
;; ui.el ends here
