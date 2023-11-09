;; init.el --- Divya's Emacs Config --- -*- lexical-binding: t -*-

;;; Commentary:

;; My main Emacs initialization file.

;;; Code:


;;; Native compilation
(setq native-comp-speed 3)

;;; Less warnings

(setq warning-minimum-level :emergency)

;; Follow symlinks
(setq vc-follow-symlinks t)

;;; Directory Setup

;;; Setting up the UI

(load (concat user-emacs-directory
	      "config/ui.el"))

;;; Key bindings
(load (concat user-emacs-directory
	      "config/keys.el"))
;;; Buffer bindings

(load (concat user-emacs-directory
	      "config/buffers.el"))

;;; Window management in Emacs
(load (concat user-emacs-directory
	      "config/wm.el"))

;;; Org

(load (concat user-emacs-directory
	      "config/org.el"))

;; Org-latex-preview
(load (concat user-emacs-directory
	      "config/org-preview.el"))

;;;; LaTeX in Emacs
(load (concat user-emacs-directory
	      "config/tex.el"))

;;; Mail in Emacs
(load (concat user-emacs-directory
	      "config/mail.el"))

;;; Dired
(load (concat user-emacs-directory
	      "config/dired.el"))

;;; Web browsing in Emacs
(load (concat user-emacs-directory
	      "config/eww.el"))
;;; Reading Books

(load (concat user-emacs-directory
	      "config/books.el"))
;;; Bookmarks
(load (concat user-emacs-directory
	      "config/bookmarks.el"))
	
;;; Feeds
(load (concat user-emacs-directory
	      "config/feed.el"))
;;; Anki
(load (concat user-emacs-directory
	      "config/anki.el"))

;; So that Emacs doesn't produce ~ files
(setq make-backup-files nil)


;;;  Fuzzy finding
(use-package fzf
  :straight t)

;; Finding across files
(use-package elgrep
  :straight t
  :bind ("C-M-g" . elgrep))


;;;; Languages

;; Emacs Lisp

;; (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(use-package helpful
  :straight t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))


;; Markdown

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :hook
  (markdown-mode . writeroom-mode)
  :config
  (setq markdown-command "marked")
  (defun divya/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
		    (markdown-header-face-2 . 1.1)
		    (markdown-header-face-3 . 1.0)
		    (markdown-header-face-4 . 1.0)
		    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face)))

    (set-face-attribute 'variable-pitch nil :font "ET Bembo"))

  (defun divya/markdown-mode-hook ()
    (divya/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'divya/markdown-mode-hook))

;; HTML

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; ;; 1. Start the server with `httpd-start'
;; ;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode
  :straight t)

(use-package skewer-mode
  :straight t)

;; Compilation

(use-package compile
  :straight t
  :custom
  (compilation-scroll-output t))

(defun auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

;; Syntax checking

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode))

;;; Project Management
;; We don't need projectile, Emacs <27.1 has an inbuilt package `project.el` that does most of what I'd need. I am keeping the projectile config here for just in case.
;; Project.el already comes binded to `C-x p`

;;; Projectile
;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init

;;   ;; Where the git repos live

;;   (when (file-directory-p "~/projects/repo/")
;;     (setq projectile-project-search-path '("~/projects/")))
;;   (setq projectile-switch-project-action #'projectile-dired)
;;   (setq projectile-project-search-path '("~/projects/")))

;;   ;; :general
;;   ;; (divya/leader-keys
;;   ;; "pf" 'counsel-projectile-find-file
;;   ;; "ps" 'counsel-projectile-switch-project
;;   ;; "pF" 'counsel-projectile-rg
;;   ;; "pp" 'counsel-projectile
;;   ;; "pc" 'projectile-compile-project
;;   ;; "pd" 'projectile-dired))


;; (use-package counsel-projectile
;;   :straight t
;;   :config (counsel-projectile-mode))

(use-package project)

;; Magit :  The best git management there is!

(use-package magit
  :straight t
  :bind-keymap
  ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
  ;; :general

  ;; (divya/leader-keys
  ;;   "g"   '(:ignore t :which-key "git")
  ;;   "gs"  'magit-status
  ;;   "gd"  'magit-diff-unstaged
  ;;   "gc"  'magit-branch-or-checkout
  ;;   "gi"  'magit-init
  ;;   "gl"   '(:ignore t :which-key "log")
  ;;   "glc" 'magit-log-current
  ;;   "glf" 'magit-log-buffer-file
  ;;   "gb"  'magit-branch
  ;;   "gP"  'magit-push-current
  ;;   "gp"  'magit-pull-branch
  ;;   "gf"  'magit-fetch
  ;;   "gF"  'magit-fetch-all
  ;;   "gr"  'magit-rebase))

;; (use-package evil-magit
;;   :after magit)

;;; Code Completion
;; Autocomplete
;; (use-package auto-complete
;;   :straight t
;;   :config
;;   (ac-config-default))

;; Corfu

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)          ;; Enable auto completion
  ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
  :bind
  ;; Another key binding can be used, such as S-SPC.
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode))

;; using corfu in terminal

(straight-use-package
 '(corfu-terminal
   :type git
   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

;; Telegram in Emacs
;; (use-package telega
;;   :straight t
;;   :config
;;   (setq telega-use-images t
;;    telega-server-libs-prefix "/usr"
;;    telega-autoplay-mode t))

;;; Shell in Emacs

;; A pop up shell
(use-package shell-pop
  :straight t
;;  :defer t
  :bind
  (("C-x /" . 'shell-pop))
  :custom
  (shell-pop-default-directory nil)
  (shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda () (eshell shell-pop-term-shell)))))
  (shell-pop-window-size 30)
  (shell-pop-full-span nil)
  (shell-pop-window-position "bottom")
  (shell-pop-autocd-to-working-dir t)
  (shell-pop-restore-window-configuration t)
  (shell-pop-cleanup-buffer-at-process-exit t))

;; Main Eshell config

(defun divya/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer))

  ;; Bind some useful keys for evil-mode
  ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  ;; (evil-normalize-keymaps)

  ;; (setq eshell-history-size         10000
  ;; 	eshell-buffer-maximum-lines 10000
  ;; 	eshell-hist-ignoredups t
  ;; 	eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :straight t)

(use-package eshell
  :straight t
  :hook (eshell-first-time-mode . divya/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))


;; Ripgrep

(use-package rg
  :straight t)

;; Searching and navigation
;; (use-package ctrlf
;;   :straight t)

;;; Screenshots within Emacs

(defun self-screenshot (&optional type)
  "Save a screenshot of type TYPE of the current Emacs frame.
As shown by the function `', type can weild the value `svg',
`png', `pdf'.

This function will output in /tmp a file beginning with \"Emacs\"
and ending with the extension of the requested TYPE."
  (interactive)
  (let* ((type (if type type
		 (intern (completing-read "Screenshot Type: "
					  '(png svg pdf postscript)))))
	 (extension (pcase type
		      ('png        ".png")
		      ('svg        ".svg")
		      ('pdf        ".pdf")
		      ('postscript ".ps")
		      (otherwise (error "Cannot export screenshot of type %s" otherwise))))
	 (filename (make-temp-file "Emacs-" nil extension))
	 (data     (x-export-frames nil type)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(global-set-key (kbd "C-x M-s") 'self-screenshot)


;; empv.el

(use-package empv
  :ensure t
  :straight (:host github :repo "isamert/empv.el")
  :config
  (bind-key "C-x m" empv-map)
  (with-eval-after-load 'embark (empv-embark-initialize-extra-actions))
  (setq empv-invidious-instance "https://invidious.rocks/api/v1")
  (add-to-list 'empv-mpv-args "--ytdl-format=best" "--save-position-on-quit"))

;; Start Emacs as a server!
;;(server-mode)

;; Hooks
(add-hook 'eww-mode-hook #'divya/enable-focus)

;;; sx.el : Stack Exchange on Emacs

(use-package sx
  :straight t
  :config
  (bind-keys :prefix "C-c s"
	     :prefix-map divya-sx-map
	     :prefix-docstring "Global keymap for sx"
	     ("i" . sx-inbox)
             ("o" . sx-open-link)
             ("u" . sx-tab-unanswered-my-tags)
             ("a" . sx-ask)
             ("s" . sx-search)
	     (";" . sx-tab-switch)
	     ("*" . sx-star)
	     ("m" . sx-tab-month)
	     ("x" . sx-tab-meta-or-main)
	     ("w" . sx-tab-week)
	     ("q" . sx-tab-all-questions)
	     ("f" . sx-tab-frontpage)
	     ("c" . sx-comment)
	     ("^" . sx-upvote)
	     ("d" . sx-display-question)))

;; Finally, get your favorite Emacs everywhere!

(use-package emacs-everywhere
  :straight t)


;;; For Superior Lisp Editing
(use-package slime
  :straight t
  :config
  (setq inferior-lisp-program "sbcl"))

;; For restarting emacs
(define-key global-map (kbd "s-r") 'restart-emacs)

;; Garbage collection in Emacs

(use-package gcmh
  :straight t
  :init
  (gcmh-mode 1))

;; Load the appropriate them
(circadian-setup)

;; Report how long packages take to load
(setq use-package-verbose t)


;; Tree-sitter
(setq treesit-extra-load-path "~/.emacs.d/tree-sitter/")

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitte" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(elfeed-feeds
   '("https://journals.sagepub.com/connected/HPY#rss-feeds"
     "https://news.nononsenseapps.com/index.atom"
     "https://protesilaos.com/master.xml"
     "https://karthinks.com/index.xml"
     "https://thecreativeindependent.com/feed.xml"
     "https://blog.sunfishcode.online/atom.xml"
     "http://arxiv.org/rss/math.DG" "http://arxiv.org/rss/math.AG"
     "http://arxiv.org/rss/math.AT" "http://arxiv.org/rss/math.CT"
     "http://arxiv.org/rss/math.CA" "http://arxiv.org/rss/math.GT"
     "http://arxiv.org/rss/math.SG" "http://arxiv.org/rss/math.HO"
     "http://arxiv.org/rss/math.KT" "http://arxiv.org/rss/math.NT"
     "http://arxiv.org/rss/math.GR" "http://arxiv.org/rss/math.GN"
     "http://arxiv.org/rss/math.LO" "http://arxiv.org/rss/math.RA"
     "https://cosmogeometer.wordpress.com/feed/"
     "https://cognitivemedium.com/feed.xml"
     "https://edwardshorterauthor.com/feed/"
     "https://www.lacanonline.com/feed/"
     "http://www.lacaninireland.com/web/feed/"
     "https://catonmat.net/feed" "https://blog.brixit.nl/rss"
     "https://solar.lowtechmagazine.com/feeds/all-en.atom.xml"
     "https://engineuring.wordpress.com/comments/feed/"
     "https://emersion.fr/blog/atom.xml"
     "https://drewdevault.com/blog/index.xml"
     "https://bitfehler.srht.site/index.xml"
     "https://sourcehut.org/blog/index.xml"
     "https://nyxt.atlas.engineer/feed"
     "http://planet.lisp.org/rss20.xml"
     "https://planet.scheme.org/atom.xml"
     "https://hnrss.org/frontpage" "https://hnrss.org/jobs"
     "https://hnrss.org/show" "https://sive.rs/en.atom.xml"
     "https://endtimes.dev/feed.xml" "https://adactio.com/rss"
     "https://thomasorus.com/feed.xml" "https://cosmic.voyage/rss.xml"
     "https://nature.com/mp.rss" "https://timharek.no/rss.xml"
     "https://www.jouissance.net/index.xml"
     "https://www.radiolacan.com/en/home?rss=1"
     "https://bluelander.bearblog.dev/feed/?type=rss"
     "http://www.aaronsw.com/2002/feeds/pgessays.rss"
     "https://rjlipton.wpcomstaging.com/feed/"
     "https://tommi.space/all.xml" "https://josephchoe.com/feed.xml"
     "http://www.numbertheory.org/ntw/ntw.xml"
     "https://kevquirk.com/feed.xml" "https://golangweekly.com/rss/"
     "https://jvns.ca/atom.xml" "https://itself.blog/feed/"
     "https://matt.might.net/articles/feed.rss"
     "https://terenceblake.wordpress.com/feed/"
     "https://stevelosh.com/rss.xml"
     "https://tinyprojects.dev/feed.xml" "https://castel.dev/rss.xml"
     "https://luchte.wordpress.com/feed/"
     "https://blog.andymatuschak.org/rss"
     "https://codecs.multimedia.cx/feed/"
     "https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-and-content.xml"
     "https://roygbyte.com/rss.xml" "https://benswift.me/feed.xml"
     "https://coredumped.dev/index.xml"
     "https://anarchistnews.org/rss.xml"
     "https://feed.podbean.com/HDSR/feed.xml"
     "https://planet.emacslife.com/atom.xml"
     "https://direct.mit.edu/rss/site_1000003/LatestOpenIssueArticles_1000004.xml"
     "https://www.nature.com/natmachintell.rss"
     "https://jmlr.org/jmlr.xml" "https://muan.co/feed.xml"
     "https://tony-zorman.com/atom.xml"
     "https://www.modeltheory.org/feed/"
     "https://www.nature.com/mp.rss"
     "http://www.edwardshorterauthor.com/feed/"
     "https://math.stackexchange.com/feeds/tag/general-topology"
     "https://news.ycombinator.com/rss"
     "https://lucasfcosta.com/feed.xml"
     "https://joy.recurse.com/feed.atom"
     "https://healeycodes.com/feed.xml"
     "https://fabiensanglard.net/rss.xml"
     "https://www.fsf.org/static/fsforg/rss/jobs.xml")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diredp-dir-name ((t (:background "transparent" :foreground "DeepSkyBlue1"))))
 '(diredp-file-suffix ((t (:foreground "orchid"))))
 '(diredp-symlink ((t (:foreground "deep pink")))))
