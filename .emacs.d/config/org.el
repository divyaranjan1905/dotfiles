;;; org.el --- Org Configuration File --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package org
             :straight t
             :commands (org-capture org-agenda)
             :hook
             (org-mode . divya/org-mode-setup)
             (org-mode . divya/org-font-setup)


             :config
             (define-key org-mode-map (kbd "C-c o b")  'org-switchb)
             (define-key org-mode-map (kbd "C-c o i") 'org-insert-link)
             (define-key org-mode-map (kbd "C-c o n")  'org-toggle-narrow-to-subtree)
             (global-set-key (kbd "C-c a")  'org-agenda)
             (define-key org-mode-map (kbd "C-c a s") 'divya/update-agenda-files)
             (define-key org-mode-map (kbd "C-c o t")  'org-todo-list)
             (define-key org-mode-map (kbd "C-c o z")  'org-content)
             (define-key org-mode-map (kbd "C-c o p")  'org-open-at-point)
             (global-set-key (kbd "C-c o c")  'org-capture)
             (define-key org-mode-map (kbd "C-c o P")  'org-present)
             (define-key org-mode-map (kbd "C-c o x")  'org-export-dispatch)
             (define-key org-mode-map (kbd "C-c o h")  'consult-org-heading)
             

             (setq org-directory "~/notes/org/")
             (setq org-ellipsis " ▾")
             (setq org-modules
                   '(org-crypt
                      org-bibtex
                      org-id
                      org-wl
                      org-habit
                      org-agenda
                      org-bookmark
                      org-eshell
                      org-irc))
             (setq org-agenda-start-with-log-mode t)
             (setq org-log-done 'time)
             (setq org-log-into-drawer t)
             ;; No slants in italics
             (setq org-hide-emphasis-markers t)
             (setq org-capture-use-agenda-date t)

             (setq org-capture-templates
                   '(("t" "Todo" entry (file "~/notes/org/org-agenda/tasks.org")
                      "** TODO %?\n  %U\n  %a\n  %i")

                     ("h" "Add habit" entry (file "~/notes/org/org-agenda/habits.org")
                      "** NEXT %?\nSCHEDULE: <%<%Y-%m-%d %a . %^{Periodicity:|+1d|+2d|+3d|+4d|+7d|+2w|+1m}>>\n\n:PROPERTIES:\n:STYLE:  habit\n:END:\n\n"))))


;;; Org capture in X
(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "org-capture") (window-system . x)))
  (select-frame-by-name "org-capture")
  (org-capture)
  (delete-other-windows))

;; Directories

(setq org-directory "~/notes/org/")
(setq org-roam-directory "~/notes/org/org-roam/")

;;; Some cool custom functions

;; From https://org-roam.discourse.group/t/capture-template-how-to-insert-link-to-org-roam-node-at-point/2500/5
(defun divya/org-roam-custom-capture-with-link ()
  "Retrieve the ID of the current org-roam file and call org-roam-capture."
  (interactive)
  (let ((id (org-entry-get 1 "ID")))
    (setq divya/org-roam-custom-curr-link
	  (org-link-make-string
	   (concat "id:" id)
	   (org-roam-node-title (org-roam-node-from-id id)))) 
    (org-roam-capture)
    (setq divya/org-roam-custom-curr-link nil)))

;; From https://emacs.stackexchange.com/questions/73158/how-to-programmatically-create-a-new-org-roam-file
(defun divya/org-roam-fast-make-link (s)

  "Make an org-roam node with title S and return a link to it.

   We eschew the usual org-capture approach for a fast, non-interactive result."
  (interactive)
  (let* ((slug (org-roam-node-slug (org-roam-node-create :title s)))
	 (filename (format "%s/%d-%s.org"
			   (expand-file-name org-roam-directory)
			   (time-convert (current-time) 'integer)
			   slug))
	 (org-id-overriding-file-name filename)
	 id)
    (with-temp-buffer
      (insert ":PROPERTIES:\n:ID:        \n:END:\n#+title: "
	      s)
      (goto-char 25)
      (setq id (org-id-get-create))
      (write-file filename)
      (org-roam-db-update-file filename)
      (format "[[id:%s][%s]]" id s))))


(defun divya/org-roam-find-make-link (s)
  "Find an org-roam node with title S and return a link to it.

   We eschew the usual `org-capture` approach for a fast, non-interactive result."
  (interactive)
  (let* ((slug (org-roam-node-slug (org-roam-node-find :title s)))
	 (filename (format "%s/%d-%s.org"
			   (expand-file-name org-roam-directory)
			   (time-convert (current-time) 'integer)
			   slug))
	 (org-id-overriding-file-name filename)
	 id)
    (with-temp-buffer
      (insert ":PROPERTIES:\n:ID:        \n:END:\n#+title: "
	      s)
      (goto-char 25)
      (setq id (org-id-get-create))
      (write-file filename)
      (org-roam-db-update-file filename)
      (format "[[id:%s][%s]]" id s))))


(defun divya/org-roam-node-from-cite (keys-entries)
  (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
  (let ((title (citar--format-entry-no-widths (cdr keys-entries)
					      "${author editor} :: ${title}")))
    (org-roam-capture- :templates
		       '(("r" "reference" plain "%?" :if-new
			  (file+head "reference/${citekey}.org"
				     ":PROPERTIES:
				      :ROAM_REFS: [cite:@${citekey}]
				      :END:
				      #+title: ${title}\n")
			  :immediate-finish t
			  :unnarrowed t))
		       :info (list :citekey (car keys-entries))
		       :node (org-roam-node-create :title title)
		       :props '(:finalize find-file))))

;; From https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html

(defun divya/org-roam-project-files ()
  "Return a list of note files for those containing `Project' tag."
  (seq-filter
   #'identity
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
	      :from tags
	      :left-join nodes
	      :on (= tags:node-id nodes:id)
	      :where (like tag (quote "%\"Project\"%"))]))))


(defun divya/update-agenda-files ()
  "Update `org-agenda-files' after running the `divya/org-roam-project-files'."
  (interactive)
  (setq org-agenda-files (append
			  '("~/notes/org/org-agenda/tasks.org")
			  '("~/notes/org/org-agenda/habits.org")
			  '("~/notes/org/org-agenda/journal.org")
			  (divya/org-roam-project-files))))


(with-eval-after-load 'org-roam
  (divya/update-agenda-files))

;; (with-eval-after-load 'org-agenda
;;   (divya/update-agenda-files))

(use-package org-ql
  :straight t)

(defun divya/org-find-heading (s)
  "Find a particular headings from the current org buffer."
  (interactive)
  (org-ql-select (current-buffer)
		 '(level 1)
		 :action #'org-get-heading))

;; Org Font Setup
(defun divya/org-font-setup ()
  "Replace list hyphen with dot"
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.85)
		  (org-level-2 . 1.6)
		  (org-level-3 . 1.55)
		  (org-level-4 . 1.45)
		  (org-level-5 . 1.35)
		  (org-level-6 . 1.3)
		  (org-level-7 . 1.2)
		  (org-level-8 . 1.1)
		  (org-document-title . 2.5)))
    (set-face-attribute (car face) nil :font "ET Bembo" :weight 'bold :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'variable-pitch nil :font "ET Bembo" :height 210)
  (set-face-attribute 'fixed-pitch nil :font "Spline Sans Mono" :height 160)
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; (defun divya/org-quote-font-setup ()
;;   "Customizing the face of quote blocks in Org mode."
;;   (setq org-fontify-quote-and-verse-blocks t)
;;   (if (org-fontify-quote-and-verse-blocks ))
;;   (set-face-attribute 'org-block nil :italic t :inherit 'variable-pitch :height 200)))

;; (divya/org-quote-font-setup)

(defun divya/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


;; Citations and Referencing in Org & Roam

(use-package org-ref
  :straight t)

(use-package org-roam-bibtex
  :straight t
  :after org-roam)

(use-package citar
  :straight t
  :custom
  (citar-bibliography '("~/bib/math.bib" "~/bib/neuro.bib" "~/bib/psycho.bib" "~/bib/phy.bib" "~/bib/phil.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))


;; Org-appear

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))



(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; A bit of tweaks 
(setq org-datatree-add-timestamp 'inactive)
(setq org-fontify-whole-heading-line t)

;(use-package org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

;;; Org-Tempo

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("la" . "src latex"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))

;;; Org-Todo
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	(sequence "INACTIVE(i)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAIT" :foreground "orange" :weight bold)
	      ("INACTIVE" :foreground "yellow" :weight bold)
	      ("ACTIVE" :foreground "red" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("COMPLETED" :foreground "forest green" :weight bold))))

;;; Refiling
(setq org-refile-targets (quote ((nil :maxlevel . 9)
				 (org-agenda-files :maxlevel . 9))))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Exclude DONE state tasks from refile targets
(defun divya/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'divya/verify-refile-target)

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-tag-alist
      '((:startgroup)
					; Put mutually exclusive tags here
	(:endgroup)
	("@errand" . ?E)
	("agenda" . ?a)
	("planning" . ?p)
	("publish" . ?P)
	("note" . ?n)
	("idea" . ?i)))

;;;; Evil Bindings in Org 

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
	 (org-agenda-mode . evil-org-mode)
	 (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
  :config
  (evil-org-agenda-set-keys))

;(use-package evil-org-agenda)

;; Update toc on save
(use-package toc-org
  :after org
  :hook
  (org-mode . toc-org-enable))

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

;; Presentations with Org

(defun divya/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun divya/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
				     (header-line (:height 4.5) variable-pitch)
				     (org-code (:height 1.55) org-code)
				     (org-verbatim (:height 1.55) org-verbatim)
				     (org-block (:height 1.25) org-block)
				     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images)
  (setq mode-line-format nil)
  (divya/org-present-prepare-slide))

(defun divya/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images))

(defun divya/org-present-next ()
  (interactive)
  (org-present-next)
  (divya/org-present-prepare-slide))

(defun divya/org-present-prev ()
  (interactive)
  (org-present-prev)
  (divya/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
	      ("C-c C-j" . divya/org-present-next)
	      ("C-c C-k" . diyva/org-present-prev))
  :hook ((org-present-mode . divya/org-present-hook)
	 (org-present-mode-quit . divya/org-present-quit-hook)))


;; The Mighty Org-Roam (v2)

(use-package org-roam
  :straight t
  :init
  (org-roam-db-autosync-enable)

  :config
  ;; From https://jethrokuan.github.io/org-roam-guide/
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
	(file-name-nondirectory
	 (directory-file-name
	  (file-name-directory
	   (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  (setq org-roam-node-display-template
	(concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))


  :custom
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
      :unnarrowed t)

     ("m" "main slipbox" plain
      "%?"
      :target (file+head "main/${slug}.org" "#+TITLE: ${title}\n")
      :unnarrowed t)

     ("r" "reference slipbox" plain
      "%?"
      :target (file+head "ref/${title}.org" "#+TITLE: ${title}\n")
      :unnarrowed t)

     ("p" "project" plain "* Objectives\n\n%?\n\n\n* Tasks\n\n** TODO\n\n* Links\n\nThis project is linked to the following things: "
      :target (file+head "projects/${slug}.org" "#+TITLE: ${title}\n#+filetags: Project")
      :unnarrowed t)))

  (org-roam-dailies-directory "daily-journal/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?" :target
      (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n"))

     ("t" "Task" entry "* TODO %?\n  %U\n  %a\n  %i" :target
      (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
      :olp ("Tasks")
      :empty-lines 1)))

  :bind (("C-c n f"  . org-roam-node-find)
	 ("C-c n c"  . org-roam-capture)
	 ("C-c n M-d"  . org-roam-dailies-goto-date)
	 ("C-c n d c"  . org-roam-dailies-capture-date)
	 ("C-c n T"  . org-roam-dailies-capture-tomorrow)
	 ("C-c n t"	. org-roam-dailies-capture-today)
	 ("C-c n d ,"  . org-roam-dailies-goto-today)
	 ("C-c n d ."  . org-roam-dailies-goto-yesterday)
	 ("C-c n d /"  . org-roam-dailies-goto-tomorrow)
	 ("C-c n F"  . org-roam-ui-follow-mode)
	 ("C-c n v"  . org-roam-ui-open)
	 ("C-c n g"  . org-roam-buffer-toggle)
	 ("C-c n s"  . org-roam-db-sync)
	 ("C-c n C"  . org-roam-db-clear-all)
	 ("C-c n i"  . org-roam-node-insert)))


(use-package org-roam-ui
  :straight t
  :after org-roam)

(use-package rg
  :straight t)

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))

;; Org-Agenda

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-span 'day)
(setq org-agenda-start-with-log-mode t)

;; Set default column view headings: Task Total-Time Time-Stamp
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))
	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	("n" "Next Tasks"
	 ((todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))))

	("W" "Work Tasks" tags-todo "+work-email")

	;; Low-effort next actions
	("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	 ((org-agenda-overriding-header "Low Effort Tasks")
	  (org-agenda-max-todos 20)
	  (org-agenda-files org-agenda-files)))

	("C-w" "Workflow Status"
	 ((todo "WAIT"
		((org-agenda-overriding-header "Waiting on External")
		 (org-agenda-files org-agenda-files)))
	  (todo "PLAN"
		((org-agenda-overriding-header "In Planning")
		 (org-agenda-todo-list-sublevels nil)
		 (org-agenda-files org-agenda-files)))
	  (todo "INACTIVE"
		((org-agenda-overriding-header "Currently Inactive")
		 (org-agenda-files org-agenda-files)))
	  (todo "ACTIVE"
		((org-agenda-overriding-header "Currently Active")
		 (org-agenda-files org-agenda-files)))
	  (todo "DONE"
		((org-agenda-overriding-header "Completed")
		 (org-agenda-files org-agenda-files)))
	  (todo "CANC"
		((org-agenda-overriding-header "Cancelled")
		 (org-agenda-files org-agenda-files)))))))

;; Update Agenda
(divya/update-agenda-files)




(provide 'org)
;;; org.el ends here
