;;; anki.el --- Emacs Configuration File --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Divya's  Anki Conmfiguration with Emacs
;;; Code:

(use-package anki-editor
  :after org-noter
  :bind (:map org-mode-map
	      ("<f12>" . anki-editor-cloze-region-auto-incr)
	      ("<f11>" . anki-editor-cloze-region-dont-incr)
	      ("<f10>" . anki-editor-reset-cloze-number)
	      ("<f9>"  . anki-editor-push-tree))

  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t)

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))

  ;; Initialize
  (anki-editor-reset-cloze-number))


;;; Org templates
(setq org-my-anki-file "~/notes/org/anki.org")

(add-to-list 'org-capture-templates
             '("b" "Anki basic"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Universe::%^{Subdeck}\n:END:\n** Front\n%?\n** Back\n%x\n"))

(add-to-list 'org-capture-templates
             '("c" "Anki cloze"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Universe::%^{Subdeck}\n:END:\n** Text\n%x\n** Extra\n"))

;; Allow Emacs to access content from clipboard.
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

(provide 'anki.el)

;;; anki.el ends here
