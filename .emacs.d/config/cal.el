;;; cal.el --- Emacs Calendar Configuration File --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Divya's Calendar congifuration

;;; Code:

(require 'calendar)
(setq org-icalendar-include-todo t)
(setq org-icalendar-store-UID t)
(setq org-icalendar-combined-agenda-file "~/life/calendar/org-agenda.ics")

(setq org-agenda-diary-file "~/notes/org/diary.org")
(setq org-agenda-include-diary t)

(global-set-key (kbd "C-x c")  'calendar)


;; To export org-agenda to an .ics file

(global-set-key (kbd "C-x <Scroll_Lock>") 'org-icalendar-combine-agenda-files)


(provide 'cal)
;;; cal.el ends here
