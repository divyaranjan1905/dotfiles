;;; org-async.el --- Async Init.el File for Org Export  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <divya@alberteinstein>
;; Keywords: lisp, tex

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(require 'org)
(require 'ox)
;; (require 'cl)
(setq org-export-async-debug nil)
