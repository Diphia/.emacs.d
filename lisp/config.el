;;; config.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Qiyan Zhang
;;
;; Author: Qiyan Zhang <https://github.com/diphia>
;; Maintainer: Qiyan Zhang <qiyan@hey.com>
;; Created: August 13, 2022
;; Modified: August 13, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/diphia/config
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(defun my-disable-projectile-for-tramp (&optional _)
  "Disable projectile mode for remote (tramp) buffers."
  (when (file-remote-p default-directory)
    (projectile-mode -1)))

(add-hook 'find-file-hook #'my-disable-projectile-for-tramp)

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)
			     (shell . t)
			     (emacs-lisp . t)))

(setq make-backup-files nil)

(provide 'config)
;;; config.el ends here
