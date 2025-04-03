;;; init-ui.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Qiyan Zhang
;;
;; Author: Qiyan Zhang <https://github.com/diphia>
;; Maintainer: Qiyan Zhang <qiyan@hey.com>
;; Created: April 10, 2022
;; Modified: April 10, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/diphia/init-ui
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq split-width-threshold 120)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-title-format '("GNU Emacs\n"))
(set-face-attribute 'default nil :font "Monaco" :height 180);;

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq insert-directory-program "/usr/local/bin/gls" dired-use-ls-dired t)
(setq dired-listing-switches "-alh --group-directories-first")

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'org-mode-hook (lambda () (show-paren-mode 0)))

;; replace - with • in list
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)

(defun start-with-org-inline-images-universal ()
  (let ((current-prefix-arg 4)) 
    (call-interactively 'org-display-inline-images)))
(add-hook 'org-mode-hook 'start-with-org-inline-images-universal)

(defun org-toggle-inline-images-universal ()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively 'org-toggle-inline-images)))

(setq org-fontify-quote-and-verse-blocks t) ;; enable to define quote and verse block style

(set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji" :height 200))
(set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "Sarasa Mono Slab SC" :height 200 :weight 'normal))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "»")
                                       ("#+END_SRC" . "«")
                                       ("#+begin_src" . "»")
                                       ("#+end_src" . "«")
                                       ("#+RESULTS" . "»")
                                       ("#+begin_quote" . "\"")
                                       ("#+end_quote" . "\"")
                                       ("#+TITLE:" . "")
                                       ("#+ATTR_ORG:" . "")
                                       ("{toc}" . "§")
                                       ("#+title:" . "")))

;;(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(setq org-hide-emphasis-markers t)

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-list-indent-offset 2)

(setq-default tab-width 4)

(setq org-bullets-bullet-list '("◉" "○" "✸" "◇" "✿" ""))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "CANCELLED" "OVERDUE")))

(setq org-todo-keyword-faces
      '(("OVERDUE" . (:foreground "red" :weight bold))
	("CANCELLED" . (:foreground "red" :weight bold))))

(setq valign-fancy-bar t)
(require 'valign)
(add-hook 'org-mode-hook #'valign-mode)

;; show link message when hovering
(defun link-message ()
  (let ((object (org-element-context)))
    (when (eq (car object) 'link)
      (message "%s"
           (org-element-property :raw-link object)))))
(add-hook 'post-command-hook 'link-message)

(setq scroll-step            1
      scroll-conservatively  10000)

(require 'doom-themes)
(setq doom-themes-enable-bold t    
      doom-themes-enable-italic t) 
(load-theme 'doom-oceanic-next t)

(require 'doom-modeline)
(doom-modeline-mode 1)

(provide 'init-ui)
;;; init-ui.el ends here
