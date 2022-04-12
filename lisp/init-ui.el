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


(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-title-format '("Org Mode\n"))
(set-face-attribute 'default nil :font "Menlo" :height 180);;

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'org-mode-hook (lambda () (show-paren-mode 0)))

;; replace - with • in list
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-fontify-quote-and-verse-blocks t) ;; enable to define quote and verse block style

(custom-theme-set-faces
 'user
 ;;'(mode-line ((t (:background "black" :foreground "gray70" :box (:line-width 4 :color "black")))))
 '(mode-line ((t (:background "ghost white" :foreground "gray50" :height 1.0))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#592c21" :font "CMU Concrete" :height 1.1))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#592c21" :font "CMU Concrete" :height 1.1))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#592c21" :font "CMU Concrete" :height 1.1))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#592c21" :font "CMU Concrete" :height 1.2))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#592c21" :font "CMU Concrete" :height 1.3))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#592c21" :font "CMU Concrete" :height 1.5 :underline nil))))
 '(org-block ((t (:inherit fixed-pitch :background "ghost white"))))
 '(org-block-begin-line ((t (:foreground "dim grey" :background "ghost white"))))
 '(org-quote ((t (:slant italic :foreground "dim gray" :background "ghost white"))))
 '(org-code ((t (:inherit (shadow fixed-pitch) :foreground "chocolate"))))
 '(org-drawer ((t (:foreground "dim grey"))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch) :foreground "dim grey"))))
 '(org-date ((t (:underline t :foreground "firebrick"))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch))))) ;; :id:
 ;;'(org-table ((t (:inherit fixed-pitch :foreground "83a598"))))
 '(org-table ((t (:inherit fixed-pitch :foreground "black"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.5))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "CMU Concrete" :height 200 :weight normal))))
 '(fixed-pitch ((t ( :family "Menlo" :height 180)))))

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
                                       ("#+title:" . "")))
;;(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(setq org-hide-emphasis-markers t)

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-list-indent-offset 2)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("◉" "○" "◇" "✿" "✸" ""))

(require 'valign)
(add-hook 'org-mode-hook #'valign-mode)
(setq valign-fancy-bar t)

;; show link message when hovering
(defun link-message ()
  (let ((object (org-element-context)))
    (when (eq (car object) 'link)
      (message "%s"
           (org-element-property :raw-link object)))))
(add-hook 'post-command-hook 'link-message)


(setq scroll-step            1
      scroll-conservatively  10000)
(setq scroll-margin 10)

(provide 'init-ui)
;;; init-ui.el ends here
