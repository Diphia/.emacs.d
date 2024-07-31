;;; init-packages.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Qiyan Zhang
;;
;; Author: Qiyan Zhang <https://github.com/diphia>
;; Maintainer: Qiyan Zhang <qiyan@hey.com>
;; Created: August 13, 2022
;; Modified: August 13, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/diphia/init-packages
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'magit)

(require 'ivy)
(ivy-mode)
(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit) ;; use escape key to quit minibuffer
(setq ivy-height 20)
(setq ivy-initial-inputs-alist nil) ;; by default, ivy will add ^ to restrict the start of string (regex), add this line to remove it

(require 'counsel)
(counsel-mode)

(require 'swiper)

(require 'smex)

(require 'which-key)
(which-key-mode)

(require 'org-roam)
(setq org-directory "/Users/diphia/org-files/")
(setq org-roam-directory "/Users/diphia/org-files/roam")

(require 'fd-dired)
(require 'peep-dired)
(setq peep-dired-ignored-extensions '("elc" "mkv" "webm" "mp4" "mp3" "ogg" "iso" "mat" "exe" "dmg" "pcap"))

(require 'dired-subtree)

(require 'tmtxt-async-tasks)
(require 'tmtxt-dired-async)

(require 'rg)

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(custom-set-variables
 '(org-agenda-files (list "/Users/diphia/org-files/agenda.org")))

(require 'ox-confluence)
(with-eval-after-load 'ox
  (require 'ox-hugo))

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.scpt$" . applescript-mode))

(require 'lua-mode)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'racket-mode)
(add-to-list 'auto-mode-alist '("\\.scm$" . racket-mode))
(add-to-list 'interpreter-mode-alist '("scm" . racket-mode))

(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(setq python-shell-interpreter "python3")
(setq python-indent 4)

(require 'projectile)
(projectile-mode)

(require 'ledger-mode)

(require 'counsel-projectile)

(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(require 'undo-tree)
(define-key evil-normal-state-map "u" 'undo-tree-undo)
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)

(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(plist-put org-format-latex-options :scale 1.5)
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))

(require 'awesome-tab)
(awesome-tab-mode t)

(require 'presentation)

(require 'posframe)

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(require 'imenu-list)
(setq imenu-list-focus-after-activation t)
(setq imenu-list-auto-resize t)

(add-hook 'c-mode-hook (lambda () (setq c-basic-offset 4)))

(provide 'init-packages)
;;; init-packages.el ends here
