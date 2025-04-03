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

(require 'swiper)
(with-eval-after-load 'ivy
  (ivy-mode)
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (setq ivy-height 20)
  (setq ivy-initial-inputs-alist nil))

(require 'counsel-projectile)
(with-eval-after-load 'counsel
  (counsel-mode))

(require 'smex)

(require 'which-key)
(with-eval-after-load 'which-key
  (which-key-mode))

(require 'fd-dired)
(require 'peep-dired)
(with-eval-after-load 'peep-dired
  (setq peep-dired-max-size 500000000)
  (setq peep-dired-ignored-extensions '("elc" "webm" "mp3" "ogg" "iso" "mat" "exe" "dmg" "pcap")))

(require 'dired-subtree)

(require 'rg)

(require 'yasnippet)
(with-eval-after-load 'yasnippet
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(custom-set-variables
 '(org-agenda-files (list "~/org-files/agenda.org")))

(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.scpt$" . applescript-mode))

(autoload 'lua-mode "lua-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(autoload 'racket-mode "racket-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.scm$" . racket-mode))
(add-to-list 'interpreter-mode-alist '("scm" . racket-mode))

(autoload 'json-mode "json-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(setq python-shell-interpreter "python3")
(setq python-indent 4)

(require 'projectile)
(projectile-mode)

(autoload 'ledger-mode "ledger-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(eval-after-load 'org
  '(progn
	 (setq org-directory "~/org-files/")
     (require 'evil-org)
	 (require 'ox)
	 (require 'org-roam)
	 (add-hook 'org-mode-hook 'evil-org-mode)
     (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
     (require 'evil-org-agenda)
     (evil-org-agenda-set-keys)))

(with-eval-after-load 'ox
  (require 'ox-confluence)
  (require 'ox-hugo))

(with-eval-after-load 'org-roam
  (setq org-roam-directory "~/org-files/roam"))

(eval-after-load 'evil
  '(progn
     (require 'undo-tree)
     (define-key evil-normal-state-map "u" 'undo-tree-undo)
     (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)))

(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(plist-put org-format-latex-options :scale 1.5)
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))

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
