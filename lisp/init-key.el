;;; init-key.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Qiyan Zhang
;;
;; Author: Qiyan Zhang <https://github.com/diphia>
;; Maintainer: Qiyan Zhang <qiyan@hey.com>
;; Created: April 10, 2022
;; Modified: April 10, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/diphia/init-key
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)

(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)

(define-key evil-window-map "\C-h" 'evil-window-left)
(define-key evil-window-map "\C-j" 'evil-window-down)
(define-key evil-window-map "\C-k" 'evil-window-up)
(define-key evil-window-map "\C-l" 'evil-window-right)

(require 'vimish-fold)
(vimish-fold-global-mode 1)

(setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
(require 'evil-vimish-fold)
(global-evil-vimish-fold-mode 1)

(require 'evil-snipe)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)

(add-hook 'org-mode-hook
  (lambda ()
   (local-set-key [s-return] 'org-insert-item)))

(defun open-temp-python ()
  "Quick open a temp python file."
  (interactive)
  (find-file "/tmp/test.py"))

(defun switch-to-scratch()
  "Quick open a scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun open-init-config ()
  "Quick open init.el ."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-journal()
  "Quick open journal.org ."
  (interactive)
  (find-file (concat org-directory "roam/20220612104302-journal.org")))

(defun open-snippet ()
  "Quick open snippet.org ."
  (interactive)
  (find-file (concat org-directory "snippet.org")))

(defun open-chat ()
  "Quick open chat list ."
  (interactive)
  (find-file (concat org-directory "chat/")))

(defun switch-to-code-mode ()
  "Switch emacs to code mode"
  (interactive)
  (progn
    (set-face-foreground 'default "#a9b7c6")
    (set-background-color "#2B2B2B"))
    (setq frame-title-format '("GNU Emacs\n")))

(defun switch-to-org-mode ()
  "Switch emacs to org mode"
  (interactive)
  (progn
    (set-face-foreground 'default "black")
    (set-background-color "white"))
    (setq frame-title-format '("Org Mode\n")))

(defun copy-file-path-to-clipboard ()
  "Copy the current file path to the clipboard."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (when file-path
      (kill-new file-path)
      (message "File path copied to clipboard: %s" file-path))))

(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(global-evil-leader-mode)
(evil-leader/set-key
  "<SPC>" 'counsel-git
  "`" 'evil-switch-to-windows-last-buffer
  "bb" 'switch-to-buffer
  "bd" 'kill-current-buffer
  "ff" 'find-file
  "fr" 'counsel-recentf
  "fp" 'open-init-config
  "v" 'open-snippet
  "j" 'open-journal
  "c" 'open-chat
  "gg" 'magit-status
  "tp" 'open-temp-python
  "nrf" 'org-roam-node-find
  "nrr" 'org-roam-buffer-toggle
  "mdt" 'org-time-stamp
  "mdT" 'org-time-stamp-inactive
  "oaa" 'org-agenda-list
  "pp" 'projectile-switch-project
  "sb" 'swiper
  "sd" 'counsel-rg
  "sp" 'counsel-projectile-rg
  "x" 'switch-to-scratch
  "y" 'copy-file-path-to-clipboard
  "?" 'counsel-describe-function)
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

(require 'evil-collection)
(evil-collection-init)

(setq org-return-follows-link t)

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(provide 'init-key)
;;; init-key.el ends here
