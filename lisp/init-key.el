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

(defun switch-to-scratch()
  "Quick open a scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun quick-open (path)
  "Quick open path"
  (interactive (list (read-directory-name "Path: ")))
  (find-file path))

(defun switch-to-code-mode ()
  "Switch emacs to code mode"
  (interactive)
  (set-face-foreground 'default "#a9b7c6")
  (set-background-color "#2B2B2B")
  (setq frame-title-format '("GNU Emacs\n")))

(defun switch-to-org-mode ()
  "Switch emacs to org mode"
  (interactive)
  (set-face-foreground 'default "black")
  (set-background-color "white")
  (setq frame-title-format '("Org Mode\n")))

(defun copy-file-path-to-clipboard ()
  "Copy the current file path to the clipboard."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (when file-path
      (kill-new file-path)
      (message "File path copied to clipboard: %s" file-path))))
      
(defun dired-copy-file-path-to-clipboard ()
  "Copy the full file path of the file under the cursor in Dired mode to the clipboard."
  (interactive)
  (let ((file-path (dired-get-file-for-visit)))
    (when file-path
      (with-temp-buffer
        (insert file-path)
        (clipboard-kill-ring-save (point-min) (point-max)))
      (message "Copied file path to clipboard: %s" file-path))))

(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(global-evil-leader-mode)
(evil-leader/set-key
  "<SPC>" 'counsel-git
  "<tab>" 'awesome-tab-counsel-switch-group
  "d1" (lambda () (interactive) (quick-open "~"))
  "dv" (lambda () (interactive) (quick-open "/Volumes"))
  "dt" (lambda () (interactive) (quick-open "~/temp/"))
  "do" (lambda () (interactive) (quick-open "~/Documents"))
  "de" (lambda () (interactive) (quick-open "~/Desktop"))
  "dd" (lambda () (interactive) (quick-open "~/Downloads"))
  "tt" 'toggle-truncate-lines
  "`" 'evil-switch-to-windows-last-buffer
  "bb" 'switch-to-buffer
  "bd" 'kill-current-buffer
  "ff" 'find-file
  "fr" 'counsel-recentf
  "fp" (lambda () (interactive) (quick-open "~/.emacs.d/init.el"))
  "v" (lambda () (interactive) (quick-open (concat org-directory "snippet.org")))
  "j" (lambda () (interactive) (quick-open (concat org-directory "roam/20220612104302-journal.org")))
  "c" (lambda () (interactive) (quick-open "/tmp/main.chat.org"))
  "gg" 'magit-status
  "tp" 'open-femp-python
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
(define-key evil-normal-state-map (kbd "C-<tab>") 'awesome-tab-forward-tab)
(define-key evil-normal-state-map (kbd "C-<S-tab>") 'awesome-tab-backward-tab)
(define-key evil-normal-state-map (kbd "s-1") 'awesome-tab-select-beg-tab)
(define-key evil-normal-state-map (kbd "s-9") 'awesome-tab-select-end-tab)
(dolist (i '(2 3 4 5 6 7 8))
  (define-key evil-normal-state-map (kbd (format "s-%d" i)) 'awesome-tab-select-visible-tab))
(define-key evil-normal-state-map (kbd "s-y") 'dired-copy-file-path-to-clipboard)


(require 'evil-collection)
(evil-collection-init)


(setq org-return-follows-link t)

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(provide 'init-key)
;;; init-key.el ends here
