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

(defun switch-to-org-mode ()
  "Switch emacs to org mode"
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (set-face-foreground 'default "black")
  (set-background-color "white")
  (setq frame-title-format '("Org Mode\n")))

(defun copy-file-path-to-clipboard ()
  "Copy the current file path to the clipboard."
  (interactive)
  (let ((file-path (if (eq major-mode 'dired-mode)
                       (dired-current-directory)
                     (buffer-file-name))))
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

(defun open-all-links-in-org-mode ()
  "Open all links in the current org-mode buffer."
  (interactive)
  (require 'org)
  (unless (eq major-mode 'org-mode)
    (error "This function works only in org-mode buffers"))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-link-any-re nil t)
      (let ((link (org-element-context)))
        (when (eq (org-element-type link) 'link)
          (org-open-at-point-global)
	  (sleep-for 0.5))))))

(defun my-dired-open-selection ()
  "Open the currently selected files in Dired using the 'open' command."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (dired-do-shell-command "open *" nil files)))

(defvar my-dired-sort-options
  '((?s . ("Sort by size" . "-S"))
    (?m . ("Sort by last modified time" . "-t"))
    (?n . ("Sort by file name" . ""))
    (?e . ("Sort by file extension" . "-X")))
  "Association list of sorting options for `my-dired-sort'.")

(defun my-dired-sort (key)
  "Sort Dired buffer in different ways based on a provided KEY."
  (interactive "cEnter sort key: ")
  (let ((option (assoc key my-dired-sort-options)))
    (if option
        (progn
          (message (car (cdr option)))
          (dired-sort-other (concat dired-listing-switches " " (cdr (cdr option)))))
      (message "Invalid key %c" key))))

(defvar my-dired-jump-key-folder-alist
  '((?1 . "~")
    (?v . "/Volumes")
    (?t . "~/temp")
    (?o . "~/Documents")
    (?e . "~/Desktop")
    (?d . "~/Downloads"))
  "Alist of key-folder pairs for 'my-dired-jump'.")

(defun my-dired-jump (key)
  "Jump Dired buffer in different ways based on a provided KEY."
  (interactive "cEnter jump key: ")
  (let ((folder (cdr (assoc key my-dired-jump-key-folder-alist))))
    (if folder
	(quick-open folder)
      (message "Invalid key %c" key))))

(defun counsel-rg-current-directory ()
  "Run `counsel-rg' in the current directory."
  (interactive)
  (let ((default-directory (file-name-directory (buffer-file-name))))
    (counsel-rg nil default-directory nil)))

(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(global-evil-leader-mode)
(evil-leader/set-key
  "<SPC>" 'counsel-git
  "<tab>" 'awesome-tab-counsel-switch-group
  "d" 'my-dired-jump
  "tt" 'toggle-truncate-lines
  "`" 'evil-switch-to-windows-last-buffer
  "bb" 'switch-to-buffer
  "bp" 'previous-buffer
  "bd" 'kill-current-buffer
  "ff" 'find-file
  "fr" 'counsel-recentf
  "fp" (lambda () (interactive) (quick-open "~/.emacs.d/init.el"))
  "fs" (lambda () (interactive) (quick-open "~/.ssh/config"))
  "v" (lambda () (interactive) (quick-open (concat org-directory "snippet.org")))
  "j" (lambda () (interactive) (quick-open (concat org-directory "roam/20220612104302-journal.org")))
  "c" (lambda () (interactive) (quick-open "/tmp/main.chat.org"))
  "gg" 'magit-status
  "tp" 'open-femp-python
  "nrf" 'org-roam-node-find
  "nrr" 'org-roam-buffer-toggle
  "mdt" 'org-time-stamp
  "mdT" 'org-time-stamp-inactive
  "pp" 'projectile-switch-project
  "sb" 'swiper
  "sd" 'counsel-rg-current-directory
  "sp" 'counsel-git-grep
  "x" 'switch-to-scratch
  "y" 'copy-file-path-to-clipboard
  "?" 'counsel-describe-function
  "[" (lambda () (interactive) (shrink-window-horizontally 40))
  "]" (lambda () (interactive) (enlarge-window-horizontally 40)))

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

(define-key evil-normal-state-map (kbd "g d") 'lsp-bridge-find-def)

(define-key evil-normal-state-map (kbd "/") 'swiper)

(require 'evil-collection)
(evil-collection-init)

(with-eval-after-load 'evil-collection
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map (kbd "q") 'toggle-j-k-keys-for-peep)
    (evil-define-key 'normal dired-mode-map (kbd "Y") 'dired-copy-file-path-to-clipboard)
    (evil-define-key 'normal dired-mode-map (kbd "o") 'my-dired-open-selection)
    (evil-define-key 'normal dired-mode-map (kbd "s") 'my-dired-sort)
    (evil-define-key 'normal dired-mode-map (kbd "d") 'my-dired-jump)
    (evil-define-key 'normal dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
    ))

(defun toggle-j-k-keys-for-peep ()
  "Toggle the behavior of 'j' and 'k' keys."
  (interactive)
  (if (eq (lookup-key evil-normal-state-local-map (kbd "j")) 'peep-dired-next-file)
      (progn
        (define-key evil-normal-state-local-map (kbd "j") 'evil-next-visual-line)
        (define-key evil-normal-state-local-map (kbd "k") 'evil-previous-visual-line)
	(delete-other-windows)
	(peep-dired-kill-buffers-without-window))
    (progn
      (peep-dired)
      (define-key evil-normal-state-local-map (kbd "j") 'peep-dired-next-file)
      (define-key evil-normal-state-local-map (kbd "k") 'peep-dired-prev-file)
      )))

(setq org-return-follows-link t)

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(provide 'init-key)
;;; init-key.el ends here
