
;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name "lisp/use-package" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/s.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/f.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/dash.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/annalist.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/magit/lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/transient/lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/with-editor/lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/emacsql" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/evil" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/evil-leader" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/evil-snipe" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/evil-collection" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/vimish-fold" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/evil-vimish-fold" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/linum-relative" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/swiper" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/smex" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/emacs-which-key" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/valign" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/org-bullets" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/org-roam" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/yasnippet" user-emacs-directory))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; (setq byte-compile-warnings '(cl-functions))

(require 'init-ui)
(require 'init-key)

(require 'magit)

(require 'ivy)
(ivy-mode)
(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit) ;; use escape key to quit minibuffer
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

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(custom-set-variables
 '(org-agenda-files (list "/Users/diphia/org-files/agenda.org")))

(require 'ox-confluence)

(provide 'init)
;;; init.el ends here
