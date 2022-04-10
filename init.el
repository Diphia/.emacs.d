
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


(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-title-format '("Org Mode\n"))
(set-face-attribute 'default nil :height 220);;

;; replace - with • in list
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-fontify-quote-and-verse-blocks t) ;; enable to define quote and verse block style

;;(setq-default mode-line-format nil)


(custom-theme-set-faces
 'user
 ;;'(mode-line ((t (:background "black" :foreground "gray70" :box (:line-width 4 :color "black")))))
 '(mode-line ((t (:background "ghost white" :foreground "gray50" :height 0.8))))
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
 '(fixed-pitch ((t ( :family "Monaco" :height 160)))))

(set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji" :height 200))
(set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "Sarasa Mono Slab SC" :height 200 :weight 'normal))

(fset 'yes-or-no-p 'y-or-n-p)

(setq org-bullets-bullet-list '("◉" "○" "◇" "✿" "✸" ""))

;; show link message when hovering
(defun link-message ()
  (let ((object (org-element-context)))
    (when (eq (car object) 'link)
      (message "%s"
           (org-element-property :raw-link object)))))
(add-hook 'post-command-hook 'link-message)

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

(require 'magit)

(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)

(require 'vimish-fold)
(vimish-fold-global-mode 1)

(require 'evil-vimish-fold)
(setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
(global-evil-vimish-fold-mode 1)

(require 'evil-snipe)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)

(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(global-evil-leader-mode)
(evil-leader/set-key
  "bb" 'switch-to-buffer
  "bd" 'kill-current-buffer
  "ff" 'find-file
  "fr" 'counsel-recentf
  "gg" 'magit-status
  "nrf" 'org-roam-node-find
  "nrr" 'org-roam-buffer-toggle
  "mdt" 'org-time-stamp
  "mdT" 'org-time-stamp-inactive
  "oaa" 'org-agenda-list
  "sb" 'swiper)
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

;;(require 'linum-relative)
;;(linum-mode)
;;(linum-relative-global-mode)

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

;;(require 'cnfonts)
;;(cnfonts-mode 1)
;;(setq cnfonts-use-face-font-rescale t)

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'org-indent-mode)

(defun nolinum ()
  (global-linum-mode 0)
)
(add-hook 'org-mode-hook 'nolinum)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-roam)
(setq org-directory "/Users/diphia/org-files/")
(setq org-roam-directory "/Users/diphia/org-files/roam")

(require 'valign)
(add-hook 'org-mode-hook #'valign-mode)
(setq valign-fancy-bar t)

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(custom-set-variables
 '(org-agenda-files (list "/Users/diphia/org-files/agenda.org")))

(provide 'init)
;;; init.el ends here
