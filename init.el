
;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name "site-lisp/use-package" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/s.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/f.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/ht.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/hydra" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/dash.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/spinner.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/annalist.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/tomelr" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/emacs-web-server" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/emacs-websocket" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/emacs-zmq" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/markdown-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/magit/lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/transient/lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/with-editor/lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/emacsql" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/evil" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/evil-leader" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/evil-snipe" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/evil-collection" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/evil-org-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/undo-tree.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/vimish-fold" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/evil-vimish-fold" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/linum-relative" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/swiper" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/smex" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/emacs-which-key" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/valign" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/org-bullets" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/org-roam" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/ox-hugo" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/emacs-jupyter" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/yasnippet" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/dockerfile-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/yaml-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/applescript-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/lua-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/racket-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/json-snatcher" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/json-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/company-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/projectile" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/counsel-projectile" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/ledger-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/plantuml-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/auctex" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/lsp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/emacs-presentation-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/awesome-tray" user-emacs-directory))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(add-hook 'auto-save-hook 'org-save-all-org-buffers)

;; (setq byte-compile-warnings '(cl-functions))

(require 'init-ui)
(require 'init-key)

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
(require 'jupyter)
(org-babel-do-load-languages 'org-babel-load-languages '((jupyter . t)))
(org-babel-jupyter-override-src-block "python")

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
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

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

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

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

(require 'plantuml-mode)
(setq org-plantuml-jar-path (expand-file-name "/Applications/plantuml-1.2022.5.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(plist-put org-format-latex-options :scale 1.5)
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))

(require 'presentation)

(require 'awesome-tray)
(awesome-tray-mode 1)

;;(require 'lsp)
;;(add-hook 'python-mode-hook #'lsp)

(provide 'init)
;;; init.el ends here
