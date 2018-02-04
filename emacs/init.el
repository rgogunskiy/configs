;Load package-install sources
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'control)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

;; UI

(setq default-input-method 'russian-computer)
(set-default-coding-systems 'utf-8)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; set default font in initial window and for any new window
(cond
 ((string-equal system-type "darwin") ; Mac OS X
  (set-default-font "Monaco-12"))
 ((string-equal system-type "gnu/linux") ; linux
  ;; (set-default-font "Terminess Powerline-10")))
  (set-default-font "Source Code Pro for Powerline-10")))
(global-linum-mode t)

(use-package swiper
  :ensure t
  )
(use-package counsel
  :ensure t
  )
(use-package ivy
  :init
  (require 'ivy)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-load-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  )
					;

(use-package yaml-mode
  :ensure t
  )


;; (use-package go-eldoc
;;   :ensure t
;;   :init
;;   (defun go-mode-setup ()
;;     (go-eldoc-setup))
;;   )

;; (use-package go-autocomplete
;;   :ensure t
;;   :init
;;   (ac-config-default)
;;   (require 'auto-complete-config)
;;   (require 'go-autocomplete)
;;   )

;; (use-package golint
;;   :ensure t
;;   )

;; (use-package project-explorer
;;   :ensure t
;;   :init
;;   (require 'project-explorer)
;;   (global-set-key (kbd "M-e") 'project-explorer-toggle)
;;   )
;; (use-package go-mode
;;   :ensure t
;;   :init
;;   (electric-pair-mode 1)
;;   (defun go-mode-setup ()
;;     (go-eldoc-setup)
;;     (add-hook 'before-save-hook 'gofmt-before-save))
;;   (add-hook 'go-mode-hook 'go-mode-setup)  
;;   (defun go-mode-setup ()
;;     (go-eldoc-setup)
;;     (setq gofmt-command "goimports")
;;     (add-hook 'before-save-hook 'gofmt-before-save))
;;   (add-hook 'go-mode-hook 'go-mode-setup)

;;   ;;Godef, shows function definition when calling godef-jump
;;   (defun go-mode-setup ()
;;     (go-eldoc-setup)
;;     (setq gofmt-command "goimports")
;;     (add-hook 'before-save-hook 'gofmt-before-save)
;;     (local-set-key (kbd "M-.") 'godef-jump))
;;   (add-hook 'go-mode-hook 'go-mode-setup)
;;   ;;Custom Compile Command
;;   (defun go-mode-setup ()
;;     (setq compile-command "go build -v && go test -v && go vet && golint")
;;     (define-key (current-local-map) "\C-c\C-c" 'compile)
;;     (go-eldoc-setup)
;;     (setq gofmt-command "goimports")
;;     (add-hook 'before-save-hook 'gofmt-before-save)
;;     (local-set-key (kbd "M-.") 'godef-jump))
;;   (add-hook 'go-mode-hook 'go-mode-setup)

;;   ;;Configure golint
;;   (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
;;   (require 'golint)
;;   (add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
;;   (require 'go-flycheck)  
;;   )

(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t)		
  )

(use-package color-theme-sanityinc-solarized
  :ensure t
  ;; :init
  ;; (color-theme-sanityinc-solarized-dark)
  )

;; (use-package material-theme
;;   :ensure t
;;   :init
;;   (load-theme 'material t)
;;   )

(use-package json-mode
  :ensure t
  )

;; (use-package mu4e
;;   :init
;;   (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;;   ;;  (require 'mu4e)
;;   :config
;;   (load-file "~/configs/emacs/mu4e.el")
;;   )

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default)
  )

(use-package magit
  :ensure t
  )

(use-package dockerfile-mode
  :ensure t
  )


(use-package flycheck
  :ensure t
  )

(use-package py-autopep8
  :ensure t

  )

(use-package ein
  :ensure t
  )
(use-package pyenv-mode
  :ensure t
  :init
  (pyenv-mode)
  )
(use-package elpy
  :ensure t
  :init
  (setq python-shell-unbuffered nil)
  (setq python-shell-prompt-detect-failure-warning nil)
  (setq python-shell-prompt-detect-enabled nil)
  (setq elpy-rpc-python-command "~/.pyenv/versions/emacs3/bin/python")
  (pyvenv-activate "~/.pyenv/versions/emacs3/bin/python")
  (elpy-enable)
  ;; (elpy-use-ipython "~/.pyenv/versions/emacs3/bin/ipython")
  (setq python-shell-interpreter "~/.pyenv/versions/emacs3/bin/ipython"
      python-shell-interpreter-args "-i --simple-prompt")
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (require 'py-autopep8)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

  )

(use-package magit
  :ensure t
  )

(use-package markdown-mode
  :ensure t
  :config
  ;; (custom-set-faces
  ;;  '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
  ;;  '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8))))
  ;;  '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
  ;;  '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2)))))
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )


(use-package markdown-preview-mode
  :ensure t
  )

(load-file "~/configs/emacs/org-mode.el")

