;;Load package-install sources
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
;;(set-default-font "Terminess Powerline-10")
(set-default-font "Go Mono for Powerline-10")
(when (eq system-type 'linux) ;; linux specific settings
  )

(when (eq system-type 'darwin) ;; mac specific settings
  )

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

(use-package flycheck
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

(use-package json-mode
  :ensure t
  )

(use-package jedi
  :ensure t
  :config
  (setq jedi:environment-root "jedi")  ; or any other name you like
  (setq jedi:environment-virtualenv
	(when (eq system-type 'darwin) ;; mac specific settings
	  (append python-environment-virtualenv
		  '("--python" "/usr/local/bin/python3"))))

	(when (eq system-type 'linux) ;; linux specific settings
	  (append python-environment-virtualenv
		  '("--python" "/usr/bin/python3")))
  )

(use-package python-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)                 ; optional
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
