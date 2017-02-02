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
(mapc 'global-unset-key '([left] [right] [up] [down]))

(let ((arrow-key-mode-maps '(help-mode-map Info-mode-map)))
  (mapc
   (lambda (map)
     (define-key (symbol-value map) [left] 'left-char)
     (define-key (symbol-value map) [right] 'right-char)
     (define-key (symbol-value map) [up] 'previous-line)
     (define-key (symbol-value map) [down] 'next-line))
   arrow-key-mode-maps))

(setq default-input-method 'russian-computer)
(set-default-coding-systems 'utf-8)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(set-default-font "InputMono-10")
(when (eq system-type 'linux) ;; linux specific settings
  )

(when (eq system-type 'darwin) ;; mac specific settings
  )

(use-package ivy-mode
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

(use-package go-eldoc
  :ensure t
  :init
  (defun go-mode-setup ()
    (go-eldoc-setup))
  )

(use-package go-autocomplete
  :ensure t
  :init
  (ac-config-default)
  (require 'auto-complete-config)
  (require 'go-autocomplete)
  )

(use-package golint
  :ensure t
  )

(use-package project-explorer
  :ensure t
  :init
  (require 'project-explorer)
  (global-set-key (kbd "M-e") 'project-explorer-toggle)
  )
(use-package go-mode
  :ensure t
  :init
  (electric-pair-mode 1)
  (defun go-mode-setup ()
    (go-eldoc-setup)
    (add-hook 'before-save-hook 'gofmt-before-save))
  (add-hook 'go-mode-hook 'go-mode-setup)  
  (defun go-mode-setup ()
    (go-eldoc-setup)
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save))
  (add-hook 'go-mode-hook 'go-mode-setup)

  ;;Godef, shows function definition when calling godef-jump
  (defun go-mode-setup ()
    (go-eldoc-setup)
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (local-set-key (kbd "M-.") 'godef-jump))
  (add-hook 'go-mode-hook 'go-mode-setup)
  ;;Custom Compile Command
  (defun go-mode-setup ()
    (setq compile-command "go build -v && go test -v && go vet && golint")
    (define-key (current-local-map) "\C-c\C-c" 'compile)
    (go-eldoc-setup)
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (local-set-key (kbd "M-.") 'godef-jump))
  (add-hook 'go-mode-hook 'go-mode-setup)

  ;;Configure golint
  (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
  (require 'golint)
  (add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
  (require 'go-flycheck)  
  )

(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t)
  )
