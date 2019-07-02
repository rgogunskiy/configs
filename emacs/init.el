;;; init.el --- Initialization file for Emacs

;;; Code:

(eval-and-compile
  (setq
   package-archives
   '(("melpa-stable" . "http://stable.melpa.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("org" . "http://orgmode.org/elpa/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     )))


(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Some platform specific binding
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-right-option-modifier 'control)
  (setq mac-right-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/local/opt/llvm/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setq exec-path (append exec-path '("/usr/local/opt/llvmbin")))
  )

(setq tab-width 4)
(setq visible-bell 1)
(global-hl-line-mode 1)
(setq default-input-method 'russian-computer)
(set-default-coding-systems 'utf-8)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq org-src-fontify-natively t)
;; set default font in initial window and for any new window
(cond
 ((string-equal system-type "darwin") ; Mac OS X
  ;; (set-default-font "Droid Sans Mono Slashed for Powerline-14"))
  (set-default-font "Fira Code-14"))
 ((string-equal system-type "gnu/linux") ; linux
  ;; (set-default-font "Terminess Powerline-10")))
  ;; (set-default-font "Source Code Pro for Powerline-10")))
  (set-default-font "Droid Sans Mono Slashed for Powerline-12")))
(global-linum-mode t)

(use-package highlight-indentation
  :ensure t
  :config
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
  :init
  (add-hook 'prog-mode-hook #'highlight-indentation-current-column-mode)
  (add-hook 'yaml-mode-hook #'highlight-indentation-current-column-mode)
  )
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (setenv "SHELL" "/bin/zsh")
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs
     '("PATH")))
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

(use-package yaml-mode
  :ensure t
  )

(use-package ag
  :ensure t
  )

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  )

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode)
  )

;; (use-package leuven-theme
;;   :ensure t
;;   :config
;;   (setq leuven-scale-outline-headlines nil)
;;   (setq leuven-scale-org-agenda-structure nil)
;;   ;; :init
;;   ;; (load-theme 'leuven t)
;;   )

;; (use-package kaolin-themes
;;   :ensure t
;;   :config
;;   (load-theme 'kaolin-dark t)
;;   (kaolin-treemacs-theme)
;;   )

(use-package json-mode
  :ensure t
  )

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default)
  )

(use-package dockerfile-mode
  :ensure t
  )

(use-package flycheck
  :ensure t
   :init
  (global-flycheck-mode t)
  )

(use-package magit
  :ensure t
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

(use-package markdown-preview-mode
  :ensure t
  )

(use-package ledger-mode
  :ensure t
  :init
  (add-hook 'ledger-mode-hook #'ledger-flymake-enable)
  )
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  ;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )

(use-package lua-mode
  :ensure t
  )

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-c m c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

(use-package bbdb
  :ensure t
  :config
  (require 'bbdb)
  )

(use-package org
  :ensure org-plus-contrib
  :defer 7
  )

(use-package terraform-mode
  :ensure t
  )

(use-package yasnippet
  :ensure t
  )

(use-package lsp-mode
  :init
  (setq lsp-prefer-flymake nil)
  :hook
  (go-mode . lsp)
  (yaml-mode . lsp)
  (python-mode . lsp)
  (c-mode . lsp)
  (c++-mode . lsp)
  :commands lsp)

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'yaml-mode-hook 'flycheck-mode)
  )

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
(use-package treemacs
  :ensure t
  )

(use-package rtags
  :ensure t
  :config
  (require 'rtags)
  )

(use-package company-rtags
  :ensure t
  :config
  (require 'company-rtags)
  (setq rtags-completions-enabled t)
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-rtags))
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings)
  )

(use-package treemacs-projectile
  :ensure t
  )

(use-package lsp-treemacs
  :ensure t
  :commands
  lsp-treemacs-errors-list
  )

(use-package lsp-yaml
  :load-path "my_packages/lsp-yaml"
  :after lsp
  :config
  (add-hook 'yaml-mode-hook #'lsp))

(use-package junos-mode
  :load-path "my_packages/junos-mode"
)

(use-package go-rename
  :ensure t
  )

(use-package golint
  :ensure t
  )

(use-package go-eldoc
  :ensure t
  )

(use-package company-go
  :ensure t
  )
(use-package go-guru
  :ensure t
  )

(defun my-go-electric-brace ()
  "Insert an opening brace may be with the closing one.
If there is a space before the brace also adds new line with
properly indented closing brace and moves cursor to another line
inserted between the braces between the braces."
  (interactive)
  (if (not (looking-back " "))
      (insert "{")
    (insert "{")
    (newline)
    (indent-according-to-mode)
    (save-excursion
      (newline)
      (insert "}")
      (indent-according-to-mode))))

(defun my-go-list-packages ()
  "Return list of Go packages."
  (split-string
   (with-temp-buffer
     (shell-command "go list ... 2>/dev/null" (current-buffer))
     (buffer-substring-no-properties (point-min) (point-max)))
   "\n"))

(defun my-godoc-package ()
  "Display godoc for given package (with completion)."
  (interactive)
  (godoc (helm :sources (helm-build-sync-source "Go packages"
						:candidates (my-go-list-packages))
               :buffer "*godoc packages*")))


(use-package smartparens
  :ensure t
  )
(use-package go-mode
  :init
  (setq gofmt-command "goimports"     ; use goimports instead of gofmt
        go-fontify-function-calls nil ; fontifing names of called
					; functions is too much for me
        company-idle-delay nil)
  :config
  (require 'go-guru)
  (setq godoc-and-godef-command "go doc")
  :bind
  (:map go-mode-map
	("M-." . go-guru-definition)
	("C-c d" . godoc-at-point)
	("C-c g" . godoc)
	("C-c h" . go-guru-hl-identifier)
	("C-c P" . my-godoc-package)
	("{" . my-go-electric-brace)
	("C-i" . company-indent-or-complete-common)
	("C-M-i" . company-indent-or-complete-common))
  :config
  ;; run gofmt/goimports when saving the file
  (add-hook 'before-save-hook #'gofmt-before-save)

  (defun my-go-mode-hook-fn ()
    (go-eldoc-setup)
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode)
    (smartparens-mode 1)
    (flycheck-mode 1)
    (setq imenu-generic-expression
          '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
            ("func" "^func *\\(.*\\) {" 1))))

  (add-hook 'go-mode-hook #'my-go-mode-hook-fn))
;; Go/speedbar integration

(eval-after-load 'speedbar
  '(speedbar-add-supported-extension ".go"))

(defun my-c-mode-hook()
  "Hooks for 'c-mode'."
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (require 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  )
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  "Hooks for 'c++-mode'."
  (setq c-basic-offset 4)
  )
(add-hook 'c++-mode-hook 'my-c++-mode-hook)


(load-file "~/configs/emacs/org-mode.el")
(load-file "~/configs/emacs/gnus.el")

(provide 'init)

;; Temp workaround
(defalias 'lsp--cur-line-diagnotics 'lsp--cur-line-diagnostics)
;;; init.el ends here
