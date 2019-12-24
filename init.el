;;; package --- Summary
;;
;; init.el - Abhik's Emacs configuration
;;
;; Copyright 2019 Abhik Khanra
;;
;; Author: Abhik Khanra <abhik.rk@gmail.com>
;;
;;; Commentary:
;;
;; This is my personal Emacs configuration.
;;
;;; License:
;;
;; FIXME: Add license
;;
;;; Code:

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(global-linum-mode 1)
(global-visual-line-mode 1)
(column-number-mode 1)
; (delete-selection-mode 1)

(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)
;; The :ensure keyword causes the package(s) to be installed automatically if not already present on your system:
(setq use-package-always-ensure t)

;; Do not use tabs for indentation
(setq-default indent-tabs-mode nil)
;; Merge indentation offset and tab-width variables
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "s-m") 'magit-status))

(use-package no-littering
  :ensure t
  :config
  ;; TODO: Might also want to do this for tramp files
  (setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
    `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  ;; Move custom configuration variables set by Emacs, to a seperate file
  (setq custom-file
    (no-littering-expand-etc-file-name "custom.el")))

(use-package smex
  :ensure t
  :config
  (let ((smex-dir (concat (no-littering-expand-var-file-name "smex"))))
    (unless (file-exists-p smex-dir)
      (make-directory smex-dir) ;; smex is not no-littering compatible yet
      (setq smex-save-file
        (concat smex-dir "smex-save.el"))))
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

;; Use company-mode for in-buffer autocompletion
(use-package company
  :ensure t
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (global-company-mode)
  (setq company-tooltip-limit 10)
  (setq company-idle-delay 0.2)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-require-match nil)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  ;; weight by frequency
  (setq company-transformers '(company-sort-by-occurence)))

(use-package solarized-theme
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :defer t)

(use-package monokai-theme
  :ensure t
  :defer t)

(use-package gruvbox-theme
  :ensure t
  :defer t)

(use-package paradox
  :ensure t
  :config
  (paradox-enable))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d%d "))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)))

(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'")

(use-package yaml-mode
  :ensure t
  :config
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))

(use-package repl-toggle
  :ensure t)

(use-package purescript-mode
  :ensure t
  :defer t
  :config
  (setq browse-url-browser-function 'eww-browse-url))

(use-package psci
  :ensure t
  :defer t
  :init
  (add-hook 'purescript-mode-hook 'inferior-psci-mode)
  (add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci)))

(use-package psc-ide
  :ensure t
  :defer t
  :init
  (add-hook 'purescript-mode-hook 'psc-ide-mode)
  (add-hook 'purescript-mode-hook 'company-mode)
  (add-hook 'purescript-mode-hook 'flycheck-mode)
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
  :config
  (setq psc-ide-rebuild-on-save t)
  ;; FIXME: This isn't working
  (setq psc-ide-add-import-on-completion t))

;; Better syntax highlighting for Clojure
(use-package clojure-mode-extra-font-locking
  :ensure t
  :defer t)

(use-package paredit
  :ensure t
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

;; To add some colors to parens
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :defer t
  :init
  (setq cider-repl-pop-to-buffer-on-connect t
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/var/cider-history"
        cider-repl-wrap-history t
        cider-repl-history-size 100
        cider-repl-use-clojure-font-lock t
        cider-docview-fill-column 70
        cider-stacktrace-fill-column 76
        ;; Stop error buffer from popping up while working in buffers other than REPL:
        nrepl-popup-stacktraces nil
        nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-repl-use-pretty-printing t
        cider-repl-result-prefix ";; => ")
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook
            (local-set-key (kbd "<C-return>") 'cider-eval-defun-at-point)))

(use-package clj-refactor
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1))))


;; Aggressively indent your clojure code
(use-package aggressive-indent
  :ensure t
  :defer t
  :commands (aggressive-indent-mode)
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

(use-package ox-reveal
  :ensure t
  :defer t
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  ;; FIXME: Add MathJax support
  ;(setq org-reveal-mathjax  )
  )

(use-package htmlize
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package intero
  :ensure t
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package hindent
  :ensure t
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'hindent-mode))

(use-package flymake-hlint
  :ensure t
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'flymake-hlint-load))

(use-package hlint-refactor
  :ensure t
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'hlint-refactor-mode))

(winner-mode 1)

(set-frame-font "Inconsolata-12" )

(load-theme 'solarized-light t)

;;; init.el ends here
