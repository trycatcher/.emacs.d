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

;; Start maximised (cross-platform)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
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

(use-package quelpa)

(use-package quelpa-use-package
  :after (quelpa))

(use-package keychain-environment)

(use-package magit
  :bind ("s-m" . 'magit-status))

(use-package no-littering
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
  :config
  (let ((smex-dir (concat (no-littering-expand-var-file-name "smex"))))
    (unless (file-exists-p smex-dir)
      (make-directory smex-dir) ;; smex is not no-littering compatible yet
      (setq smex-save-file
            (concat smex-dir "smex-save.el"))))
  :bind (("M-x" . 'smex)
         ("M-X" . 'smex-major-mode-commands)))

;; Use company-mode for in-buffer autocompletion
(use-package company
  :config (global-company-mode))

(use-package solarized-theme)

(use-package zenburn-theme)

(use-package monokai-theme)

(use-package gruvbox-theme)

(use-package paradox
  :config
  (paradox-enable))

(use-package which-key
  :config
  (which-key-mode))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d%d "))

(use-package swiper
  :bind
  ("C-s" . 'swiper))

(use-package counsel
  :bind
  (("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package ace-window
  :bind
  (("s-w" . 'ace-window)
   ([remap other-window] . 'ace-window)))

(use-package avy
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)))

(use-package adoc-mode
  :mode "\\.adoc\\'")

(use-package yaml-mode
  :bind
  (:map yaml-mode-map
        ("C-m" . 'newline-and-indent)))

(use-package repl-toggle)

(use-package purescript-mode
  :config
  (setq browse-url-browser-function 'eww-browse-url))

(use-package psci
  :hook
  ((purescript-mode-hook . inferior-psci-mode)
   (purescript-mode . company-mode)
   (purescript-mode . flycheck-mode)
   (purescript-mode-hook . turn-on-purescript-indentation))
  :config
  (add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci)))

(use-package psc-ide
  :hook
  (purescript-mode . psc-ide-mode)
  :config
  (setq psc-ide-rebuild-on-save t)
  ;; FIXME: This isn't working
  (setq psc-ide-add-import-on-completion t))

;; Better syntax highlighting for Clojure
(use-package clojure-mode-extra-font-locking)

(use-package paredit
  :hook
  (emacs-lisp-mode . enable-paredit-mode))

;; To add some colors to parens
(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :hook
  (clojure-mode . enable-paredit-mode))

(use-package cider
  :config
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
  :hook
  ((cider-mode . eldoc-mode)
   (cider-repl-mode . enable-paredit-mode)
   (cider-repl-mode . company-mode)
   (cider-mode . company-mode)
   (cider-mode . (lambda ()
                   (local-set-key (kbd "<C-return>") 'cider-eval-defun-at-point)))))

(use-package clj-refactor
  :hook
  (clojure-mode . (lambda ()
                    (clj-refactor-mode 1))))

;; Aggressively indent your clojure code
(use-package aggressive-indent
  :hook
  (clojure-mode . aggressive-indent-mode))

(use-package ox-reveal
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  ;; FIXME: Add MathJax support
  ;(setq org-reveal-mathjax  )
  )

(use-package htmlize)

(use-package rust-mode
  :bind
  (:map rust-mode-map
        (("C-c C-c" . 'rust-run)
         ("C-c C-k" . 'rust-compile)
         ("C-c C-t" . 'rust-test)
         ("C-c C-l" . 'rust-run-clippy)
         ("C-c C-d" . 'rust-debug-wrap-or-unwrap)))
  :hook
  ((rust-mode . lsp-deferred)
   (rust-mode . (lambda ()
                  (setq indent-tabs-mode nil)
                  (setq rust-format-on-save t)))))

(use-package elpy
  :init
  (elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  :hook
  ((elpy-mode . flycheck-mode)
   (elpy-mode . (lambda ()
                  (add-hook 'before-save-hook 'elpy-black-fix-code nil t)))))

(use-package json-mode)

(use-package sml-mode)

(use-package racket-mode
  :hook
  ((racket-mode . enable-paredit-mode)
   (racket-repl-mode . enable-paredit-mode)))

(winner-mode 1)

(use-package lsp-haskell)

(use-package typescript-mode)

(use-package web-mode
  :hook (typescript-tsx-mode . lsp)
  :mode (("\\.html\\'" . web-mode)
         ("\\.tsx\\'" . typescript-tsx-mode))
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript-tsx")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-comment-style 2
        web-mode-enable-css-colorization t))

(use-package prettier
  :hook ((typescript-tsx-mode . prettier-mode)
         (typescript-mode . prettier-mode)
         (js-mode . prettier-mode)
         (json-mode . prettier-mode)
         (css-mode . prettier-mode)
         (scss-mode . prettier-mode)))

(use-package lsp-mode
  :hook ((haskell-mode . lsp)
         (haskell-literate-mode . lsp))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-keymap-prefix "C-c l")
  (lsp-enable-which-key-integration t))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package slime
  :preface
  ;; Stop SLIME's REPL from grabbing DEL,
  ;; when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key)
      nil))
  :config
  (setq inferior-lisp-program "sbcl")
  :hook
  ((slime-repl-mode . enable-paredit-mode)
   (slime-repl-mode . override-slime-repl-bindings-with-paredit)))

(use-package zig-mode)

(use-package go-mode
  :preface
  (defun lsp-go-save ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :hook ((go-mode . lsp-deferred)
         (go-mode . lsp-go-save)))

(use-package nix-mode
  :mode "\\.nix\\'")

(set-frame-font "Fira code-12")

(load-theme 'solarized-light t)

;;; init.el ends here
