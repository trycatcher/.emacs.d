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
  :ensure t)
(global-set-key (kbd "s-m") 'magit-status)

(use-package no-littering
  :ensure t
  :config
  ;; TODO: Might also want to do this for tramp files
  (setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
    `((".*" . (no-littering-expand-var-file-name "backup/"))))
  ;; Move custom configuration variables set by Emacs, to a seperate file
  (setq custom-file
    (no-littering-expand-etc-file-name "custom.el")))
