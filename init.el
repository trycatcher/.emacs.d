(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(global-linum-mode 1)
(global-visual-line-mode 1)
(column-number-mode 1)
; (delete-selection-mode 1)

;; Move custom configuration variables set by Emacs, to a seperate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
