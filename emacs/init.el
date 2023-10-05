;; Performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setenv "LSP_USE_PLISTS" "1")

;; Setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Behaviour modifications
(setq inhibit-startup-message t)
(global-display-line-numbers-mode 1)
(setq make-backup-files nil)
(electric-pair-mode t)
(global-hl-line-mode t)
(load-theme 'modus-vivendi t)

;; Packages and package configurations
(straight-use-package 'use-package)

(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-log-io nil)
  (setq lsp-idle-delay 0.500)
  (setq lsp-use-plists t)
  (setq warning-suppress-types '((lsp-mode)))
  ;; (setq lsp-keymap-prefix "s-l")
  :hook ((prog-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)
(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)
(use-package which-key
  :straight t
  :config
  (which-key-mode))
(use-package company
  :straight t)
(use-package lsp-treemacs
  :straight t)
(use-package flycheck
  :straight t)
(use-package dap-mode
  :straight t)

(use-package geiser-guile
  :straight t)
(use-package sml-mode
  :straight t)

(use-package pomm
  :straight t
  :commands (pomm pomm-third-time)
  :config
  (setq  pomm-audio-enabled t)
  (setq alert-default-style 'libnotify)
  :custom
  (pomm-state-file-location "~/.config/emacs/extras/pomm")
  (pomm-third-time-state-file-location "~/.config/emacs/extras/pomm-third-time")
  (pomm-long-break-period 15)
  (pomm-ask-before-long-break nil))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
