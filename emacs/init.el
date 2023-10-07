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
(global-display-line-numbers-mode 1)
(electric-pair-mode t)
(global-hl-line-mode t)
(load-theme 'modus-vivendi t)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq x-select-enable-clipboard t)

;; Packages and package configurations
(straight-use-package 'use-package)

(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-log-io nil)
  (setq lsp-idle-delay 0.25)
  (setq lsp-use-plists t)
  (setq warning-suppress-types '((lsp-mode)))
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  ;; (setq lsp-keymap-prefix "s-l")
  :hook ((prog-mode . lsp-deferred)
         ;; enable which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package company
  :straight t
  ;; :config
  ;; (setq company-idle-delay 0.2)
  )
  ;; :hook (after-init . global-company-mode))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package lsp-treemacs
  :straight t
  :bind (("M-o" . treemacs)))

(use-package flycheck
  :straight t)

(use-package dap-mode
  :straight t)

(use-package geiser-guile
  :straight t)

(use-package sml-mode
  :straight t)

(use-package typescript-mode
  :straight t
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :straight t
  :config
  (setq js2-basic-offset 2))

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

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))
