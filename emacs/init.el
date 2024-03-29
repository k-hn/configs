;; optimising startup
;;;;;;;;;;;;;;;;;;;;;
;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))
(setq use-package-always-defer t)

;; straight package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package doom-themes
  :straight t)

(use-package theme-changer
  :straight t
  :demand t
  :init
  (setq calendar-latitude 5.55)
  (setq calendar-longitude -0.22)
  :config
  (change-theme 'doom-one-light 'doom-one))

(use-package emacs
  :init
  (setq make-backup-files nil)
  (setq scroll-conservatively 101)
  (global-hl-line-mode t))

(use-package elec-pair
  :straight nil
  :init
  (electric-pair-mode t))

(use-package display-line-numbers
  :straight nil
  :init
  (global-display-line-numbers-mode t))

(use-package eglot
  :straight nil
  :init
  (setq read-process-output-max (* 1024 1024)
	eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs '(idris-mode . ("idris2-lsp"))))

(use-package which-key
  :straight t
  :demand t
  :config
  (setq which-key-idle-delay 0.25)
  (which-key-mode))

(use-package company
  :straight t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package neotree
  :straight t
  :bind ([f2] . 'neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Magit
;;;;;;;;
(use-package magit
  :straight t)

;; C
;;;;
(use-package c-mode
  :straight nil
  :hook (c-mode . eglot-ensure))

;; Idris
;;;;;;;;
(use-package idris-mode
  :straight t
  :custom
  (idris-interpreter-path "idris2")
  :hook
  ;; load opened file, mainly for the semantic syntax highlighting on open
  (idris-mode . idris-load-file)
  (idris-mode . eglot-ensure))

;; Markdown
;;;;;;;;;;;
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
