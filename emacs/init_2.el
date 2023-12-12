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

(use-package emacs
  :init
  (setq make-backup-files nil)
  (load-theme 'wombat t))

(use-package elec-pair
  :straight nil
  :config
  (electric-pair-mode t))

(use-package display-line-numbers
  :straight nil
  :config
  (global-display-line-numbers-mode t))

(use-package eglot
  :straight nil
  :init
  ;; auto start eglot for idris-mode
  (add-hook 'idris-mode-hook 'eglot-ensure)
  :config
  (setq read-process-output-max (* 1024 1024))
  (add-to-list 'eglot-server-programs
	       '(idris-mode . ("idris2-lsp"))))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package company
  :straight t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Idris
;;;;;;;;
(use-package idris-mode
  :straight t
  :custom
  (idris-interpreter-path "idris2")
  :hook
  ;; load opened file, mainly for the semantic syntax highlighting on open
  (idris-mode . idris-load-file))

;; Markdown
;;;;;;;;;;;
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
