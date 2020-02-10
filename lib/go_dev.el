;; go-mode
(add-to-list 'load-path (substitute-in-file-name "$HOME/lib/emacs/go-mode.el/"))
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(require 'go-autocomplete)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;; (defun set-exec-path-from-shell-PATH ()
;; 	(let ((path-from-shell (replace-regexp-in-string
;; 													"[ \t\n]*$"
;; 													""
;; 													(shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;; 		(setenv "PATH" path-from-shell)
;; 		(setq eshell-path-env path-from-shell)
;; 		(setq exec-path (split-string path-from-shell path-separator))))

;; (setenv "GOPATH" "/Users/gerardo.borbolla/go")
;; (add-to-list 'exec-path (substitute-in-file-name "$GOROOT/bin"))

;; (with-eval-after-load "*.go" (require 'go-autocomplete))

(defun my-go-mode-hook ()
	(add-hook 'before-save-hook 'gofmt-before-save)
	(local-set-key (kbd "s-b") 'godef-jump)
	(local-set-key (kbd "s-[") 'pop-tag-mark)
	(set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
	)

(add-hook 'go-mode-hook 'my-go-mode-hook)


