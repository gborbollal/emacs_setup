;; Key bindings to move around and edit code

(global-set-key (kbd "C-<tab>") 'switch-to-buffer)
(global-set-key (kbd "s-}") (lambda () (interactive) (select-window (next-window))))
(global-set-key (kbd "s-{") (lambda () (interactive) (select-window (previous-window))))
(global-set-key (kbd "s-l") 'switch-to-buffer)
(global-set-key (kbd "s-w") (lambda () (interactive) (kill-current-buffer)))
(global-set-key (kbd "s-P") 'helm-find-files)
(global-set-key (kbd "s-p") 'find-file)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "S-s-<up>") 'move-text-up)
(global-set-key (kbd "S-s-<down>") 'move-text-down)
(global-set-key (kbd "s-a") 'move-beginning-of-line)
(global-set-key (kbd "s-d") 'duplicate-current-line-or-region)
(global-set-key (kbd "s-e") 'eval-last-sexp)
(global-set-key (kbd "M-s-®") "C-c C-k C-x o M-p")
(global-set-key (kbd "s-O") 'helm-browse-project)
(global-set-key (kbd "s-S") 'helm-projectile-ag)
(global-set-key (kbd "s-9") 'magit-status)


(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
