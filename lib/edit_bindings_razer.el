;; Key bindings to move around and edit code

(global-set-key (kbd "C-<tab>") 'switch-to-buffer)
(global-set-key (kbd "M-}") (lambda () (interactive) (select-window (next-window))))
(global-set-key (kbd "M-{") (lambda () (interactive) (select-window (previous-window))))
(global-set-key (kbd "C-l") 'switch-to-buffer)
(global-set-key (kbd "C-q") (lambda () (interactive) (kill-current-buffer)))
(global-set-key (kbd "C-p") 'helm-find-files)
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "C-d") 'duplicate-current-line-or-region)

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
