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

(defun move-text-internal (arg)
	(cond
	 ((and mark-active transient-mark-mode)
		(if (> (point) (mark))
				(exchange-point-and-mark))
		(let ((column (current-column))
					(text (delete-and-extract-region (point) (mark))))
			(forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
	 (t
		(beginning-of-line)
		(when (or (> arg 0) (not (bobp)))
			(forward-line)
			(when (or (< arg 0) (not (eobp)))
				(transpose-lines arg))
			(forward-line -1)))))

(defun move-text-down (arg)
	"Move region (transient-mark-mode active) or current line
  arg lines down."
	(interactive "*p")
	(move-text-internal arg))

(defun move-text-up (arg)
	"Move region (transient-mark-mode active) or current line
  arg lines up."
	(interactive "*p")
   (move-text-internal (- arg)))

(global-set-key [\C-\M-up] 'move-text-up)
(global-set-key [\C-\M-down] 'move-text-down)