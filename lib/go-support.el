(defun go-fmt-current-buffer ()
	"Format the current buffer using goreturns"
	(interactive)
	(if (not (eq (string-match "\\.go$" (buffer-file-name)) nil))
			(let ((retcode (call-process "goreturns" nil nil nil "-w"
																	 (buffer-file-name))))
				(if (= retcode 0)
					(revert-buffer nil t)))))

(defun init-go-hooks ()
	"Initialize Go related hooks"
	(add-hook 'after-save-hook 'go-fmt-current-buffer))
