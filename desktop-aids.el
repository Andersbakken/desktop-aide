(defvar-local --desktop-aids-pending nil)

(defvar desktop-aids-buffer-filter nil)
;; (setq desktop-aids-buffer-filter (lambda (buffer filename) (string-match "rtags.el$" filename)))

(defun --desktop-aids-current-window-configurations ()
  "nil")
;; (mapconcat (lambda (frame)
;;                )
;;              (frame-list)))

(defun --desktop-aids-encode ()
  (concat "(cons\n (list "
          (mapconcat (lambda (buffer)
                       (let ((name (buffer-file-name buffer)))
                         (cond ((not name) nil)
                               ((and (functionp desktop-aids-buffer-filter)
                                     (not (funcall desktop-aids-buffer-filter buffer name))) nil)
                               (t
                                (format "\n'(\"%s\" . %d)"
                                        name
                                        (with-current-buffer buffer
                                          (point)))))))
                     (buffer-list)
                     "")
          ")\n " (--desktop-aids-current-window-configurations) ")"))

(defconst --desktop-aids-file-default (concat user-emacs-directory "desktop-aids-saved"))
(defcustom desktop-aids-file --desktop-aids-file-default
  "File to store desktop aids to."
  :group 'desktop-aids
  :type 'string
  :safe 'stringp)

(defcustom desktop-aids-silent nil
  "Be more silent when loading files."
  :group 'desktop-aids
  :type 'boolean
  :safe 'booleanp)

(defun --desktop-aids-file (&optional file)
  (expand-file-name (or file desktop-aids-file (concat user-emacs-directory "desktop-aids-saved"))))

(defun --desktop-aids-insert-file-contents (filename)
  (condition-case nil
      (insert-file-contents filename)
    (error nil)))

(defun desktop-aids-save (&optional file)
  (interactive)
  (with-temp-buffer
    (insert (--desktop-aids-encode))
    (let ((ret t))
      (condition-case nil
          (write-region (point-min) (point-max) (--desktop-aids-file file))
        (error
         (message "Failed to save desktop-aids file %s" (--desktop-aids-file file))
         (setq ret nil)))
      ret)))

(defun --desktop-aids-kill-emacs-hook ()
  (or (desktop-aids-save) t))

(defun desktop-aids-load (&optional file recreate)
  (interactive)
  (with-temp-buffer
    (if (not (--desktop-aids-insert-file-contents (--desktop-aids-file file)))
        (progn
          (unless desktop-aids-silent
            (message "desktop-aids: Failed to restore"))
          nil)
      (let ((count 0)
            (failed 0)
            (saved (condition-case nil
                       (eval (read (current-buffer))))
                   (error nil)))
        (mapc (lambda (desc)
                (when recreate
                  (let ((cur (find-buffer-visiting (car desc))))
                    (when cur
                      (kill-buffer cur))))
                (let ((buffer (get-buffer (file-name-nondirectory (car desc)))))
                  (unless buffer
                    (setq buffer (get-buffer-create (file-name-nondirectory (car desc))))
                    (with-current-buffer buffer
                      (if (not (--desktop-aids-insert-file-contents (car desc)))
                          (progn
                            (incf failed)
                            (unless desktop-aids-silent
                              (message "desktop-aids: Failed to restore buffer %s" (car desc)))
                            (kill-buffer buffer))
                        (setq buffer-file-name (car desc))
                        (setq default-directory (file-name-directory (car desc)))
                        (goto-char (min (point-max) (cdr desc)))
                        (set-buffer-modified-p nil)
                        (setq --desktop-aids-pending t)
                        (incf count 1)
                        (add-hook 'post-command-hook '--desktop-aids-post-command-hook nil t))))))
              (car saved))
        (unless desktop-aids-silent
          (message "desktop-aids: Restored %d buffers%s" count
                   (if (> failed 0)
                       (format ", failed to restore %d buffers" failed)
                     "")))
        t))))

(defun --desktop-aids-post-command-hook ()
  (when (buffer-local-value '--desktop-aids-pending (current-buffer))
    (kill-local-variable '--desktop-aids-pending)
    (remove-hook 'post-command-hook '--desktop-aids-post-command-hook t)
    (set-auto-mode)))

(define-minor-mode desktop-aids-mode
  "Desktop aids, the mode that gives"
  :lighter " da"
  :global t
  (if desktop-aids-mode
      (add-hook 'kill-emacs-hook '--desktop-aids-kill-emacs-hook)
    (remove-hook 'kill-emacs-hook '--desktop-aids-kill-emacs-hook)))

  ;; (message "desktop-aids-mode %s" (if desktop-aids-mode "enabled" "off")))

(provide 'desktop-aids)
