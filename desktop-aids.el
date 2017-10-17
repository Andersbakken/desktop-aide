(defvar-local --desktop-aids-pending-offset nil)

(defvar --desktop-aids-encode-filter nil)
(setq --desktop-aids-encode-filter (lambda (buffer filename) (string-match "rtags.el$" filename)))
(defun --desktop-aids-encode ()
  (concat "(list "
          (mapconcat (lambda (buffer)
                       (let ((name (buffer-file-name buffer)))
                         (cond ((not name) nil)
                               ((and (functionp --desktop-aids-encode-filter)
                                     (not (funcall --desktop-aids-encode-filter buffer name))) nil)
                               ((buffer-local-value '--desktop-aids-pending-offset buffer)
                                (format "\n'(\"%s\" . %d)"
                                        name
                                        (buffer-local-value '--desktop-aids-pending-offset buffer)))
                               (t
                                (format "\n'(\"%s\" . %d)"
                                        name
                                        (with-current-buffer buffer
                                          (point)))))))
                     (buffer-list)
                     "")
          ")"))

(defvar desktop-aids-file nil)
(defun desktop-aids-file (&optional file)
  (expand-file-name (or file desktop-aids-file (concat user-emacs-directory "desktop-aids-saved"))))

(defun desktop-aids-save (&optional file)
  (interactive)
  (with-temp-buffer
    (insert (--desktop-aids-encode))
    (write-region (point-min) (point-max) (desktop-aids-file file))))

(defun desktop-aids-load (&optional file recreate)
  (interactive)
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents (desktop-aids-file file))
        (mapc (lambda (desc)
                (when recreate
                  (let ((cur (find-buffer-visiting (car desc))))
                    (when cur
                      (kill-buffer cur))))
                (let ((buffer (get-buffer (file-name-nondirectory (car desc)))))
                  (unless buffer
                    (setq buffer (get-buffer-create (file-name-nondirectory (car desc))))
                    (with-current-buffer buffer
                      (setq buffer-file-name (car desc))
                      (setq --desktop-aids-pending-offset (cdr desc))))))
              (eval (read (current-buffer)))))
    (error nil)))

(defun --desktop-aids-insert-file-contents (filename)
  (condition-case nil
      (insert-file-contents filename)
    (error nil)))

(defun --desktop-aids-post-command-hook ()
  (when (buffer-local-value --desktop-aids-pending-offset (current-buffer))
    (--desktop-aids-insert-file-contents (buffer-file-name))
    ;; (condition-case nil
    ;;     (progn
    ;;       (insert-file-contents (buffer-file-name))
    ;;       (set-auto-mode))
    ;;   (error nil))
    (kill-local-variable '--desktop-aids-pending-offset)))

