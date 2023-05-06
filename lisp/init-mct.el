;;;;; init-mct.el --- MCT-*- lexical-binding: t -*-

;;; Install packages
(straight-use-package '(mct :type git :host gitlab :repo "protesilaos/mct"))

;;; MCT
(setq mct-hide-completion-mode-line t)
(setq mct-completion-passlist '(consult-location embark-keybinding imenu))
(setq mct-remove-shadowed-file-names t)
(setq mct-completion-window-size (cons #'mct-frame-height-third 1))
(setq mct-persist-dynamic-completion nil)
(setq mct-live-completion 'visible)
(setq mct-live-update-delay 0.5)

(mct-mode 1)

(defun p-sort-by-alpha-length (elems)
  (sort elems (lambda (c1 c2)
                (or (string-version-lessp c1 c2)
                    (< (length c1) (length c2))))))

(defun p-annotation-buffer-file (buffer)
  (when-let ((name (buffer-file-name (get-buffer buffer))))
    (format " %s" (abbreviate-file-name name))))

(defun p-annotation-buffer (&rest app)
  (let ((completion-extra-properties `(:annotation-function ,#'p-annotation-buffer-file)))
    (apply app)))

(advice-add #'read-buffer :around #'p-annotation-buffer)

(provide 'init-mct)
;;;;; init-mct.el ends here
