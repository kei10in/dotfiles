;;;
;;; helm
;;;

(use-package helm
  ;; :ensure 1
  :init
  (require 'helm-config)
  (helm-mode 1)

  (global-set-key (kbd "C-x b") 'helm-for-files)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)

  (add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(dired-at-point . nil))

  (setq helm-ff-auto-update-initial-value nil)
  (setq helm-idle-delay 0.001)
  (setq helm-input-idle-delay 0.001)
  (setq helm-buffer-max-length 40)

  (defvar my/helm-source-files-in-current-dir
    `((name . "Files from Current Directory")
      (candidates . (lambda ()
                      (with-helm-current-buffer
                        (let ((dir (helm-current-directory)))
                          (when (file-accessible-directory-p dir)
                            (directory-files dir nil))))))
      (keymap . ,helm-generic-files-map)
      (no-delay-on-input)
      (help-message . helm-generic-file-help-message)
      (mode-line . helm-generic-file-mode-line-string)
      (type . file)))

  (setq helm-for-files-preferred-list
        '(helm-source-buffers-list
          my/helm-source-files-in-current-dir
          helm-source-recentf
          helm-source-bookmarks
          helm-source-file-cache
          helm-source-buffer-not-found
          )))
