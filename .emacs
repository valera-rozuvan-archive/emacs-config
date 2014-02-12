;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
