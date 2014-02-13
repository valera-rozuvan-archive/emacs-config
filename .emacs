;; Disable auto-save and auto-backup.
(setq auto-save-default nil)
(setq make-backup-files nil)


;; On save, remove trailing whitespace from all lines.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Turn all indenting off.
(defun my-turn-indentation-off ()
    (interactive)
    (local-set-key (kbd "<tab>") 'tab-to-tab-stop))
(dolist (hook '(perl-mode-hook
    cperl-mode-hook
    c-mode-hook
    c++-mode-hook
    java-mode-hook
    emacs-lisp-mode-hook
    javascript-mode-hook))
(add-hook hook 'my-turn-indentation-off))


;; Always replace tabs with spaces.
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)


;; Set tab width to 4 for all buffers.
(setq-default tab-width 4)
(custom-set-variables '(tab-width 4))


;; The default tab stops are 8 spaces appart.
;; Using some manual editing, we change this list.
;; Tabs will be 4, 8, 12, 16, 20, ..., 108, 112, 116, 120 spaces apart.
(setq tab-stop-list (number-sequence 4 120 4))


;; Disable the start-up splash screen.
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
