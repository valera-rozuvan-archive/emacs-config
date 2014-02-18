;; Include tabs. Each file will have it's own tab for easy
;; switching between open files.
(add-to-list 'load-path "~/.emacs.d/tabbar")
(require 'tabbar)
(tabbar-mode)


;; Define and enable tab group function that defines all tabs to be one
;; of two possible groups: “emacs” and “user”.
(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
    "Returns the name of the tab group names the current buffer belongs to.
    There are two groups: Emacs buffers (those whose name starts with '*', plus
    dired buffers), and the rest.  This works at least with Emacs v24.2 using
    tabbar.el v1.7."
    (list
        (cond
            ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
            ((eq major-mode 'dired-mode) "emacs")
            (t "user")
        )
    )
)
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)


;; Easy navigation between tabbar tabs.
(global-set-key (kbd "<prior>") 'tabbar-backward-tab)
(global-set-key (kbd "<next>") 'tabbar-forward-tab)


;; Easy navigation between tabbar groups.
(global-set-key (kbd "<home>") 'tabbar-backward-group)
(global-set-key (kbd "<end>") 'tabbar-forward-group)


;; Include a major mode for editing MarkDown files.
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))


;; Include a major mode for editing Yaml files.
(add-to-list 'load-path "~/.emacs.d/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))


;; Include a major mode for editing CoffeeScript files.
(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))


;; Include haml-mode. This mode is required by sass-mode (see below).
(add-to-list 'load-path "~/.emacs.d/haml-mode")
(require 'haml-mode)


;; Include a major mode for editing Sass files.
(add-to-list 'load-path "~/.emacs.d/sass-mode")
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))


;; Turn on ruby-mode for Rake files.
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))


;; Disable auto-save and auto-backup.
(setq auto-save-default nil)
(setq make-backup-files nil)


;; On save, remove trailing whitespace from all lines.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Turn indenting off for all modes that we use.
(defun my-turn-indentation-off ()
    (interactive)
    (local-set-key (kbd "<tab>") 'tab-to-tab-stop))
(dolist (hook '(emacs-lisp-mode-hook
    text-mode-hook
    fundamental-mode-hook
    python-mode-hook
    js-mode-hook
    coffee-mode-hook
    sass-mode-hook
    ruby-mode-hook
    yaml-mode-hook
    markdown-mode-hook))
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


;; Disabling the Menu Bar, and Toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)


;; Automatically reload files when they are modified by external programs.
(global-auto-revert-mode 1)
;; Display a warning about it.
(defun inform-revert-modified-file (&optional p1 p2)
    "bdimych custom function"
    (let ((revert-buffer-function nil))
        (revert-buffer p1 p2)
        (message-box (concat "[\"" (buffer-file-name) "\"]: Reloaded file from disk."))
    )
)
(setq revert-buffer-function 'inform-revert-modified-file)
