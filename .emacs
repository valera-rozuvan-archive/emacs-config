;; Include feature-mode for editing cucumber Gherkin (feature)
;; files.
(add-to-list 'load-path "~/.emacs.d/feature-mode")
(setq feature-default-language "fi")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))


;; Include tabs. Each file will have it's own tab for easy
;; switching between open files.
(add-to-list 'load-path "~/.emacs.d/tabbar")
(require 'tabbar)

;; Tabbar settings.
;; Taken from:
;;     https://gist.github.com/3demax/1264635
(set-face-attribute
    'tabbar-default nil
    :background "gray20"
    :foreground "gray20"
    :box '(:line-width 1 :color "gray20" :style nil)
)
(set-face-attribute
    'tabbar-unselected nil
    :background "gray30"
    :foreground "white"
    :box '(:line-width 5 :color "gray30" :style nil)
)
(set-face-attribute
    'tabbar-selected nil
    :background "gray75"
    :foreground "black"
    :box '(:line-width 5 :color "gray75" :style nil)
)
(set-face-attribute
    'tabbar-highlight nil
    :background "white"
    :foreground "black"
    :underline nil
    :box '(:line-width 5 :color "white" :style nil)
)
(set-face-attribute
    'tabbar-button nil
    :box '(:line-width 1 :color "gray20" :style nil)
)
(set-face-attribute
    'tabbar-separator nil
    :background "gray20"
    :height 0.6
)

;; Change padding of the tabs. We also need to set separator to avoid
;; overlapping tabs by highlighted tabs.
(custom-set-variables
    '(tabbar-separator (quote (0.5)))
)

;; Ading spaces.
(defun tabbar-buffer-tab-label (tab)
    "Return a label for TAB.
    That is, a string used to represent it on the tab bar."
    (let (
            (
                label (if tabbar--buffer-show-groups
                    (format "[%s] " (tabbar-tab-tabset tab))
                    (format "%s " (tabbar-tab-value tab))
                )
            )
        )

        ;; Unless the tab bar auto scrolls to keep the selected tab
        ;; visible, shorten the tab label to keep as many tabs as possible
        ;; in the visible area of the tab bar.
        (if tabbar-auto-scroll-flag
            label
            (tabbar-shorten
                label (max 1
                    (/ (window-width)
                        (length (tabbar-view
                            (tabbar-current-tabset))
                        )
                    )
                )
            )
        )
    )
)

;; Always enable the tabbar.
(tabbar-mode 1)


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
(global-set-key [C-prior] 'tabbar-backward-tab)
(global-set-key [C-next] 'tabbar-forward-tab)


;; Easy navigation between tabbar groups.
(global-set-key [M-prior] 'tabbar-backward-group)
(global-set-key [M-next] 'tabbar-forward-group)


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


;; Turn on ruby-mode for Rake files, and Vagrantfile files.
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Vagrantfile\\'" . ruby-mode))


;; Include a major mode for editing gitignore files.
(add-to-list 'load-path "~/.emacs.d/gitignore-mode")
(require 'gitignore-mode)
(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . gitignore-mode))


;; Set-up JSLint.
;; First enable flymake-easy mode.
;; (add-to-list 'load-path "~/.emacs.d/flymake-easy")
;; (require 'flymake-easy)
;; Then enable flymake-jslint mode.
;; (add-to-list 'load-path "~/.emacs.d/flymake-jslint")
;; (require 'flymake-jslint)
;; Enable JSLint for JavaScript mode.
;; (add-hook 'js-mode-hook 'flymake-jslint-load)


;; Set-up JSHint.
(add-to-list 'load-path "~/.emacs.d/jshint-mode")
(require 'flymake-jshint)
(add-hook 'js-mode-hook
     (lambda () (flymake-mode t)))
(add-to-list 'auto-mode-alist '("\\.jshintrc\\'" . js-mode))
;; Enable JSHint for JavaScript mode.
;; (add-hook 'js-mode-hook 'flymake-jslint-load)
;; (add-hook 'find-file-hook 'flymake-find-file-hook)


;; Enable popup mode.
(add-to-list 'load-path "~/.emacs.d/popup-el")
(require 'popup)


;; Enable auto-complete mode.
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(ac-config-default)


;; Disable auto-save and auto-backup.
(setq auto-save-default nil)
(setq make-backup-files nil)


;; On save, remove trailing whitespace from all lines.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Turn indenting off on TAB and RETURN key
;; press for all modes that we use.
(defun no-indentation-for-tab ()
    (interactive)
    (local-set-key (kbd "<tab>") 'tab-to-tab-stop)
)
(defun set-newline-for-return ()
    (interactive)
    (local-set-key (kbd "RET") 'newline)
)
(dolist (
    hook '(
        emacs-lisp-mode-hook
        text-mode-hook
        fundamental-mode-hook
        python-mode-hook
        js-mode-hook
        coffee-mode-hook
        sass-mode-hook
        ruby-mode-hook
        yaml-mode-hook
        markdown-mode-hook
        feature-mode-hook
        shell-script-mode-hook
        sh-mode-hook
    ))
    (add-hook hook 'no-indentation-for-tab)
    (add-hook hook 'set-newline-for-return)
)


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


;; If a file has been modified on disk, ask what to do.
;; Taken from and modified:
;;     http://stackoverflow.com/questions/7031051/emacs-notify-when-a-file-has-been-modified-externally
(defun ask-user-about-supersession-threat (fn) "blatantly ignore files that changed on disk")
(run-with-timer 0 2 'my-check-external-modifications)
(add-hook 'after-save-hook 'my-check-external-modifications)
(add-hook 'after-revert-hook 'my-check-external-modifications)
(defun my-load-external-modifications ()
    (interactive)
    (if (buffer-modified-p)
        (revert-buffer) ; ask for confirmation
        (revert-buffer t t) ; don't ask for confirmation - it's unnecessary, since the buffer hasn't been modified
    )
    (my-check-external-modifications)
)
(defun my-overwrite-external-modifications ()
    (interactive)
    (clear-visited-file-modtime)
    (set-buffer-modified-p (current-buffer))
    (save-buffer)
    (my-check-external-modifications)
)
(defun my-check-external-modifications ()
    (if (verify-visited-file-modtime (current-buffer))
        (progn
            (global-set-key (kbd "<f5>") 'my-load-external-modifications)
            (global-set-key (kbd "<f6>") 'save-buffer)
            (setq header-line-format tabbar-header-line-format)
        )
        (progn
            (global-set-key (kbd "<f5>") 'my-load-external-modifications)
            (global-set-key (kbd "<f6>") 'my-overwrite-external-modifications)
            (setq header-line-format
                (format
                    "%s. Press F5 to load external changes, F6 to overwrite them"
                    (propertize
                        "This file has been changed externally"
                        'face
                        '(:foreground "#f00")
                    )
                )
            )
        )
    )
)


;; Configure the theme and the font size.
;; (load-theme 'deeper-blue t)
(load-theme 'adwaita t)
(set-face-attribute 'default nil :height 140)


;; Make sure that the current position (line and column number) of the
;; cursor is shown.
(setq line-number-mode t)
(setq column-number-mode t)


;; Enable the display of date and time in mode line.
(setq
    display-time-day-and-date t
    display-time-24hr-format t
)
(display-time-mode t)

(server-start)
