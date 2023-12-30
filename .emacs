;; package manager and include path


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; include paths
(setenv "PATH" (concat "/usr/local/bin:/opt:/Library/TeX/texbin:" (getenv "PATH")))
(add-to-list 'exec-path "/Library/TeX/texbin")

;; extend load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; General UI
;;; TODO: Need to install Consolas again
(set-frame-font "Menlo 20")

; enable all commands
(setq disabled-command-function nil)

; y-n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

; no bell sound
(setq ring-bell-function 'ignore)

; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 35)

; make cursor blink forever
(setq blink-cursor-blinks 0)

;; show clock
(display-time-mode 1)

; show line numbers on the left hand side, show column in lower bar
(global-display-line-numbers-mode 1)
(setq column-number-mode t)

; re-builder
(setq reb-re-syntax 'string)

; Keep up with compile output
(setq compilation-scroll-output t)

; enable subword mode
(subword-mode 1)

; indentation
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq line-number-display-limit-width 2000000)

; make mondays first day of week
(setq calendar-week-start-day 1)

; emacs mac
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

; Disable some menus
(dolist (mode '(menu-bar-mode
                tool-bar-mode
                tooltip-mode
                scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

; enable auto-save of desktop
(setq desktop-dirname "~/"
      desktop-base-file-name "emacs.desktop"
      desktop-base-lock-name "lock"
      desktop-path (list desktop-dirname)
      desktop-save t
      desktop-files-not-to-save "^$" ; reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout 30)

(desktop-save-mode 1)

(setq savehist-file (locate-user-emacs-file "savehist")
      savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      history-length 10000)

(savehist-mode +1)

; clean up trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)
(setq mode-require-final-newline t)

; check for file changes
(global-auto-revert-mode t)

; Highlight matching parenthesis
(show-paren-mode 1)
(setq show-paren-priority -50)
(setq hl-line-overlay-priority -100)

; Allows moving through wrapped lines as they appear
(setq line-move-visual t)

; Wrap long lines
(setq-default truncate-lines nil)

; ######### Removed everything package related for now

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package dockerfile-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package python-mode
  :ensure t)

(use-package minimal-theme
  :ensure t
  :config
  (load-theme 'minimal-black t)

  ; Don't change font-size of org-mode headlines
  (custom-theme-set-faces 'user
        		  `(org-level-1 ((t (:bold t :foreground ,"light gray" :height 1.0))))
        		  `(org-level-2 ((t (:bold nil :foreground ,"light gray" :height 1.0))))
        		  `(org-level-3 ((t (:bold t :foreground ,"light gray" :height 1.0))))
        		  `(org-level-4 ((t (:bold nil :foreground ,"light gray" :height 1.0))))
        		  `(org-level-5 ((t (:bold nil :foreground ,"light gray" :height 1.0))))
        		  `(org-link ((t (:foreground ,"light slate gray" :underline t))))
        		  `(org-todo ((t (:bold t :foreground "red"))))
        		  `(org-done ((t (:bold t :foreground "green")))))

  (set-cursor-color "#FF6F65")
  (custom-set-faces '(cursor ((t (:background "#FF6F65")))))

  ; Highlighting of current line background color, don't disable syntax coloring.
  (global-hl-line-mode 0)
  ; (set-face-attribute 'hl-line nil :inherit nil  :background "#222")
  (set-face-attribute 'highlight nil :inherit nil :foreground "#000" :background "lime")
  (set-face-attribute 'region nil :inherit nil :foreground "#000" :background "lime")
  (set-face-attribute 'lazy-highlight nil :foreground "#000" :background "lime")
  (set-face-attribute 'isearch nil :foreground "#000" :background "lime")
  (set-face-attribute 'isearch-fail nil :foreground "#FFF" :background "red")

  ; Disable italics font face
  (set-face-italic 'font-lock-comment-face nil))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("/Users/max/Documents/my-emacs-config/snippets"))))

(use-package expand-region
  :ensure t
  :init
  (global-set-key (kbd "C-c l") 'er/expand-region))

;; Autocomplete popups
(use-package company
             :ensure t
             :config
             (progn
               (setq company-idle-delay 0.2
                     ;; min prefix of 2 chars
                     company-minimum-prefix-length 2
                     company-selection-wrap-around t
                     company-show-numbers t
                     company-echo-delay 0
                     company-tooltip-limit 20
                     company-transformers '(company-sort-by-occurrence)
                     company-begin-commands '(self-insert-command)
                     )
               (global-company-mode))
             )

(use-package avy
  :ensure t
  :init
  (global-set-key (kbd "C-;") 'avy-goto-char)
  (global-set-key (kbd "C-:") 'avy-goto-line))

;; For the LaTex integration:
;;
;; If packages are not found, install them with `sudo tlmgr install wrapfig.sty`
;; Error messages look like:
;;    File `wrapfig.sty' not found.
;;
;; This can happen since we installed only basictex via brew
(use-package org
  :ensure t
  :after (yasnippet)
  :config

  (visual-line-mode 1)

  :init

  (yas-minor-mode 1)

  (org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))

  ;; Don't ask for confirmation when evaluating code blocks
  (setq org-confirm-babel-evaluate nil)

  (setq org-image-actual-width nil)

  (define-key org-mode-map (kbd "C-c C-x C-l") 'org-toggle-latex-fragment)
  (define-key org-mode-map (kbd "C-c C-x C-p") 'org-latex-export-to-pdf)
  (define-key org-mode-map (kbd "C-c C-x C-e") 'org-babel-execute-src-block)

  ;; Not exactly sure what this is, maybe HTML options for org mode export?
  (setq org-format-latex-options
   (quote
    (:foreground default :background default :scale 2.2 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :mpatchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))

  (setq org-todo-keyword-faces
        '(
          ("TODO" . (:foreground "Red" :weight bold))
          ("ONGOING" . (:foreground "Yellow" :weight bold))
          ("REFERENCE" . (:foreground "DarkOrchid" :weight bold))
          ("DOCUMENTED" . (:foreground "SpringGreen" :weight bold))
          ("STOPPED" . (:foreground "MistyRose" :weight bold))
          ("DONE" . (:foreground "Green" :weight bold))
          ))

  (setq org-todo-keywords
        '((sequence "ONGOING" "TODO" "REFERENCE" "DOCUMENTED" "STOPPED" "DONE")))

)

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


;; General keybindings
;;; Not used for now
;;;(global-set-key (kbd "C-c r") 'mb/rename-file-and-buffer)

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

(global-set-key (kbd "C-c b") 'beginning-of-buffer)
(global-set-key (kbd "C-c e") 'end-of-buffer)

(global-set-key (kbd "M-s h q") 'hi-lock-mode)

(global-set-key (kbd "C-c k") 'copy-line)

(global-set-key (kbd "C-c a") 'align-regexp)

(global-set-key (kbd "C-c j") 'recompile)

(global-set-key (kbd "C-c x") 'erase-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimeters-mode yasnippet yaml-mode vertico rainbow-mode rainbow-delimiters python-mode org-bullets minimal-theme expand-region dockerfile-mode company avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#FF6F65"))))
 '(org-done ((t (:bold t :foreground "green"))))
 '(org-level-1 ((t (:bold t :foreground "light gray" :height 1.0))))
 '(org-level-2 ((t (:bold nil :foreground "light gray" :height 1.0))))
 '(org-level-3 ((t (:bold t :foreground "light gray" :height 1.0))))
 '(org-level-4 ((t (:bold nil :foreground "light gray" :height 1.0))))
 '(org-level-5 ((t (:bold nil :foreground "light gray" :height 1.0))))
 '(org-link ((t (:foreground "light slate gray" :underline t))))
 '(org-todo ((t (:bold t :foreground "red")))))
