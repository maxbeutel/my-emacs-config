;; package manager and include path

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")))

;; include paths
(setenv "PATH" (concat "/usr/local/bin:/opt:" (getenv "PATH")))
(setq exec-path (append exec-path '("/Applications/Racket\ v7.6/bin/racket")))
(setq exec-path (append exec-path '("/Users/max/go/bin")))
(setq exec-path (append exec-path '("/usr/local/texlive/2017/bin/x86_64-darwin")))

;; extend load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path "/usr/local/Cellar/maxima/5.41.0/share/maxima/5.41.0/emacs")

;; General ui
(add-to-list 'default-frame-alist '(font . "Consolas-20"))

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
(global-linum-mode t)
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
(setq desktop-dirname "~/.emacs.d/"
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

;; Packages

;; Syntax checking
(use-package flycheck
             :ensure t
             :config
             (global-flycheck-mode))

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

;; Make buffer names unique
;; buffernames that are foo<1>, foo<2> are hard to read. This makes them foo|dir  foo|otherdir
(use-package uniquify
             :config (setq uniquify-buffer-name-style 'post-forward))

(use-package diff-hl
  :ensure t
  :init
  ; Show changes in fringe, use simple symbols
  (global-diff-hl-mode 1)
  (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type))

(use-package neotree
  :ensure t
  :init
  (global-set-key (kbd "C-c n") 'neotree-dir))

(use-package expand-region
  :ensure t
  :init
  (global-set-key (kbd "C-c l") 'er/expand-region))

(use-package ace-window
  :ensure t
  :init
  (global-set-key (kbd "C-x o") 'ace-window))

(use-package avy
  :ensure t
  :init
  (global-set-key (kbd "C-;") 'avy-goto-char)
  (global-set-key (kbd "C-:") 'avy-goto-line))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package hydra
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package ivy-hydra
  :ensure t
  :after (ivy hydra))

(use-package swiper
  :ensure t
  :after (ivy)
  :init
  ; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key (kbd "C-c o") 'swiper))

(use-package counsel
  :ensure t
  :after (ivy)
  :init
  (setq counsel-find-file-ignore-regexp "\\~$\\'")
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "<f2> j") 'counsel-set-variable)
  ;; (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  ;; (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package dockerfile-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (setq projectile-use-git-grep 1))

(use-package go-mode
  :ensure t
  :init
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook
	    (lambda () (add-hook 'before-save-hook #'gofmt-before-save))))

(use-package dired+
  :load-path "~/.emacs.d/packages/dired+"
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package dired-x
  :init
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (setq-default dired-omit-files-p t)
  (setq dired-omit-verbose 0)
  (setq dired-omit-files "\\~$"))

(use-package crontab-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("crontab\\'" . crontab-mode))
  (add-to-list 'auto-mode-alist '("/crontab_" . crontab-mode)))

(use-package nxml-mode
  :commands nxml-mode
  :init
  (setq nxml-child-indent 4
	nxml-attribute-indent 4))

;; TODO: do we need flx ido?
(use-package ido-mode
  :commands ido-mode
  :init
  (ido-mode 1)
  (ido-everywhere 1))

(use-package org
  :ensure t
  :after (yasnippet)
  :config

  (visual-line-mode 1)

  (yas-minor-mode 1)

  :init

  (org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))

  ;; Don't ask for confirmation when evaluating code blocks
  (setq org-confirm-babel-evaluate nil)

  (define-key org-mode-map (kbd "C-c C-x C-e") 'org-babel-execute-src-block)

  ;; Override latex header to set document class to 'standalone'
  ;; This allows rendering PDFs that have the same size as the tikz chart.
  ;; Useful for embedding latex/tikz images in org documents.
  (setq org-format-latex-header
   "\\documentclass[tikz, border=1mm]{standalone}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove")

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

  (setq org-log-done t
        org-agenda-files '("/Users/max/Documents/org")))

(use-package string-inflection
  :ensure t)

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package google-c-style
  :ensure t)

(use-package c-mode
  :commands c-mode
  :init
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (c-set-offset 'case-label '+)
	      (c-set-offset 'inclass '4)
	      (c-set-offset 'comment-intro 0)))
  (add-hook 'c-mode-hook 'google-set-c-style)
  (add-hook 'c++-mode-hook 'google-set-c-style)
  :after (google-c-style))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("/Users/max/Documents/playground/emacs/my-emacs-config/snippets"))))

(use-package racket-mode
  :ensure t
  :init
  (setq racket-program "/Applications/Racket v7.6/bin/racket"))

(use-package maxima
  :init
  (setq imaxima-use-maxima-mode-flag t)
  (add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode)))

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

;; Custom functions

(defun mb/org-new-snippet()
  "Copy a snippet I saved from the sync folder to some other folder, so that I can add notes for it."
  (interactive)
  (let (prog-dir file-list target-dir snippets-dir sync-dir file-list-sorted start-file start-file-full end-file-full end-file-full-org)
    ;; The location of my sync program
    (setq prog-dir "/Users/max/Documents/playground/golang/google-drive-sync-latest")
    ;; Where to store the images, so that org-mode can pick them up
    (setq sync-dir "/Users/max/Documents/org/sync-snippets")

    (setq snippets-dir "/Users/max/Documents/org/Snippets")

    (let ((default-directory prog-dir)
          (cmd (concat "go run main.go 66_ORG_SNIPPETS " sync-dir " ./credentials.json")))
      (message cmd)
      (message (shell-command-to-string cmd)))

    (setq file-list
          (-remove (lambda (x) (nth 1 x))
                   (directory-files-and-attributes sync-dir)))

  ;; Sort list by most recent
  ;; http://stackoverflow.com/questions/26514437/emacs-sort-list-of-directories-files-by-modification-date
  (setq file-list-sorted
        (mapcar #'car
                (sort file-list
                      #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))

  (setq start-file (ivy-read
                    (concat "Select file:")
                    file-list-sorted
                    :re-builder #'ivy--regex
                    :sort nil
                    :initial-input nil))

  (message "start file is %s" start-file)

  (setq start-file-full (expand-file-name start-file sync-dir))
  (message "Start file full: %s" start-file-full)

  (setq end-file-full (concat (file-name-as-directory snippets-dir) (file-name-nondirectory start-file-full)))
  (message "End file full: %s" end-file-full)

  (setq end-file-full-org (concat end-file-full ".org"))
  (message "Corresponding .org file: %s" end-file-full-org)

  (unless (file-exists-p snippets-dir)
    (make-directory snippets-dir))

  (unless (file-exists-p end-file-full-org)
    (copy-file start-file-full end-file-full))

  (message "Copied %s to %s" start-file-full end-file-full)

  (unless (file-exists-p end-file-full-org)
    (write-region "" nil end-file-full-org))

  (message "Done with copying around.")

  ;; Open the corresponding .org file
  (find-file end-file-full-org)

  ;; Open the downloaded snippet file (most likely a PDF)
  (find-file end-file-full)
  ;;(shell-command (concat "open " end-file-full))
))

(defun mb/org-insert-image()
  "Copy image from a directory, insert to same directory as screenshots, add link in current file."
  (interactive)

  (let (prog-dir file-list target-dir sync-dir file-list-sorted start-file start-file-full end-file-full)
    ;; The location of my sync program
    (setq prog-dir "/Users/max/Documents/playground/golang/google-drive-sync-latest")
    ;; Where to store the images, so that org-mode can pick them up
    (setq sync-dir "/Users/max/Documents/org/sync-images")

    (let ((default-directory prog-dir)
          (cmd (concat "go run main.go 77_ORG_IMAGES " sync-dir " ./credentials.json")))
      (message cmd)
      (message (shell-command-to-string cmd)))

    (setq file-list
          (-remove (lambda (x) (nth 1 x))
                   (directory-files-and-attributes sync-dir)))

  ;; Sort list by most recent
  ;; http://stackoverflow.com/questions/26514437/emacs-sort-list-of-directories-files-by-modification-date
  (setq file-list-sorted
        (mapcar #'car
                (sort file-list
                      #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))

  (setq start-file (ivy-read
                    (concat "Select file:")
                    file-list-sorted
                    :re-builder #'ivy--regex
                    :sort nil
                    :initial-input nil))

  (setq start-file-full (expand-file-name start-file sync-dir))

  (message "Start file full")
  (message start-file-full)

  (setq end-file-full
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (file-name-sans-extension start-file)
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".jpg"))

  (unless (file-exists-p (file-name-directory end-file-full))
    (make-directory (file-name-directory end-file-full)))

  (message "End file full")
  (message end-file-full)

  (copy-file start-file-full end-file-full)
  (message "Copied %s to %s" start-file-full end-file-full)

  (if (file-exists-p end-file-full)
      (insert (concat "[[file:" end-file-full "]]")))

  (org-display-inline-images t t)))

(defun mb/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))

  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))

  ; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]")))

  (org-display-inline-images t t))

(defun mb/rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun mb/uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun mb/copy-full-path-to-kill-ring ()
  "Copy buffer's full path to kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))

;; General keybindings
(global-set-key (kbd "C-c r") 'mb/rename-file-and-buffer)

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

(global-set-key (kbd "C-c b") 'beginning-of-buffer)
(global-set-key (kbd "C-c e") 'end-of-buffer)

(global-set-key (kbd "M-s h q") 'hi-lock-mode)

(global-set-key (kbd "C-c k") 'copy-line)

(global-set-key (kbd "C-c a") 'align-regexp)

(global-set-key (kbd "C-c j") 'recompile)

(global-set-key (kbd "C-c x") 'erase-buffer)

;; NOTE:
;; - Below I'm customizing the scale factor of the Latex preview for org mode in `org-format-latex-options`.
;; - Also customizing doc-view path to ghostscript, which was not found although it was in the PATH.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-ghostscript-program "/usr/local/bin/gs")
 '(package-selected-packages
   (quote
    (tuareg multiple-cursors yasnippet org-bullets yaml-mode dockerfile-mode use-package tao-theme string-inflection rainbow-delimiters racket-mode projectile neotree monochrome-theme molokai-theme minimal-theme ivy-hydra google-c-style go-mode expand-region diff-hl crontab-mode counsel ace-window)))
 '(yas-snippet-dirs
   (quote
    ("/Users/max/Documents/playground/emacs/my-emacs-config/snippets"))))
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
