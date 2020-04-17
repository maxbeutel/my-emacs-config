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
(setq exec-path (append exec-path '("/opt")))
(setq exec-path (append exec-path '("/Applications/Racket\ v7.6/bin/racket")))

;; extend load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; general ui
(add-to-list 'default-frame-alist '(font . "Consolas-20"))

; enable all commands
(setq disabled-command-function nil)

; y-n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

; highlight current line
; show changes in fringe, use simple symbols
;;(global-diff-hl-mode 1)
;;(setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)

; no bell sound
(setq ring-bell-function 'ignore)

; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 35)

; show paren
(show-paren-mode 1)

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
(setq desktop-dirname             "~/.emacs.d/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   30)

(desktop-save-mode 1)

(setq savehist-file (locate-user-emacs-file "savehist")
      savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      history-length 10000)

(savehist-mode +1)

;; Theme

;; Packages
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
			  `(org-link ((t (:foreground ,"light slate gray" :underline t))))
			  `(org-todo ((t (:bold t :foreground "red"))))
			  `(org-done ((t (:bold t :foreground "green")))))

  ; Highlighting of current line background color, don't disable syntax coloring.
  (global-hl-line-mode 1)
  (set-face-attribute 'hl-line nil :inherit nil  :foreground nil :background "#222222")
  (set-face-attribute 'highlight nil :inherit nil :background "#666666")
  (set-face-attribute 'region nil :inherit nil :background "#666666")
  
  ; Disable italics font face
  (set-face-italic 'font-lock-comment-face nil))

;; custom functions
(defun zap-to-isearch (rbeg rend)
    (interactive "r")
    (when (not mark-active)
      (error "Mark is not active"))
    (let* ((isearch-bounds (list isearch-other-end (point)))
           (ismin (apply 'min isearch-bounds))
           (ismax (apply 'max isearch-bounds))
           )
      (if (< (mark) ismin)
          (kill-region (mark) ismin)
        (if (> (mark) ismax)
            (kill-region ismax (mark))
          (error "Internal error in isearch kill function.")))
      (isearch-exit)
      ))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)

(defun isearch-exit-other-end (rbeg rend)
    (interactive "r")
    (isearch-exit)
    (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))


;; Key bindings




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (minimal-theme use-package tao-theme monochrome-theme molokai-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
