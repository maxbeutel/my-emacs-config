; enable all commands
(setq disabled-command-function nil)

;; y-n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; include paths
(setenv "PATH" (concat "/Users/max/.cargo/bin:/usr/local/bin:/opt:" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/opt")))
(setq exec-path (append exec-path '("/Users/max/go/bin")))
(setq exec-path (append exec-path '("/Applications/Racket\ v7.6/bin/racket")))

;; install required packages

; list the packages you want
(setq package-list '(popup color-theme flx-ido flx async helm-git-grep helm helm-core helm-projectile dash projectile pkg-info epl php-mode web-mode zenburn-theme dired+ helm-ag crontab-mode magit expand-region helm-swoop org diff-hl scss-mode yasnippet flycheck cmake-mode string-inflection avy icicles bookmark+ neotree go-mode smartparens ess))

;; package manager and include path
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                     ("marmalade" . "https://marmalade-repo.org/packages/")
                     ("org" . "http://orgmode.org/elpa/")
                     ("melpa" . "http://melpa.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; general ui
(add-to-list 'default-frame-alist '(font . "Consolas-20"))

; highlight current line
(global-hl-line-mode 1)

;; Highlighting of current line background color, don't
;; disable syntax coloring.
(set-face-attribute 'hl-line nil :inherit nil :background "#222222")
(set-face-attribute 'highlight nil :inherit nil :background "#222222")
(set-face-attribute 'region nil :inherit nil :background "#222222")


(dolist (mode '(menu-bar-mode
                tool-bar-mode
                tooltip-mode
                scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; emacs mac
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; no bell sound
(setq visible-bell 1)

;; explicitly initialize packages
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; enable auto-save of desktop
(desktop-save-mode 1)

(setq savehist-file (locate-user-emacs-file "savehist")
      savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60

      history-length 10000)

(savehist-mode +1)

;; Maxima
(add-to-list 'load-path "/usr/local/Cellar/maxima/5.41.0/share/maxima/5.41.0/emacs")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))

;; eshell
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 35)

;; show paren
(show-paren-mode 1)

;; show changes in fringe, use simple symbols
(setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
(global-diff-hl-mode 1)

;; re-builder
(setq reb-re-syntax 'string)

;; Keep up with compile output
(setq compilation-scroll-output t)

;; show line numbers on the left hand side, show column in lower bar
(global-linum-mode t)
(setq column-number-mode t)

;; lisp
(setq inferior-lisp-program "sbcl")
(load (expand-file-name "~/quicklisp/slime-helper.el"))

;; Racket
(setq racket-program "/Applications/Racket v7.6/bin/racket")

;; c-mode
;; (setq c-basic-indent 4)
;; (setq c-default-style "linux"
;;       c-basic-offset 4
;;       c-indent-level 4)

(add-hook 'c-mode-common-hook
          (lambda ()
             (c-set-offset 'case-label '+)
             (c-set-offset 'inclass '4)
             (c-set-offset 'comment-intro 0)))
(add-hook 'c-mode-hook 'google-set-c-style)
(add-hook 'c++-mode-hook 'google-set-c-style)

;; go mode
(setq gofmt-command "goimports")

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'gofmt-before-save)))

;; nxml-mode
(setq nxml-child-indent 4
      nxml-attribute-indent 4)

(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq line-number-display-limit-width 2000000)

;; load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; Enable rainbow delimeters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; smartparens
;; for more see https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
;(smartparens-global-mode 1)

;; (add-hook 'php-mode-hook #'turn-on-smartparens-strict-mode)
;; (add-hook 'c-mode-hook #'turn-on-smartparens-strict-mode)
;; (add-hook 'rust-mode-hook #'turn-on-smartparens-strict-mode)
;; (add-hook 'c++-mode-hook #'turn-on-smartparens-strict-mode)

;; (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

;; (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
;; (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
;; (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

;; (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

;; (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)

; rebind defun start/end to avoid conflict with smartparens keybindings
(global-set-key (kbd "C-M-<") 'beginning-of-defun)
(global-set-key (kbd "C-M->") 'end-of-defun)

;; list of recent files
(global-set-key "\C-x\ \C-b" 'helm-recentf)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; flycheck
(global-flycheck-mode)

(setq-default flycheck-disabled-checkers '(php-phpmd php-phpcs emacs-lisp-checkdoc rust-cargo))
(setq-default flycheck-php-executable "/usr/local/bin/php")
(setq-default flycheck-rust-executable "/Users/max/.cargo/bin/rustc")
(setq-default flycheck-rust-cargo-executable "/Users/max/.cargo/bin/cargo")

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; rust
(setq-default rustfmt-bin "/Users/max/.cargo/bin/rustfmt")
(setq racer-rust-src-path "/Users/max/Documents/playground/rust/rustc-1.13.0/src")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'racer-mode-hook #'company-mode)

(setq company-tooltip-align-annotations t)

;; avy
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-line)

;; bookmark+
(setq bmkp-light-style-autonamed 'line)
(setq bmkp-light-style-non-autonamed 'line)

(setq bmkp-auto-light-when-set 'any-bookmark)
(setq bmkp-auto-light-when-jump 'any-bookmark)

;; icicle
(icy-mode 1)

(setq-default icicle-default-thing-insertion "more-of-the-same")

;; clean up trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)
(setq mode-require-final-newline t)

;; check for file changes (let's see how this works)
(global-auto-revert-mode t)

;; crontab mode
(add-to-list 'auto-mode-alist '("crontab\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("/crontab_" . crontab-mode))

;; cmake mode
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

;; yansippet
(setq yas-snippet-dirs '("~/Documents/playground/emacs/my-emacs-config/yasnippet-snippets"))

(yas-global-mode 1)

;; helm
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; swiper
(global-set-key (kbd "C-c o") 'swiper)

;; dired
(diredp-toggle-find-file-reuse-dir 1)

(add-hook 'dired-mode-hook 'auto-revert-mode)

(require 'dired-x)

; ignore emacs backup files in dir listing
(setq-default dired-omit-files-p t)
(setq dired-omit-files "\\~$")

;; enable subword mode
(subword-mode 1)

;; auto-complete
;;(ac-config-default)

;; calendar config

; make mondays first day of week
(setq calendar-week-start-day 1)

;; projectile
(projectile-global-mode)

(setq projectile-use-git-grep 1)

;; flx-ido
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; web-mode settings (also for mixed php/html files)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)


(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; php-mode only
;;(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)

;; scss mode
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; enable theme
;(load-theme 'zenburn t)
;(load-theme 'zerodark t)
(load-theme 'monochrome t)
;; Optionally setup the modeline
;;(zerodark-setup-modeline-format)

;; For monochrome theme (and maybe others), don't change the font size of org-mode headlines
(custom-theme-set-faces 'user
   `(org-level-1 ((t (:bold t :foreground ,"light gray" :height 1.0))))
   `(org-level-2 ((t (:bold nil :foreground ,"light gray" :height 1.0))))
   `(org-level-3 ((t (:bold t :foreground ,"light gray" :height 1.0))))
   `(org-level-4 ((t (:bold nil :foreground ,"light gray" :height 1.0))))
   `(org-link ((t (:foreground ,"light slate gray" :underline t))))
   `(org-todo ((t (:bold t :foreground "red"))))
   `(org-done ((t (:bold t :foreground "green")))))

;; Globally disable italics
(set-face-italic 'font-lock-comment-face nil)

; make cursor blink forever
(setq blink-cursor-blinks 0)

;; show clock
(display-time-mode 1)

;; compile

; Make the compilation window automatically disappear - from enberg on #emacs
;; (setq compilation-finish-functions
;;       (lambda (buf str)
;;         (if (null (string-match ".*exited abnormally.*" str))
;;             ;;no errors, make the compilation window go away in a few seconds
;;             (progn
;;               (run-at-time
;;                "1 sec" nil 'delete-windows-on
;;                (get-buffer-create "*compilation*"))
;;               (message "No Compilation Errors!")))))

; abt specific compile targets
(setq multi-compile-alist '(
    (go-mode . (("00 unit-test" . "cd ~/Documents/zalora/abt/ && make unit-test")
                ("11 build" . "cd ~/Documents/zalora/abt/ && make")
                ("22 static-check" . "cd ~/Documents/zalora/abt/ && make static-check")
                ("33 inte-test" . "cd ~/Documents/zalora/abt/ && make integration-test")
                  ))
    ))

(setq multi-compile-completion-system 'helm)

;; isearch
(defun zap-to-isearch (rbeg rend)
    "Kill the region between the mark and the closest portion of
  the isearch match string. The behaviour is meant to be analogous
  to zap-to-char; let's call it zap-to-isearch. The deleted region
  does not include the isearch word. This is meant to be bound only
  in isearch mode.
  The point of this function is that oftentimes you want to delete
  some portion of text, one end of which happens to be an active
  isearch word. The observation to make is that if you use isearch
  a lot to move the cursor around (as you should, it is much more
  efficient than using the arrows), it happens a lot that you could
  just delete the active region between the mark and the point, not
  include the isearch word."
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
    "Exit isearch, but at the other end of the search string.
  This is useful when followed by an immediate kill."
    (interactive "r")
    (isearch-exit)
    (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;; custom functions
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

(global-set-key (kbd "C-c t") 'toggle-window-split)

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

(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil) t))))

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

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun toggle-quotes ()
  (interactive)
  (save-excursion
    (let ((start (nth 8 (syntax-ppss)))
          (quote-length 0) sub kind replacement)
      (goto-char start)
      (setq sub (buffer-substring start (progn (forward-sexp) (point)))
            kind (aref sub 0))
      (while (char-equal kind (aref sub 0))
        (setq sub (substring sub 1)
              quote-length (1+ quote-length)))
      (setq sub (substring sub 0 (- (length sub) quote-length)))
      (goto-char start)
      (delete-region start (+ start (* 2 quote-length) (length sub)))
      (setq kind (if (char-equal kind ?\") ?\' ?\"))
      (loop for i from 0
            for c across sub
            for slash = (char-equal c ?\\)
            then (if (and (not slash) (char-equal c ?\\)) t nil) do
            (unless slash
              (when (member c '(?\" ?\'))
                (aset sub i
                      (if (char-equal kind ?\") ?\' ?\")))))
      (setq replacement (make-string quote-length kind))
      (insert replacement sub replacement))))

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(eval-after-load "helm-regexp"
    '(setq helm-source-moccur
           (helm-make-source "Moccur"
               'helm-source-multi-occur :follow 1)))

(defun my-helm-multi-all ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))

(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))

(global-set-key (kbd "C-c m") 'move-buffer-file)

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

(global-set-key (kbd "C-c b") 'beginning-of-buffer)
(global-set-key (kbd "C-c e") 'end-of-buffer)

(global-set-key (kbd "M-s q") 'hi-lock-mode)

(global-set-key (kbd "C-c l") 'er/expand-region)

(global-set-key (kbd "C-c q") 'toggle-quotes)

(global-set-key (kbd "C-c k") 'copy-line)

(global-set-key (kbd "C-c a") 'align-regexp)

(global-set-key (kbd "C-c w") 'forward-whitespace)

(global-set-key (kbd "C-c n") 'neotree-dir)

(global-set-key (kbd "C-c s") 'ansi-term)

(global-set-key (kbd "C-c j") 'recompile)

(global-set-key (kbd "C-c x") 'erase-buffer)

(global-set-key (kbd "M-k") 'delete-region)

(global-set-key (kbd "M-p") 'sp-splice-sexp)

(global-set-key (kbd "C-x o") 'ace-window)

(global-set-key (kbd "C-c i") 'multi-compile-run)

;; -------
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(icicle-command-abbrev-alist (quote ((kill-buffer ## 2)))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "f5ad3af69f2b6b7c547208b8708d4fa7928b5697ca0845633d1d67c2d145952a" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" default)))
 '(icicle-command-abbrev-alist (quote ((query-replace b 1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "yellow")))))
