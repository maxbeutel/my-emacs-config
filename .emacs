; enable all commands
(setq disabled-command-function nil)

;; y-n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; include paths
(setenv "PATH" (concat "/usr/local/bin:/opt:" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/opt")))

;; install required packages

; list the packages you want
(setq package-list '(auto-complete popup color-theme flx-ido flx async helm-git-grep helm helm-core helm-projectile dash projectile pkg-info epl php-mode web-mode zenburn-theme dired+ helm-ag crontab-mode magit expand-region helm-swoop org diff-hl scss-mode yasnippet))

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

; make cursor blink forever
(setq blink-cursor-blinks 0)

; highlight current line
(global-hl-line-mode 1)

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

;; show paren
(show-paren-mode 1)

;; show changes in fringe, use simple symbols
(setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
(global-diff-hl-mode 1)

;; Keep up with compile output
(setq compilation-scroll-output t)

;; show line numbers on the left hand side, show column in lower bar
(global-linum-mode t)
(setq column-number-mode t)

;; c-mode
(setq c-basic-indent 4)
(setq c-default-style "linux"
      c-basic-offset 4
      c-indent-level 4)

(add-hook 'c-mode-common-hook
          (lambda ()
             (c-set-offset 'case-label '+)
             (c-set-offset 'inclass '4)
             (c-set-offset 'comment-intro 0)))

;; nxml-mode
(setq nxml-child-indent 4
      nxml-attribute-indent 4)

(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq line-number-display-limit-width 2000000)


(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; clean up trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)
(setq mode-require-final-newline t)

;; check for file changes (let's see how this works)
(global-auto-revert-mode t)

;; crontab mode
(add-to-list 'auto-mode-alist '("crontab\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("/crontab_" . crontab-mode))

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
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

;; dired+
(diredp-toggle-find-file-reuse-dir 1)

(add-hook 'dired-mode-hook 'auto-revert-mode)

;; enable subword mode
(subword-mode 1)

;; auto-complete
(ac-config-default)

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

;; eshell
(setenv "PAGER" "cat")

(defalias 'e 'find-file)
(defalias 'ff 'find-file)
(defalias 'emacs 'find-file)

(setq eshell-prefer-lisp-functions nil)

(global-set-key (kbd "C-c s") 'eshell)

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

;; enable zenburn
(load-theme 'zenburn t)

;; show clock
(display-time-mode 1)

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

(global-set-key (kbd "C-c m") 'move-buffer-file)

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

(global-set-key (kbd "C-c b") 'beginning-of-buffer)
(global-set-key (kbd "C-c e") 'end-of-buffer)

(global-set-key (kbd "M-s q") 'hi-lock-mode)

(global-set-key (kbd "C-c l") 'er/expand-region)

(global-set-key (kbd "C-c q") 'toggle-quotes)

(global-set-key (kbd "C-c k") 'copy-line)

(global-set-key (kbd "C-c j") 'my-helm-multi-all)

;; -------
