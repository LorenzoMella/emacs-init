;;;  init.el --- Personal init configuration for general use and programming -*- lexical-binding: t; coding: utf-8 -*-

;;  Copyright (C) 2021-2024 Lorenzo Mella

;;  Author: Lorenzo Mella <lorenzo.mella@hotmail.it>

;; Commentary:

;; 1. org-agenda configuration
;; 2. LaTeX support (`AUCTeX', `pdf-tools', `ebib', etc.)
;; 3. EXWM support: automatically activated if no WM is found (of course, Linux only)
;; 4. nerd-icons


;;;
;;; Quick-access to user-defined variables
;;;


;; Face families meant to replace the placeholder fonts specified in faces.el
(defvar *face-fixed-pitch-family* "Monospace")

(defvar *face-fixed-pitch-serif-family* "Serif")

(defvar *face-variable-pitch-family* "Sans Serif")

;; Binaries and paths
(defvar *org-agenda-paths* '())

(defvar *shell-binary* (getenv "SHELL") ; using the default for the current user
  "Preferred shell to use with term/ansi-term.")

(defvar *python-interpreter-binary* "python3"
  "Preferred Python interpreter.")

(defvar *python-lsp-server-binary* "~/.local/bin/pylsp"
  "Path to the chosen Python LSP Server executable.")

(defvar *typescript-language-server-binary*
  '("~/.local/bin/typescript-language-server" "--stdio")
  "Path to the chosen JS/TS Language Server executable.")

(defvar *gdb-binary* "/usr/bin/gdb"
  "Path to gdb executable.")

(defvar *c/c++-lsp-server-binary* "/usr/local/bin/ccls"
  "Path to the chose C/C++ etc. LSP server executable.")

(defvar *lisp-binary* "sbcl"
  "Path to the Common Lisp interpreter of choice.")

(defvar *scheme-binary* "guile"
  "Path to the Scheme interpreter of choice.")

(defvar *additional-man-paths* '("/opt/local/share/man"))

(defvar *additional-texinfo-paths* '("/opt/local/share/info")
  "List of the nonstandard texinfo paths.")

(defvar *python-virtual-environment-home-path* "~/.virtualenvs"
  "The directory where the Python Virtual Environments are located. Ignored if nil.")

(defvar *python-shell-extra-pythonpaths* '("~/.local/site_packages")
  "Paths to be added to `python-shell-extra-pythonpaths'")

(defvar *tree-sitter-language-sources*
  ;; entries are of the form (LANG . (URL REVISION SOURCE-DIR CC C++)), with the
  ;; cdr (CC C++) optional
  ;; TODO Add json
  '((bash "https://github.com/tree-sitter/tree-sitter-bash.git" "master" "src")
    (c "https://github.com/tree-sitter/tree-sitter-c.git" "master" "src")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp.git" "master" "src")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript.git" "master" "src")
    (python "https://github.com/tree-sitter/tree-sitter-python.git" "master" "src"))
  "Grammar retrieval information to populate `treesit-language-source-alist'.")

(defvar *texlive-bin-path* "/usr/local/texlive/bin"
  "Path to the TeXlive binaries.")

(defvar *additional-bin-paths* '("~/.local/bin" "~/.pyenv/shims" "/usr/local/bin")
  "List of paths to additional binaries.")

(defvar *additional-auto-modes*
  '(("\\.pgpass\\'" . conf-mode)
    ("\\.sbclrc\\'" . lisp-mode)))

(defvar *sql-product* 'postgres
  "The default SQL dialect in sql-mode buffers.")

(defvar *sql-connection-alist*
  '(("postgres"
     (sql-product 'postgres)
     (sql-user "myname")
     (sql-server "localhost")
     (sql-database "postgres")
     (sql-port 5432)))
  "Database user/role configuration.")

(defvar *ein-image-viewer* "/bin/feh --image-bg white"
  "Image viewing program used by the ein package (with arguments).")

;; Other settings
(defvar *gc-bytes* (* 50 1024 1024) ; 50MB
  "Preferred heap threshold size to start garbage collection.")

(defvar *custom-file-name*
  (expand-file-name "custom-file.el" user-emacs-directory)
  "`Customize' will save its settings in this file.
Set it to `nil' to append to this file.")

(defvar *transparency-level* '(95 95)
  "Frame transparency parameter.")

(defvar *preferred-browser* 'browse-url-default-browser
  "Any one of the browser symbols defined by the browse-url package.")

(defvar *latex-preview-scaling-in-org* 2.0
  "Scaling of Latex image previews in Org Mode.")

(defvar *initial-scratch-message*
  (expand-file-name "initial-scratch-message.txt" user-emacs-directory)
  "Path to text file including a custom *scratch* buffer message.")

(defvar *dashboard-logo* 'logo)


;;;
;;; General purpose custom functions
;;;


(defun lm/init-show ()
  "Opens the configuration file currently defined as `user-init-file'."
  (interactive)
  (find-file-existing user-init-file))

(defalias 'init-show 'lm/init-show)

(defun lm/frame-resize-and-center (&optional width-fraction)
  "Resizes the frame to about two thirds of the screen."
  (interactive '(0.618))  ; Using the inverted golden ratio in place of 2/3
  (let* ((workarea (frame-monitor-attribute 'workarea))
	 (new-width (floor (* (caddr workarea) width-fraction)))
	 (new-height (cadddr workarea))
	 (top-left-x (+ (car workarea) (/ (- (caddr workarea) new-width) 2)))
	 (top-left-y (cadr workarea)))
    (set-frame-position (window-frame) top-left-x top-left-y)
    ;; TODO: somehow it is not centered. Fringes problem
    (set-frame-size (window-frame) new-width new-height t)))

(defun lm/cycle-line-wrap-modes ()
  "Cycles through `visual-line-mode', the simple default line-wrap,
and line truncation."
  (interactive)
  (cond (visual-line-mode
	 (visual-line-mode -1)
	 (toggle-truncate-lines -1))
	(truncate-lines
	 (visual-line-mode 1)
	 (message "Visual Line Mode enabled"))
	(t    ; state here = (not truncate-lines)
	 (toggle-truncate-lines 1))))

(cl-defun lm/adjust-transparency (alpha &key (frame (window-frame)) (background nil))
  "Quick interactive transparency adjustment."
  (interactive "NEnter transparency level: ")
  (set-frame-parameter frame (if background 'alpha-background 'alpha) alpha))

(defalias 'adjust-transparency 'lm/adjust-transparency)

(defun lm/string-from-file (path)
  "Returns the content of a text file as a string."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun lm/macos-move-file-to-trash (path)
  "Replacement to `move-file-to-trash' using the native MacOS 'Move
to Bin' functionality. This is necessary if we want the 'Put
Back' menu option to be available when right-clicking on items
that have been moved to the Bin."
  (call-process-shell-command
   (format
    "osascript -e 'tell application \"Finder\" to move POSIX file \"%s\" to trash'"
    (string-remove-suffix "/" (expand-file-name path)))))


(defun lm/browse-url-of-dired-marked-files (&optional secondary)
  (interactive "P")
  (dolist (file (dired-get-marked-files nil nil nil nil t))
    (let ((browse-url-browser-function
	   (if secondary
	       browse-url-secondary-browser-function
	     browse-url-browser-function))
	  ;; Some URL handlers open files in Emacs.  We want to always
	  ;; open in a browser, so disable those.
	  (browse-url-default-handlers nil))
      (if file
	  (browse-url-of-file (expand-file-name file))
	(error "No file on this line")))))


(defun lm/whitespace-cleanup-notify (cleanup-fn &rest args)
  (let ((modified-tick (buffer-modified-tick)))
    (apply cleanup-fn args)
    (if (> (buffer-modified-tick) modified-tick)
	(message "Excess whitespace deleted")
      (message "(No whitespace to clean up)"))))


(defun lm/string-from-file-content (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))


;;;
;;; Package management
;;;


(require 'package)

;; Add references to online repositories other than GNU ELPA
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")) ; only effective with Emacs 28 or earlier
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Package management will be handled by use-package
(when (and (version< emacs-version "29.0.60") ; earliest version that I verified comes with `use-package'
	   (not (package-installed-p 'use-package)))
  (package-refresh-contents)
  (package-install 'use-package))

;; Keep ELPA keys up to date
(use-package gnu-elpa-keyring-update
  :ensure t)


;;;
;;; `Customize' setup
;;;


(setq custom-file *custom-file-name*)

(unless (file-exists-p *custom-file-name*)
  (make-empty-file *custom-file-name*))


;;;
;;; Process behavior of the current Emacs session
;;;


;; Set the heap threshold for garbage collection
(customize-set-variable 'gc-cons-threshold *gc-bytes*)

;; Server configuration
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))


;;;
;;; Basic interface customizations
;;;


;; Unbind toxic MacOS keybindings
(when (eq (window-system) 'ns)
  (unbind-key "s-o")
  (unbind-key "s-&")
  (unbind-key "s-k"))

;; General keybindings

(require 'bind-key)

;; Remap commands to more convenient keys (the defaults still work)
(bind-key "M-o" #'other-window)
(bind-key "C-M-;" #'comment-line)
(unless (eq (window-system) 'ns)	; Already in place on MacOS
  (bind-key "s-=" #'text-scale-increase)
  (bind-key "s--" #'text-scale-decrease)
  (bind-key "s-u" #'revert-buffer))

;; Remap the insert-char shortcuts
(bind-key "s-8" 'iso-transl-ctl-x-8-map key-translation-map)
(bind-key "s-8 RET" 'counsel-unicode-char)

;; Window-resizing keybindings
(unbind-key "s-m") ; Normally bound to `iconify-frame' on MacOS. Use `C-z' instead
(bind-key "s-m f" #'toggle-frame-fullscreen)
(bind-key "s-m m" #'toggle-frame-maximized)
(bind-key "s-m c" #'lm/frame-resize-and-center)
(bind-key "s-m s" #'window-toggle-side-windows)

;; Remap keys to more convenient commands
(bind-key [remap kill-buffer] #'kill-current-buffer)
(bind-key [remap kill-buffer] #'quit-window ; Never kill *scratch* by accident
	  lisp-interaction-mode-map (string= (buffer-name) "*scratch*"))
(bind-key [remap capitalize-word] #'capitalize-dwim) ; the dwim-versions work on regions too
(bind-key [remap downcase-word] #'downcase-dwim)
(bind-key [remap upcase-word] #'upcase-dwim)

;; Other keybindings
(bind-key "s-m `" #'lm/cycle-line-wrap-modes)


;; Always visualize column numbers
(column-number-mode)

;; Replace/delete the active region when keys/backspace are pressed
(delete-selection-mode)

;; Follow symlinks when calling find-file (useful for git awareness)
(customize-set-variable 'find-file-visit-truename t)

;; Give more leeway to the fill column
(customize-set-variable 'fill-column 80)

;; No French spacing
(customize-set-variable 'sentence-end-double-space nil)

;; Quicken many confirmation prompts
(if (version< emacs-version "28.1")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (customize-set-variable 'use-short-answers t))

;; Mouse configuration

(use-package mwheel
  :custom
  (mouse-wheel-tilt-scroll t
    "Horizontal scrolling on touchpads, Apple Magic Mouse and mice with lateral wheel click")
  (mouse-wheel-flip-direction t
    "Natural orientation for horizontal scrolling")
  :config
  ;; These are for scrolling even when the Emacs frame of interest is in not the
  ;; active GUI window, to achieve Mac-like behavior (works under Gnome Shell
  ;; 3.x and 40-43)
  (when (eq (window-system) 'x)
    (bind-key "<s-mouse-4>" #'mwheel-scroll)
    (bind-key "<s-mouse-5>" #'mwheel-scroll)))

(use-package pixel-scroll
  :init
  ;; TODO Ideally it should only activate with Magic Mouse or similar
  ;; smooth-scrolling devices
  (when (eq (window-system) 'ns)
    (pixel-scroll-precision-mode)))

(use-package xt-mouse
  :init
  (unless (window-system)
    (xterm-mouse-mode)))

;; Display Buffer customization
(use-package window
  :custom
  (display-buffer-alist
   `(("\\*eldoc\\*"
      display-buffer-in-side-window
      (side . bottom)
      (slot . 0)
      (dedicated . t)
      (window-parameters . ((no-other-window . t)))))))

;; Whitespace
(use-package whitespace
  :init
  (advice-add 'whitespace-cleanup :around #'lm/whitespace-cleanup-notify)
  :bind
  ("C-c w w" . whitespace-mode)
  ("C-c w c" . whitespace-cleanup))

;; Tab Bar customization
(use-package tab-bar
  :bind
  ;; TODO Implement repeat
  ("C-x t <right>" . tab-bar-move-tab)
  ("C-x t <left>" . tab-bar-move-tab-backward)
  :custom
  (tab-bar-show 1)
  (tab-bar-format
   (append tab-bar-format '(tab-bar-format-align-right tab-bar-format-global))))

;; Files customization
(use-package files
  :custom
  (delete-by-moving-to-trash t)
  :config
  ;; Trash behavior on MacOS
  (when (and delete-by-moving-to-trash
	     (eq window-system 'ns))
    (advice-add 'move-file-to-trash :override #'lm/macos-move-file-to-trash))
  ;; Additional file types
  (dolist (mode-spec *additional-auto-modes*)
    (push mode-spec auto-mode-alist)))

;; Dired customization

(use-package dired
  :init
  (require 'dired-x)
  :bind
  (:map dired-mode-map
    ("C-c a" . auto-revert-mode)
    ("<mouse-2>" . dired-find-file))
  :custom
  (dired-auto-revert-buffer t
    "Refresh the dired buffer whenever unburied")
  (dired-use-ls-dired (if (eq system-type 'gnu/linux) 'unspecified)
    "nil on MacOS to avoid a warning")
  (dired-listing-switches
   (if (eq system-type 'gnu/linux)
       "-lahF --group-directories-first"
     "-lahFb")
   "ls -l readability adjustments. Group directories first when using coreutils ls")
  (dired-ls-F-marks-symlinks (eq system-type 'darwin)
    "Rename symlinks correctly, when marked with '@' by ls -lF")
  :hook
  (dired-mode . hl-line-mode)
  :config
  (advice-add 'browse-url-of-dired-file :override #'lm/browse-url-of-dired-marked-files))

;; Package Menu customization
(use-package package
  :hook
  (package-menu-mode . hl-line-mode))

;; Org customization

(use-package org
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :custom
  (org-ellipsis " ▸")
  ;; These work well (and together) on recent org-mode versions
  (org-special-ctrl-a/e (not (version< org-version "9.5")))
  (org-agenda-files (mapcar #'expand-file-name *org-agenda-paths*))
  ;; (org-agenda-files (list (expand-file-name "~/notes")))
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w)" "|" "CANCELLED(c)" "DONE(d)")))
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  (org-agenda-mode . hl-line-mode)
  :bind
  (:map org-mode-map ("C-c t" . org-tags-view))
  ("C-c a" . org-agenda)
  :config
  ;; Additional code-block expansions
  (dolist (elem '(("b" . "src bash")
		  ("conf" . "src conf")
		  ("el" . "src emacs-lisp")
		  ("py" . "src python")))
    (add-to-list 'org-structure-template-alist elem))
  (plist-put org-format-latex-options :scale *latex-preview-scaling-in-org*)
  (dolist (x '(python jupyter))
    (add-to-list 'org-babel-load-languages (cons x t))))

(use-package org-tempo
  :after org
  :config
  (dolist (elem '(("t" . "title")
		  ("au" . "author")
		  ("d" . "date")
		  ("n" . "name")))
    (add-to-list 'org-tempo-keywords-alist elem)))

;; Help buffer customization
(use-package help
  :hook
  (help-mode . visual-line-mode)
  :custom
  (help-window-select t
    "Switch focus to a help window automatically, when created"))

;; Man buffer customization
(use-package man
  :custom
  (Man-notify-method 'aggressive
    "Open Man in another window and switch focus to it"))

;; Info mode customization
(use-package info
  :hook
  (Info-mode . visual-line-mode)
  :custom
  (Info-additional-directory-list
   (append Info-additional-directory-list *additional-texinfo-paths*)))

;; Doc View configuration
(use-package doc-view
  :custom
  (doc-view-resolution 300) ; increase the DPI count (the default is too conservative)
  (doc-view-continuous t
   "Change page when scrolling beyond the top/bottom")
  (doc-view-resolution 300
   "DPIs used to render the pdf pages"))

;; GUI browser configuration
(use-package browse-url
  :custom
  (browse-url-browser-function *preferred-browser*))

;; Markdown mode configuration
(use-package markdown-mode
  :hook
  (markdown-mode . visual-line-mode))


;;;
;;; Aesthetic adjustments
;;;


;; System-dependent visual replacement for the beep warning sound
;; (alternatively, check the mode-line-bell package)
(customize-set-variable 'visible-bell t)

;; Pixelwise motion
(customize-set-variable 'frame-resize-pixelwise t)
(unless (version< emacs-version "29")
  (pixel-scroll-precision-mode t))

;; Optionally transparent frame
(let ((alpha-sym (if (or (version< emacs-version "29")
			 (not (eq (window-system) 'x)))
		     'alpha
		   'alpha-background)))
  (customize-set-variable
   'default-frame-alist
   (add-to-list 'default-frame-alist (cons alpha-sym *transparency-level*))))

;; Replace the default scratch message
(customize-set-variable 'initial-scratch-message (lm/string-from-file *initial-scratch-message*))

;; Convert non-visible ^L (form feed) into a horizontal line
(use-package page-break-lines
  :ensure t
  :init
  (global-page-break-lines-mode))

;; Display Time Mode appearance
(use-package time
  :custom
  (display-time-interval 1)
  (display-time-default-load-average nil)
  (display-time-format "%a %d %b  %T"))

;; Theme overlays

;; Isolate themes not managed by the package managers (e.g., user-created ones)
(let ((theme-directory (expand-file-name "themes" user-emacs-directory)))
  (when (file-exists-p theme-directory)
    (customize-set-variable 'custom-theme-directory theme-directory)))

;; Custom face tweaks
;;
;; For most people, it will suffice doing `M-x customize-faces' on the
;; `default' face, (deselect Foreground and Background if active, so
;; they don't interfere with changing the theme) and then applying a
;; theme. However,
;;
;; - the standard faces `fixed-pitch', `fixed-pitch-serif' and
;;   `variable-pitch', inherited by many others, are defined with
;;   generic font names that don't work everywhere. I customize them.
;; - I reduce the height of `line-number' by 20% for aesthetic reasons.
;; - I modify `custom-variable-obsolete' with a strike-through, again,
;;   for emphasis.

(use-package faces
  :custom-face
  ;; Commas required for substitutions (where is the backquote?
  ;; Hidden in use-package custom-face handler, I presume)
  (fixed-pitch
   ((t (:inherit unspecified :family ,*face-fixed-pitch-family*))))
  (fixed-pitch-serif
   ((t (:inherit unspecified :family ,*face-fixed-pitch-serif-family*))))
  (variable-pitch
   ((t (:inherit unspecified :family ,*face-variable-pitch-family*))))
  (line-number
   ((t (:inherit shadow :height 0.8))))
  (line-number-current-line  ; make the height the same as line-number
   ((t (:inherit line-number)))))

(use-package info
  :custom-face
  (Info-quoted
   ((t (:inherit fixed-pitch-serif)))))

(use-package cus-edit
  :custom-face
  (custom-variable-obsolete
   ((t (:inherit custom-variable-tag :strike-through t :weight normal)))))


;;;
;;; Regional and locale configuration
;;;


;; Calendar adjustments
(use-package calendar
  :custom
  (calendar-date-style 'european)
  (calendar-week-start-day 1))

;; Set the environment locale in case Emacs defaults to nil or "C"
;; (this may happen on MacOS)
(dolist (env-var '("LANG" "LANGUAGE" "LC_CTYPE" "LC_COLLATE"
		   "LC_TIME" "LC_MESSAGES" "LC_MONETARY"))
  (let (locale (getenv env-var))
    (when (or (null locale) (string= locale "C"))
      (setenv env-var "en_US.UTF-8"))))


;;;
;;; Environment and Shell Configuration
;;;


;; Add additional paths the exec-path list and update the process PATH variable
(add-to-list 'exec-path (expand-file-name *texlive-bin-path*))
(dolist (path *additional-bin-paths*)
  (add-to-list 'exec-path (expand-file-name path)))

(setenv "PATH"	(string-join
		 (list (string-join exec-path path-separator)
		       (getenv "PATH"))
		 path-separator))

;; Also update man paths
(setenv "MANPATH"
	(string-join
	 (list (string-join *additional-man-paths* path-separator)
	       (getenv "MANPATH"))
	 path-separator))


;; Automate the interactive shell query of ansi-term
(advice-add 'ansi-term :filter-args #'(lambda (shell-name)
					(interactive (list *shell-binary*))
					shell-name))

(use-package vterm
  :ensure t
  :custom
  (vterm-shell *shell-binary*)
  (vterm-max-scrollback 10000))


;;;
;;; Encryption
;;;


(use-package epg-config
  :custom
  (epg-pinentry-mode 'loopback))


;;;
;;; Interface additions
;;;


;; Diminish: hide modeline symbols for minor modes
(use-package diminish
  :ensure t
  :config
  ;; Diminish selected bundled modes
  (with-eval-after-load 'subword
    (diminish 'subword-mode))
  (with-eval-after-load 'yasnippet
    (diminish 'yas-minor-mode))
  (with-eval-after-load 'page-break-lines
    (diminish 'page-break-lines-mode)))

;; Dashboard: an interactive Emacs front page
(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-center-content t)
  (dashboard-page-separator "\n\f\n")
  (dashboard-startup-banner *dashboard-logo*)
  (dashboard-banner-logo-title (format "GNU Emacs %s" emacs-version))
  (dashboard-items '((recents . 10) (bookmarks . 10) (agenda . 10)))
  (dashboard-set-footer nil)
  :config
  (with-eval-after-load 'dashboard-widgets
    ;; Hooks effective after resizing the frame
    (add-hook 'dashboard-after-initialize-hook #'hl-line-mode)
    (add-hook 'dashboard-after-initialize-hook #'dashboard-jump-to-recents)
    (add-hook 'dashboard-mode-hook #'hl-line-mode)
    (add-hook 'dashboard-mode-hook #'dashboard-jump-to-recents))
  :bind
  (:map dashboard-mode-map
   ("n" . dashboard-next-line)
   ("p" . dashboard-previous-line)))

;; Avy: jump easily to any visible text in every frame/window
(use-package avy
  :ensure t
  :custom
  (avy-style 'words)
  :bind
  ("C-;" . avy-goto-char)
  :custom
  (avy-style 'words))

;; Which Key: much needed pop-up help for prefix keymaps
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

;; Lorem Ipsum: functions to generate placeholder text
(use-package lorem-ipsum
  :ensure t)

;; Org Bullets: beautify org headers with circles instead of *
(use-package org-bullets
  :ensure t
  :after org
  :custom
  (org-bullets-bullet-list '("◉" "●" "○" "⚬"))
  :hook
  (org-mode . org-bullets-mode))


;;;
;;; Narrowing and search
;;;


;; Ivy: interactive narrowing functionality
(use-package ivy
  :ensure t
  :diminish
  :custom
  (ivy-count-format "[%d / %d] ")
  (ivy-wrap t)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode))

;; Counsel: apply the Ivy engine to all narrowing situations
(use-package counsel
  :ensure t
  :diminish
  :config
  (counsel-mode)
  :bind
  ;; The following replaces the simple Ivy narrowing of the native M-x,
  ;; visualizing keybindings and adding more Hydra actions
  ("M-x" . counsel-M-x)
  ;; The following show previews of the buffer while browsing the list
  ;; (compared to ivy-switch-buffer and ivy-switch-buffer-other-window)
  ("C-x C-b" . counsel-switch-buffer)
  ("C-x 4 b" . counsel-switch-buffer-other-window)
  ("s-8 RET" . counsel-unicode-char))

;; Prescient: frequency-based result rankings
(use-package prescient
  :ensure t
  :custom
  (prescient-save-file
   (expand-file-name
    (format "%s/%s" user-emacs-directory "prescient-save.el")))
  :config
  ;; Keep the rankings between sessions
  (prescient-persist-mode))

;; Interface to use the Prescient engine rankings with Ivy
(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode))

;; Swiper: apply Ivy to interactive search
(use-package swiper
  :ensure t
  :diminish
  :bind
  ("C-s" . swiper))


;;;
;;; IDE functionality
;;;


;; Convenient minor modes for programming
(use-package prog-mode
  :hook
  ((prog-mode . subword-mode)
   (prog-mode . show-paren-mode)
   (prog-mode . display-line-numbers-mode)
   (prog-mode . electric-pair-local-mode)))

;; Tree-Sitter grammars configuration
(use-package treesit
  :after prog-mode
  :config
  (setq treesit-language-source-alist *tree-sitter-language-sources*)
  (dolist (lang-spec treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang-spec))
      (treesit-install-language-grammar (car lang-spec))))
  (unless (version< emacs-version "29")
    (customize-set-variable 'major-mode-remap-alist
			    (append major-mode-remap-alist
				    '((c-or-c++-mode . c-or-c++-ts-mode)
				      (css-mode . css-ts-mode)
				      (python-mode . python-ts-mode)
				      (sh-mode . bash-ts-mode))))))

;; Magit: highly comfy git interface
(use-package magit
  :ensure t
  :commands magit-status
  :bind
  ("C-x g" . magit-status))

;; Auto Insert Mode: insert templates in new files
(use-package autoinsert
  :config
  (setq auto-insert-alist
	(assoc-delete-all "C / C++ header" auto-insert-alist
			  (lambda (x y) (and (consp x)
					     (stringp (cdr x))
					     (string= (cdr x) y)))))
  (define-auto-insert
    '("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C / C++ header")
    '((replace-regexp-in-string
       "[^A-Z0-9]" "_"
       (replace-regexp-in-string
	"\\+" "P"
	(upcase (file-name-nondirectory buffer-file-name))))
      "#ifndef H_" str \n
      "#define H_" str \n \n
      _ "\n\n#endif /* H_" str " */"))
  (define-auto-insert
    '("\\.sh\\'" . "Shell script")
    '(nil
      "#!" *shell-binary* \n \n \n))
  (define-auto-insert
    '("\\.[Pp][Yy]\\'" . "Python script")
    '((replace-regexp-in-string
       "[^A-Z0-9]" "_")
      "#!/usr/bin/env python" \n \n \n
      "def main():" \n
      _ "pass" \n \n \n
      "if __name__ == '__main__':" \n
      "main()" \n))
  (auto-insert-mode t))

;; RealGUD: better debugger interface
(use-package realgud
  :ensure t
  :bind
  (:map python-mode-map
	("C-x C-a C-r" . realgud:pdb)
	("C-x C-a C-a" . realgud:attach-cmd-buffer)
   :map python-ts-mode-map
	("C-x C-a C-r" . realgud:pdb)
	("C-x C-a C-a" . realgud:attach-cmd-buffer)
   :map c-mode-map ("C-x C-a C-r" . realgud:gdb)
   :map c++-mode-map ("C-x C-a C-r" . realgud:gdb)))

;; Company: inline autocompletion engine
(use-package company
  :ensure t
  :ensure yasnippet
  :bind
  (:map company-active-map
   ("M-n" . nil)
   ("M-p" . nil)
   ("C-n" . company-select-next-or-abort)
   ("C-p" . company-select-previous-or-abort))
  :custom
  (company-selection-wrap-around t)
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  :hook
  (prog-mode . company-mode)
  (prog-mode . yas-minor-mode))

;; Eldoc configuration
(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-idle-delay 0.3))

;; RealGUD configuration
(use-package realgud
  :ensure t
  :bind
  (:map
   python-mode-map
   ("C-x C-a C-r" . realgud:pdb)
   ("C-x C-a C-a" . realgud:attach-cmd-buffer)
   :map
   c-mode-map
   ("C-x C-a C-r" . realgud:gdb)
   ("C-x C-a C-a" . realgud:attach-cmd-buffer)
   :map
   c++-mode-map
   ("C-x C-a C-r" . realgud:gdb)
   ("C-x C-a C-a" . realgud:attach-cmd-buffer)))

;; CSV Mode - Support for column-wise CSV-file visualization
(use-package csv-mode
  :ensure t
  :hook
  (csv-mode . csv-align-mode)
  (csv-mode . csv-header-line)
  ;; I wish I could use this but it misaligns the header bar
  ;; (csv-mode . display-line-numbers-mode)
  (csv-mode . hl-line-mode))

;; Bash

;; Eglot support for Microsoft's Language Server Protocol (LSP)
(use-package eglot
  :ensure t
  :bind
  (:map eglot-mode-map
    ("S-<f6>" . eglot-rename))
  :hook
  ((c-mode c-ts-mode c++-mode c++-ts-mode) . eglot-ensure)
  ((js-mode js-ts-mode js2-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs `(c-mode ,*c/c++-lsp-server-binary*))
  (add-to-list 'eglot-server-programs `(c-ts-mode ,*c/c++-lsp-server-binary*))
  (add-to-list 'eglot-server-programs `(c++-mode ,*c/c++-lsp-server-binary*))
  (add-to-list 'eglot-server-programs `(c++-ts-mode ,*c/c++-lsp-server-binary*))
  (add-to-list 'eglot-server-programs `(js-mode ,*typescript-language-server-binary*))
  (add-to-list 'eglot-server-programs `(js-ts-mode ,*typescript-language-server-binary*))
  (add-to-list 'eglot-server-programs `(js2-mode ,*typescript-language-server-binary*))
  (add-to-list 'eglot-server-programs `(python-mode ,*python-lsp-server-binary*))
  (add-to-list 'eglot-server-programs `(python-ts-mode ,*python-lsp-server-binary*)))

;; Web dev configuration

(use-package mhtml-mode
  :config
  (unbind-key "M-o" html-mode-map)
  (unbind-key "M-o" mhtml-mode-map))

(use-package js
  :after js-comint
  :bind
  (:map js-mode-map
	("C-c C-b" . js-send-buffer)
	("C-c C-c" . js-send-last-sexp)
	("C-c C-z" . js-comint-repl)
	("C-c C-r" . js-send-region))
  (:map js-ts-mode-map
	("C-c C-b" . js-send-buffer)
	("C-c C-c" . js-send-last-sexp)
	("C-c C-z" . js-comint-repl)
	("C-c C-r" . js-send-region))
  :custom
  (js-indent-level 2))

(use-package js2-mode
  :ensure t)

(use-package js-comint
  :ensure t)

;; SQL configuration
(use-package sql-indent
  :ensure t)

(use-package sql
  :custom
  (sql-product *sql-product*)
  (sql-connection-alist *sql-connection-alist*)
  :hook
  (sql-mode . sqlind-minor-mode)
  :bind
  ("C-c C-p" . sql-connect)
  ("C-<return>" . sql-send-paragraph))

;; ESS - Emacs Speaks Statistics: R and R Markdown suite

(defun insert-pipe ()
  "Insert the pipe (%>%) operator at point, as defined by the magrittr
package."
  (interactive)
  (insert "%>% "))

;; Automate the pdf rendering of R Markdown projects
(defun rmarkdown-render (filename)
  "Run rmarkdown::render on the chosen file.
R and the rmarkdown package, and appropriate renderers,
must be installed at a minimum."
  (interactive "FFile to render: ")
  (message "Rendering file \"%s\"..." filename)
  (async-shell-command
   (format "Rscript -e \'rmarkdown::render(\"%s\")\'" filename))
  (message "... done!"))

(use-package ess
  :ensure t
  :custom
  (ess-use-ido nil)
  (ess-style 'RStudio)
  :config
  (with-eval-after-load 'ess-r-mode
    (bind-key "C-c r" #'rmarkdown-render ess-r-mode-map)
    (bind-key "C->" #'insert-pipe ess-r-mode-map))
  (with-eval-after-load 'inferior-ess-r-mode
    (bind-key "C->" #'insert-pipe inferior-ess-r-mode-map)))

;; Poly-R: R-Markdown support based on poly-mode
(use-package poly-R
  :ensure t
  :after ess)

;; JavaScript configuration

(use-package js-comint
  :ensure t)

(use-package js
  :after js-comint
  :bind
  (:map js-mode-map
	("C-c C-z" . #'js-comint-repl)
	("C-c C-c" . #'js-comint-send-last-sexp)
	("C-c C-r" . #'js-comint-send-region)
	("C-c C-b" . #'js-comint-send-buffer))
  (:map js-ts-mode-map
	("C-c C-z" . #'js-comint-repl)
	("C-c C-c" . #'js-comint-send-last-sexp)
	("C-c C-r" . #'js-comint-send-region)
	("C-c C-b" . #'js-comint-send-buffer)))

(use-package js2-mode
  :ensure t)

;; Python configuration

(defun python-shell-send-paragraph-or-region (&optional send-main msg)
  "Send the paragraph at point to the inferior Python process.  The
statement is delimited by the beginning and end of paragraph, but if
the region is active, the text in the region is sent instead via
`python-shell-send-region'.  Optional argument SEND-MAIN, if non-nil,
means allow execution of code inside blocks delimited by \"if __name__
== \\='__main__\\=':\".  Interactively, SEND-MAIN is the prefix
argument.  Optional argument MSG, if non-nil, forces display of a
user-friendly message if there's no process running; it defaults to t
when called interactively."
  (interactive (list current-prefix-arg t))
  (if (region-active-p)
      (python-shell-send-region (region-beginning) (region-end) send-main msg)
    (save-excursion
      (python-shell-send-region
       (progn (backward-paragraph) (point))
       (progn (forward-paragraph) (point))
       send-main msg)))
  (forward-paragraph)
  (if (= (point) (point-max))
      (end-of-line)
    (forward-paragraph)
    (backward-paragraph)
    (forward-line)))

(use-package python
  :init					; these can be done with local variables
  (setenv "PYTHONPATH"
	  (string-join
	   (mapcar #'expand-file-name *python-shell-extra-pythonpaths*)
	   path-separator))
  (when *python-virtual-environment-home-path* ; FIXME  why use `when'?
    ;; The easiest way to let pyvenv etc. know where to create virtual environments
    (setenv "WORKON_HOME" (expand-file-name *python-virtual-environment-home-path*)))
  :custom
  (python-shell-extra-pythonpaths
   (mapcar #'expand-file-name *python-shell-extra-pythonpaths*))
  (python-indent-offset 4)
  (python-shell-completion-native-enable
   (not (eq system-type 'darwin))
   "Native shell completion doesn't work on MacOS")
  :bind
  ;; Remaps that mimic the behavior of ESS
  (:map python-mode-map
	("C-c C-b" . python-shell-send-buffer)
	("C-c C-c" . python-shell-send-paragraph-or-region))
  (:map python-ts-mode-map
	("C-c C-b" . python-shell-send-buffer)
	("C-c C-c" . python-shell-send-paragraph-or-region)))

;; Activate and make the inferior shell aware of virtual environments
;; FIXME: I don't remember the meaning of hook and shell specifications
(use-package pyvenv
  :ensure t
  :commands pyvenv-create
  :custom
  (pyvenv-exec-shell *shell-binary*)
  :bind
  ("C-c v c" . pyvenv-create)
  ("C-c v a" . pyvenv-activate)
  ("C-c v c" . pyvenv-create)
  ("C-c v d" . pyvenv-deactivate)
  ("C-c v w" . pyvenv-workon)
  ("C-c v r" . pyvenv-restart-python)
  :hook
  ((python-mode python-ts-mode inferior-python-mode) . pyvenv-mode)
  :config
  (setq pyvenv-mode-line-indicator
	'(pyvenv-virtual-env-name
	  ("[pyvenv:" pyvenv-virtual-env-name "] "))))

;; EIN: Jupyter support (experimental setup: doesn't support lsp)

(use-package jupyter
  :ensure t)

(use-package ein
  :ensure t
  :custom
  (ein:output-area-inlined-images nil)
  (ein:jupyter-default-kernel *python-interpreter-binary*)
  :init
  (with-eval-after-load 'mailcap        ; FIXME: probably not portable
    (add-to-list 'mailcap-user-mime-data '((viewer . (concat *ein-image-viewer* " %s"))
					   (type . "image/png")))))

(use-package ein-notebook
  :bind
  (:map ein:notebook-mode-map
   ("<C-return>" . ein:worksheet-execute-cell-and-insert-below-km)
   ("M-n" . ein:worksheet-goto-next-input-km)
   ("M-p" . ein:worksheet-goto-prev-input-km)))

(use-package ein:notebooklist
  :bind
  (:map ein:notebooklist ("C-c C-k" . ein:stop))) ; FIXME: should be of the form 'C-c [key]'

;; C/C++ configuration and ccls

(use-package cc-mode
  :bind
  (:map c-mode-map ("<f5>" . compile)
   :map c++-mode-map ("<f5>" . compile)))

;; Company backend for C/C++ headers
(use-package company-c-headers
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends #'company-c-headers))

;; ccls: C/C++ backend for LSP
(use-package ccls
  :ensure t
  :custom
  (ccls-executable *c/c++-lsp-server-binary*))

;; Debugger interface
(use-package gdb-mi
  :custom
  (gud-gdb-command-name *gdb-binary*))

;; Common Lisp support

(use-package slime-company
  :ensure t
  :config
  (add-to-list 'company-backends #'company-slime))

(use-package slime
  :ensure t
  :commands slime
  :bind (:map lisp-mode-map ("C-c s" . slime))
  :init
  (customize-set-variable 'inferior-lisp-program *lisp-binary*)
  :hook
  ;; Browsing the Web 1.0 CLHS is comfier from within Emacs
  (lisp-mode . (lambda ()
		 (setq-local browse-url-browser-function
			     (lambda (a &optional b)
			       ;; (other-window 1)
			       (eww-browse-url a t)))))
  :bind
  (:map lisp-mode-map
	("C-c s" . slime)))

(use-package slime-company
  :ensure t
  :config
  (add-to-list 'company-backends #'company-slime))

;; Guile Scheme support

(use-package scheme
  :custom
  (scheme-program-name *scheme-binary*))

(use-package scheme
  :custom
  (scheme-program-name *scheme-binary*))

(use-package geiser
  :ensure t)

(use-package geiser-guile
  :ensure t
  :after geiser
  :custom
  (geiser-guile-manual-lookup-other-window-p t
   "Open info entries in another window")
  :bind
  (:map scheme-mode-map ("C-c s" . run-geiser)))

;; Meme "Wizard Book" in Info format. Read it with `M-x info'.
;; Also worth checking out:
;; - a homebrew LaTeX version at:
;;   https://github.com/sarabander/sicp-pdf.git
;; - the original free HTML version at:
;;   https://mitpress.mit.edu/sites/default/files/sicp/index.html
(use-package sicp
  :ensure t)


;;;
;;; Load `Customize' setup
;;;


(load-file *custom-file-name*)
