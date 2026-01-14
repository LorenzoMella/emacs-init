;;;  init.el --- Personal init configuration for general use, C/C++ programming, Python, Lisp, and R -*- lexical-binding: t; coding: utf-8 -*-

;;  Copyright (C) 2021-2026 Lorenzo Mella

;;  Author: Lorenzo Mella <lorenzo.mella@yahoo.com>

;; Commentary:


(defun lm/string-from-file (path)
  "Returns the content of a text file as a string."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))


;;;
;;; Quick-access to user-defined variables
;;;


;; Face families meant to replace the placeholder fonts specified in faces.el
(defvar *face-fixed-pitch-family* "Monospace")

(defvar *face-fixed-pitch-serif-family* "Monospace")

(defvar *face-variable-pitch-family* "Sans Serif")

;; Binaries and paths
(defvar *shell-binary* (getenv "SHELL") ; using the default for the current user
  "Preferred shell to use with `term'/`ansi-term'.")

(defvar *python-interpreter-binary* "python"
  "Preferred Python interpreter.")

(defvar *python-lsp-server-binary* "~/.local/bin/pylsp"
  "Path to the chosen Python LSP Server executable.")

(defvar *typescript-language-server-binary*
  '("~/.local/bin/typescript-language-server" "--stdio")
  "Path to the chosen JS/TS Language Server executable.")

(defvar *gdb-binary* "gdb"
  "Path to gdb executable.")

(defvar *c/c++-lsp-server-binary* "clangd"
  "Path to the chose C/C++ etc. LSP server executable.")

(defvar *lisp-binary* "sbcl"
  "Path to the Common Lisp interpreter of choice.")

(defvar *scheme-binary* "guile"
  "Path to the Scheme interpreter of choice.")

(defvar *additional-man-paths*
  '("/usr/local/share/man" "/opt/local/share/man"))

(defvar *additional-texinfo-paths*
  '("/usr/local/share/info" "/opt/local/share/info")
  "List of the nonstandard texinfo paths.")

(defvar *additional-python-source-paths* '("~/projects/local-packages")
  "Paths to be added to `python-shell-extra-pythonpaths'")

(defvar *python-virtual-environment-home-path* "~/.virtualenvs"
  "The directory where the Python Virtual Environments are located. Ignored if nil.")

(defvar *tree-sitter-language-sources*
  ;; entries are of the form (LANG . (URL REVISION SOURCE-DIR CC C++)), with the
  ;; cdr (CC C++) optional
  '((bash "https://github.com/tree-sitter/tree-sitter-bash.git" "master" "src")
    (c "https://github.com/tree-sitter/tree-sitter-c.git" "master" "src")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp.git" "master" "src")
    (cmake "https://github.com/uyha/tree-sitter-cmake.git" "master" "src")
    (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript.git" "master" "src")
    (html "https://github.com/tree-sitter/tree-sitter-html.git" "master" "src")
    (css "https://github.com/tree-sitter/tree-sitter-css.git" "master" "src")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript.git" "master" "src")
    (json "https://github.com/tree-sitter/tree-sitter-json.git" "master" "src")
    (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua.git" "main" "src")
    (python "https://github.com/tree-sitter/tree-sitter-python.git" "master" "src")
    (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml.git" "master" "src"))
  "Grammar retrieval information to populate `treesit-language-source-alist'.")

(defvar *org-babel-active-languages* '(jupyter lisp python R shell sql)
  "ob-[lang].el packages to load together with Org Mode.")

(defvar *texlive-bin-path* "/usr/bin"
  "Path to the TeXlive binaries.")

(defvar *additional-bin-paths* '("~/.local/bin" "/opt/local/bin" "/usr/local/bin")
  "List of paths to additional binaries.")

(defvar *additional-auto-modes*
  '(("\\.godot\\'" . conf-windows-mode)
    ("[Mm]akefile\\'" . makefile-gmake-mode)
    ("CMakeLists\\.txt\\'" . cmake-ts-mode)
    ("\\.cmake\\'" . cmake-ts-mode)
    ("\\.clang-format\\'" . yaml-ts-mode)
    ("\\.inl\\'" . c-or-c++-mode)
    ("\\.lua\\'" . lua-ts-mode)
    ("\\.aws/credentials\\'" . conf-mode)
    ("\\.pgpass\\'" . conf-mode)
    ("\\.sbclrc\\'" . lisp-mode)))

(defvar *sql-product* 'postgres
  "The default SQL dialect in `sql-mode' buffers.")

(defvar *sql-connection-alist*
  '(("postgres"
     (sql-product 'postgres)
     (sql-user "myname")
     (sql-server "localhost")
     (sql-database "postgres")
     (sql-port 5432)))
  "Database user/role configuration.")

(defvar *ein-image-viewer* "/usr/bin/open -a Preview"
  "Image viewing program used by the EIN package (with options).")

(defvar *godot-app-path* "/usr/bin/redot"
  "Path to the Godot or Godot fork executable.")

;; Other settings
(defvar *gc-bytes* (* 50 1024 1024) ; 50MB
  "Preferred heap threshold size to start garbage collection.")

(defvar *custom-file-name* (expand-file-name "custom-file.el" user-emacs-directory)
  "`customize' will save its settings in this file.
Set it to nil to append to this file.")

(defvar *transparency-alpha* 98
  "Frame transparency alpha parameter.")

(defvar *global-line-spacing* 0.2
  "Distance between consecutive lines in pixels or fraction of line height.")

(defvar *preferred-browser* #'browse-url-default-browser
  "Any one of the browser symbols defined by the `browse-url' package.")

(defvar *latex-preview-scaling-in-org* 2.0
  "Scaling of Latex image previews in Org Mode.")

(defvar *initial-scratch-message*
  (expand-file-name "initial-scratch-message.txt" user-emacs-directory)
  "Path to text file including a custom *scratch* buffer message.")

(defvar *dashboard-logo* 'logo)

(defvar lm/fs-path-separator (string-remove-prefix "p" (file-name-as-directory "p"))
  "A native way to determine the directory path separator (not to be confused with `path-separator').")


;;;
;;; Custom general-purpose functions
;;;


(defun lm/init-show ()
  "Opens the configuration file currently defined as `user-init-file'."
  (interactive)
  (find-file-existing user-init-file))

(defalias 'init-show 'lm/init-show)

(defun lm/frame-resize-and-center (frame &optional width-fraction)
  "Resizes the frame to about two thirds of the screen."
  (interactive '((window-frame) 1.0))
  (let* ((workarea (frame-monitor-attribute 'workarea))
	 (new-height (elt workarea 3))
	 (new-width (floor (* new-height width-fraction)))
	 (top-left-x (+ (elt workarea 0) (/ (- (elt workarea 2) new-width) 2)))
	 (top-left-y (elt workarea 1)))
    (set-frame-position (window-frame) top-left-x top-left-y)
    ;; TODO: somehow it is not centered. Fringes problem
    (set-frame-size (window-frame) new-width new-height t)))

(defun lm/frame-resize-left (frame)
  "Resizes the frame to cover the left half-screen."
  (interactive '((window-frame)))
  (let* ((workarea (frame-monitor-attribute 'workarea))
	 (new-height (elt workarea 3))
	 (new-width (round (/ (elt workarea 2) 2.0)))
	 (top-left-x (elt workarea 0))
	 (top-left-y (elt workarea 1)))
    (set-frame-position (window-frame) top-left-x top-left-y)
    (set-frame-size (window-frame) new-width new-height t)))

(defun lm/frame-resize-right (frame)
  "Resizes the frame to cover the right half-screen."
  (interactive '((window-frame)))
  (let* ((workarea (frame-monitor-attribute 'workarea))
	 (new-height (elt workarea 3))
	 (new-width (round (/ (elt workarea 2) 2.0)))
	 (top-left-x (+ new-width (elt workarea 0)))
	 (top-left-y (elt workarea 1)))
    (set-frame-position (window-frame) top-left-x top-left-y)
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

(defun lm/adjust-transparency (alpha &optional frame)
  "Quick interactive transparency adjustment."
  (interactive "NEnter alpha level: ")
  (if (and (not (version< emacs-version "29"))
	   (eq (window-system) 'x))
      (set-frame-parameter frame 'alpha-background alpha)
    (set-frame-parameter frame 'alpha alpha)))

(defalias 'adjust-transparency 'lm/adjust-transparency)

(defun lm/macos-move-file-to-trash (path)
  "Replacement to `move-file-to-trash' using the native MacOS 'Move
to Bin' functionality. This is necessary if we want the 'Put
Back' menu option to be available when right-clicking on items
that have been moved to the Bin."
  (ns-do-applescript
   (format
    "tell application \"Finder\" to move POSIX file \"%s\" to trash"
    (directory-file-name (expand-file-name path)))))

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

(defun lm/browse-file-directory ()
  (interactive)
  (browse-url (file-name-directory (buffer-file-name))))

(defun lm/whitespace-cleanup-notify (cleanup-fn &rest args)
  "Wrapper adding echo-area messages to `whitespace-cleanup' since
sometimes it is not obvious whether space has been deleted or
not."
  (let ((modified-tick (buffer-modified-tick)))
    (apply cleanup-fn args)
    (if (> (buffer-modified-tick) modified-tick)
	(message "Excess whitespace deleted")
      (message "(No whitespace to clean up)"))))

(defvar *excluded-grep-directories*
  '(".git" ".idea" ".ipynb_checkpoints" ".ccls-cache"))

(defun lm/rgrep-project (regexp &optional case-insensitive exclude-dirs exclude-glob)
  "Recursive grep at a project-root level.

Pass a possibly empty keyboard argument to do a case-insensitive search"
  (interactive "MSearch project for regexp:\s
P
MExcluded directories (space-separated):\s
MExclude files with regexp: ")
  (setq exclude-dirs (string-split exclude-dirs nil t nil)
	exclude-dirs (append exclude-dirs *excluded-grep-directories*))
  (if-let ((proj (project-current)))
      (let ((default-directory (project-root proj))
	    (grep-format
	     "grep --extended-regexp --color=auto --null -nHIr %s %s %s -e \"%s\" .")
	    (case-insensitive-option (if case-insensitive "-i" ""))
	    (exclude-dir-options
	     (seq-reduce (lambda (x y) (format "%s --exclude-dir=\"%s\"" x y)) exclude-dirs ""))
	    (exclude-option
	     (if (string-empty-p exclude-glob) "" (format "--exclude=\"%s\"" exclude-glob))))
	(grep (format grep-format case-insensitive-option exclude-dir-options exclude-option regexp)))
    (message "(Buffer %s is not part of a project)" (buffer-name))))

(defun lm/vterm-right (&optional arg)
  "Displays `vterm' on the right of the screen, starting a terminal if necessary."
  (interactive "P")
  (if (= 0 (window-pixel-left))
      (vterm-other-window arg)
    (vterm arg)))

(defun lm/kill-buffer-other-window ()
  (interactive)
  (kill-buffer (window-buffer (other-window-for-scrolling))))

(defalias 'kill-buffer-other-window #'lm/kill-buffer-other-window)

(defun lm/kill-buffer-other-window (&optional count)
  (interactive)
  (unless count
    (setq count 1))
  (save-excursion
    (kill-buffer (window-buffer (next-window nil -1)))))


;;;
;;; Package management
;;;


(require 'package)

;; Add references to online repositories other than GNU ELPA
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")) ; only effective with Emacs 28 or earlier
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Package management will be handled by use-package
(when (and (version< emacs-version "29.0.60")
	   (not (package-installed-p 'use-package)))
    (package-refresh-contents)
    (package-install 'use-package))

;; Keep ELPA keys up to date
(use-package gnu-elpa-keyring-update
  :ensure t)


;;;
;;; `Customize' setup
;;;


(setq custom-file (expand-file-name *custom-file-name* user-emacs-directory))

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


;; Frame size fix under Gnome (prevents occasional creation of very small frames)
(when (and (memq (window-system) '(x pgtk))
	   (string-equal "GNOME" (getenv "XDG_CURRENT_DESKTOP")))
  (customize-set-variable 'initial-frame-alist
			  '((width . 90) (height . 35)))
  (set-frame-size nil 90 35)
  (customize-set-variable 'default-frame-alist
			  '((width . 90) (height . 35)))
  (set-frame-size nil 90 35))

;; General keybindings

(require 'bind-key)  ; this is a `use-package' dependency so it should be loaded anyway

;; Unbind toxic MacOS keybindings
(when (eq (window-system) 'ns)
  (unbind-key "s-o")
  (unbind-key "s-&")
  (unbind-key "s-k"))

;; Remap commands to more convenient keys (the defaults still work)
(bind-key "M-o" #'other-window)
(bind-key "C-M-;" #'comment-line)
(unless (eq (window-system) 'ns)   ; Already in place in MacOS
  (bind-key "s-=" #'text-scale-increase)
  (bind-key "s--" #'text-scale-decrease)
  (bind-key "s-u" #'revert-buffer))

;; Remap the insert-char shortcuts
(unbind-key "s-8")
(bind-key "s-8" 'iso-transl-ctl-x-8-map key-translation-map)

;; Window-resizing keybindings
(unbind-key "s-m") ; Normally bound to `iconify-frame' on MacOS. Use `C-z' instead
(bind-key "s-m f" #'toggle-frame-fullscreen)
(bind-key "s-m m" #'toggle-frame-maximized)
(bind-key "s-m c" #'lm/frame-resize-and-center)
(bind-key "s-m <left>" #'lm/frame-resize-left)
(bind-key "s-m <right>" #'lm/frame-resize-right)
(bind-key "s-m s" #'window-toggle-side-windows)

;; Remap keys to more convenient commands
(bind-key [remap kill-buffer] #'kill-current-buffer)
(bind-key [remap kill-buffer] #'quit-window ; Never kill *scratch* by accident
	  lisp-interaction-mode-map (string= (buffer-name) "*scratch*"))
(bind-key [remap capitalize-word] #'capitalize-dwim) ; the dwim-versions work on regions too
(bind-key [remap downcase-word] #'downcase-dwim)
(bind-key [remap upcase-word] #'upcase-dwim)

;; Other keybindings
(bind-key "s-m a" #'lm/adjust-transparency)
(bind-key "s-m `" #'lm/cycle-line-wrap-modes)
(bind-key "s-m a" #'lm/adjust-transparency)
(bind-key "s-m w" #'window-swap-states)
(bind-key "M-s r" #'lm/rgrep-project)
(bind-key "C-c t" #'lm/vterm-right)
(bind-key "C-x 4 k" #'lm/kill-buffer-other-window)

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
(if (boundp 'use-short-answers)
    (customize-set-variable 'use-short-answers t)
  (defalias 'yes-or-no-p 'y-or-n-p))

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
  ;; 3.x and 40-47)
  (when (eq (window-system) 'x)
    (bind-key "<s-mouse-4>" #'mwheel-scroll)
    (bind-key "<s-mouse-5>" #'mwheel-scroll)))

(use-package pixel-scroll
  :init
  ;; TODO Ideally it should only activate with Magic Mouse or similar
  ;; smooth-scrolling devices
  (when (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode)))

(use-package xt-mouse
  :init
  (unless (display-graphic-p)
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
      (window-parameters . ((no-other-window . t))))
     ("\\*gud-.*\\*"
      (display-buffer-reuse-window
       display-buffer-use-some-window)
      ((dedicated . t))))))

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
    (add-to-list 'auto-mode-alist mode-spec)))

;; Dired customization
(use-package dired
  :bind
  (:map dired-mode-map
    ("r" . lm/dired-find-file-read-only)
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
     "-lahF")
    "ls -l readability adjustments. Group directories first when using coreutils ls")
  (dired-ls-F-marks-symlinks (eq system-type 'darwin)
    "Rename symlinks correctly, when marked with '@' by ls -lF")
  :hook
  (dired-mode . hl-line-mode)
  :config
  (require 'dired-x)
  (advice-add 'browse-url-of-dired-file :override #'lm/browse-url-of-dired-marked-files))

;; Package Menu customization
(use-package package
  :hook
  (package-menu-mode . hl-line-mode))

;; Org customization
(use-package org
  :custom
  (org-ellipsis " ▸")
  (org-startup-indented (not (version< org-version "9.5")))
  (org-special-ctrl-a/e (not (version< org-version "9.5")))
  ;; (org-agenda-files (mapcar #'expand-file-name *org-agenda-paths*))
  (org-confirm-babel-evaluate nil)
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w)" "|" "CANCELLED(c)" "DONE(d)")))
  :custom-face
  ;; The default `org-indent' face inherits from fixed-pitch, rather than
  ;; default. Since the hidden indent characters appear on virtually every line,
  ;; they may give the impression of an increase in `line-spacing', which is
  ;; however left untouched.
  (org-indent ((t (:inherit org-hide))))
  :hook
  (org-mode . visual-line-mode)
  (org-agenda-mode . hl-line-mode)
  :bind
  ("C-c a" . org-agenda)
  (:map org-mode-map
	("C-c l" . org-toggle-link-display)
	("C-c t" . org-tags-view)
	("M-p" . org-backward-element)
	("M-n" . org-forward-element)
	("M-{" . org-backward-paragraph)
	("M-}" . org-forward-paragraph))
  :config
  ;; Additional code-block expansions
  (dolist (elem '(("b" . "src bash")
		  ("conf" . "src conf")
		  ("el" . "src emacs-lisp")
		  ("py" . "src python")))
    (add-to-list 'org-structure-template-alist elem))
  ;; Adjust the LaTeX rendering scale multiplier
  (plist-put org-format-latex-options :scale *latex-preview-scaling-in-org*)
  ;; Load Additional Org Babel Languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   (mapcar (lambda (x) `(,x . t)) *org-babel-active-languages*)))

(use-package org-tempo
  :after org
  :config
  (dolist (elem '(("t" . "title")
		  ("au" . "author")
		  ("d" . "date")))
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
  (doc-view-resolution 300) ; increase the DPI count (the default, 100, is too conservative)
  (doc-view-continuous t
    "Change page when scrolling beyond the top/bottom"))

;; GUI browser configuration
(use-package browse-url
  :custom
  (browse-url-browser-function *preferred-browser*))

(use-package image
  :bind (:map image-mode-map ("W" . lm/browse-file-directory)))

;; Markdown mode configuration
(use-package markdown-mode
  :hook
  (markdown-mode . visual-line-mode))


;;;
;;; Aesthetic adjustments
;;;


;; TODO Set up `move-frame-functions' to correctly update the fullscreen position when switching screens

;; System-dependent visual replacement for the beep warning sound
;; (alternatively, check the mode-line-bell package)
(customize-set-variable 'visible-bell t)

;; Replace the default scratch message
(when (file-exists-p *initial-scratch-message*)
  (customize-set-variable 'initial-scratch-message
			  (lm/string-from-file *initial-scratch-message*)))

(when (display-graphic-p)
  ;; Resize window pixel-wise with mouse
  (customize-set-variable 'frame-resize-pixelwise t)
  ;; Optionally transparent frame
  (lm/adjust-transparency *transparency-alpha*))

;; Convert non-visible ^L (form feed) into a horizontal line
(use-package page-break-lines
  :ensure t
  :init
  (global-page-break-lines-mode))

;; Display Time Mode appearance
(use-package time
  :defer t
  :custom
  (display-time-interval 1)
  (display-time-default-load-average nil)
  (display-time-format "%a %d %b  %T"))

;; Isolate themes not managed by `package' or `use-package' (e.g., user-created ones)
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
   ((t (:inherit (shadow default) :height 0.8))))
  (line-number-current-line  ; make the height the same as line-number
  ((t (:inherit line-number)))))

(use-package cus-edit
  :custom-face
  (custom-variable-obsolete
   ((t (:inherit custom-variable-tag :strike-through t :weight normal)))))

(customize-set-variable 'line-spacing *global-line-spacing*)


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
  (let ((locale (getenv env-var)))
    (when (or (null locale) (string= locale "C"))
      (setenv env-var "en_US.UTF-8"))))


;;;
;;; Environment and Shell Configuration
;;;


;; Add additional paths the exec-path list and update the process PATH variable
(add-to-list 'exec-path (expand-file-name *texlive-bin-path*))
(dolist (path *additional-bin-paths*)
  (add-to-list 'exec-path (expand-file-name path)))

(setenv "PATH"
	(string-join
	 (list (string-join exec-path path-separator)
	       (getenv "PATH"))
	 path-separator))

;; Also update man paths
(setenv "MANPATH"
	(string-join
	 (cons (getenv "MANPATH")
	       (mapcar #'expand-file-name *additional-man-paths*))
	 path-separator))


;; Automate the interactive shell query of ansi-term
(advice-add 'ansi-term :filter-args #'(lambda (shell-name)
					(interactive (list *shell-binary*))
					shell-name))

(use-package vterm
  :ensure t
  :custom
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
  :config
  (with-eval-after-load 'dashboard-widgets
    ;; Hooks effective after resizing the frame
    (add-hook 'dashboard-after-initialize-hook #'hl-line-mode)
    (add-hook 'dashboard-after-initialize-hook #'dashboard-jump-to-recents)
    (add-hook 'dashboard-mode-hook #'hl-line-mode)
    (add-hook 'dashboard-mode-hook #'dashboard-jump-to-recents)
    (if (boundp 'dashboard-footer-messages)
	(customize-set-variable 'dashboard-footer-messages '(nil))
      (dashboard-set-footer nil)))
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
  ("C-;" . avy-goto-char))

;; Which Key: much needed pop-up help for prefix keymaps
(use-package which-key
  :ensure t  ; available starting with Emacs 30
  :diminish
  :config
  (which-key-mode))

;; Lorem Ipsum: functions to generate placeholder text
(use-package lorem-ipsum
  :ensure t)

;; Org Bullets: beautify org headers with circles instead of *
;; TODO no longer maintained: replace with org-superstar.
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
  ;; Other bindings
  ("C-c g" . counsel-git-grep)
  ("s-8 RET" . counsel-unicode-char))

;; Prescient: frequency-based result rankings
(use-package prescient
  :ensure t
  :custom
  (prescient-save-file
   (expand-file-name "prescient-save.el" user-emacs-directory))
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
   (prog-mode . electric-pair-local-mode)
   (prog-mode . (lambda () (add-hook 'before-save-hook #'whitespace-cleanup)))))

;; Tree-Sitter grammars configuration
(use-package treesit
  :after prog-mode
  :config
  (setq treesit-language-source-alist *tree-sitter-language-sources*)
  (dolist (lang-spec treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang-spec))
      (treesit-install-language-grammar (car lang-spec))))
  (unless (version< emacs-version "29")
    (customize-set-variable
     'major-mode-remap-alist
     (append major-mode-remap-alist
	     '((c-mode . c-ts-mode)
	       (c++-mode . c++-ts-mode)
	       (c-or-c++-mode . c-or-c++-ts-mode)
	       (css-mode . css-ts-mode)
	       (gdscript-mode . gdscript-ts-mode)
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
    '((thread-last buffer-file-name
		   (file-name-nondirectory)
		   (upcase)
		   (replace-regexp-in-string "\\.[^.]*\\+[^.]*" "P")
		   (replace-regexp-in-string "[^A-Z0-9]" "_"))
      "#ifndef H_" str \n
      "#define H_" str \n \n
      _
      "\n\n#endif /* H_" str " */"))
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
      _
      "pass" \n \n \n
      "if __name__ == '__main__':" \n
      "main()" \n))
  (auto-insert-mode t))

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
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  :hook
  (prog-mode . company-mode)
  (prog-mode . yas-minor-mode)
  :config
  ;; conflicts with eglot+clangd
  (customize-set-variable 'company-backends
			  (remq 'company-clang company-backends)))


;; Eldoc configuration
(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t))

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
   c-ts-mode-map
   ("C-x C-a C-r" . realgud:gdb)
   ("C-x C-a C-a" . realgud:attach-cmd-buffer)
   :map
   c++-mode-map
   ("C-x C-a C-r" . realgud:gdb)
   ("C-x C-a C-a" . realgud:attach-cmd-buffer)
   :map
   c++-ts-mode-map
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

;; Eglot support for Microsoft's Language Server Protocol (LSP)

(use-package eglot
  :ensure t
  :bind
  (:map eglot-mode-map
    ("S-<f6>" . eglot-rename))
  :hook
  ((gdscript-mode gdscript-ts-mode) . eglot-ensure)
  ((c-mode c-ts-mode c++-mode c++-ts-mode) . eglot-ensure)
  ((gdscript-mode gdscript-ts-mode) . eglot-ensure)
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

;; SQL configuration
(use-package sql-indent
  :ensure t)

(use-package sql
  :custom
  (sql-product *sql-product*)
  (sql-connection-alist *sql-connection-alist*)
  :hook
  (sql-mode . sqlind-minor-mode)
  :bind (:map sql-mode-map
	      ("C-c C-p" . sql-connect)
	      ("C-<return>" . sql-send-paragraph)))

;; ESS - Emacs Speaks Statistics: R and R Markdown suite

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
  :preface
  (defun insert-pipe ()
    "Insert the pipe (%>%) operator at point, as defined by the magrittr
package."
    (interactive)
    (insert "%>% "))
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

;; Web dev configuration

(use-package mhtml-mode
  :config
  ;; Remove useless `facemenu-keymap' binding, clashing with our custom
  ;; `other-window' binding
  (unbind-key "M-o" html-mode-map)
  (unbind-key "M-o" mhtml-mode-map))

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
  :ensure t
  :init
  (unless (version< emacs-version "29")
    (add-to-list 'major-mode-remap-alist (cons 'javascript-mode #'js2-mode))
    (add-to-list 'major-mode-remap-alist (cons 'js-mode #'js2-mode))))

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

;; TODO use `python-shell-send-block' (Emacs 30+) instead of
;; `python-shell-send-paragraph-or-region'
;; UPDATE: not really. It doesn't do the same thing. But looking at its code
;; could be useful to write a better version of my funciton
(use-package python
  :custom
  (python-indent-offset 4)
  (python-shell-completion-native-enable ; TODO possibly caused by not installing readline from PyPI
   (if (not (equal system-type 'darwin)) t)
   "Native shell completion doesn't work on MacOS")
  :bind
  ;; Remaps that mimic the behavior of ESS
  (:map python-mode-map
   ("C-c C-b" . python-shell-send-buffer)
   ("C-c C-c" . python-shell-send-paragraph-or-region))
  (:map python-ts-mode-map
   ("C-c C-b" . python-shell-send-buffer)
   ("C-c C-c" . python-shell-send-paragraph-or-region))
  :init ; these can be done with local variables
  (setenv "PYTHONPATH"
	  (string-join
	   (mapcar #'expand-file-name *additional-python-source-paths*)
	   path-separator))
  (when *python-virtual-environment-home-path* ; FIXME  why use `when'?
    ;; The easiest way to let pyvenv etc. know where to create virtual environments
    (setenv "WORKON_HOME" (expand-file-name *python-virtual-environment-home-path*))))

;; Activate and make the inferior shell aware of virtual environments
;; FIXME: I don't remember the meaning of hook and shell specifications
(use-package pyvenv
  :ensure t
  :custom
  (pyvenv-exec-shell *shell-binary*)
  :bind
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
	  (#("[pyvenv:" 1 6 (face bold)) pyvenv-virtual-env-name "] "))))

(use-package jupyter
  :ensure t)

;; EIN: Jupyter support (experimental setup: doesn't support lsp)
(use-package ein
  :ensure t
  :custom
  (ein:output-area-inlined-images t)
  (ein:jupyter-default-kernel *python-interpreter-binary*)
  :init
  ;; (with-eval-after-load 'mailcap
    ;; (add-to-list 'mailcap-user-mime-data '((viewer . (concat *ein-image-viewer* " %s"))
					   ;; (type . "image/png"))))
  )

(use-package ein-notebook
  :bind
  (:map ein:notebook-mode-map
	("<S-return>" . ein:worksheet-execute-cell-and-insert-below-km)
	("C-c <deletechar>" . ein:worksheet-delete-cell)
	("M-n" . ein:worksheet-goto-next-input-km)
	("M-p" . ein:worksheet-goto-prev-input-km)))

(use-package ein-notebooklist
  :bind
  (:map ein:notebooklist ("C-c C-k" . ein:stop)))

;; C/C++ configuration and ccls

(use-package cc-mode
  :bind
  (:map c-mode-map
	("<f7>" . compile)
	("<f5>" . gud-gdb)
  :map c++-mode-map
	("<f7>" . compile)
	("<f5>" . gud-gdb))
  :hook
  ((c-mode c++-mode) . (lambda () (c-set-style "custom")))
  :config
  ;; "custom" is identical to "stroustrup" for now, but it can be customized
  (c-add-style "custom" '("stroustrup" (c-basic-offset . 4))))

;; Unlike for other languages (Python), c-ts-mode and c++-ts-mode are their own package

(use-package c-ts-mode
  :preface

  (defun lm/untabified-indent ()
    (indent-tabs-mode -1))

  :hook
  (c-ts-mode . lm/untabified-indent)
  (c++-ts-mode . lm/untabified-indent)
  (c-or-c++-ts-mode . lm/untabified-indent)
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'bsd)
  :bind
  (:map c-ts-mode-map
	("<f7>" . compile)
	("<f5>" . gud-gdb)
   :map c++-ts-mode-map
	("<f7>" . compile)
	("<f5>" . gud-gdb)))

(use-package make-mode
  :bind
   (:map makefile-mode-map ("<f7>" . compile)))

(use-package compile
  :custom
  (compile-command "make -kj "))

(use-package gdb-mi
  :custom
  (gud-gdb-command-name (format "%s -i=mi" *gdb-binary*)))

;; Company backend for C/C++ headers
(use-package company-c-headers
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends #'company-c-headers)
  ;; Ad-hoc config (should cover most cases)
  (dolist (path '("~/.local/include"
		  "/opt/local/include"
		  "/usr/local/include"
		  "/Library/Developer/CommandLineTools/usr/include/c++/v1/"))
    (add-to-list 'company-c-headers-path-system (expand-file-name path))))

;; ccls: C/C++ backend for LSP
;; (unquote if *c/c++-lsp-server-binary* is set to ccls)
'(use-package ccls
  :ensure t
  :custom
  (ccls-executable *c/c++-lsp-server-binary*))

;; Common Lisp support

(use-package slime
  :ensure t
  :commands slime
  :bind (:map lisp-mode-map ("C-c s" . slime))
  :init
  (customize-set-variable 'inferior-lisp-program *lisp-binary*))

(use-package slime-company
  :ensure t
  :config
  (add-to-list 'company-backends #'company-slime))

;; Guile Scheme support

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

;; GDScript support
(use-package gdscript-mode
  :ensure t
  :bind
  (:map gdscript-mode-map
	("C-c <" . gdscript-indent-shift-left)
	("C-c >" . gdscript-indent-shift-right))
  :custom
  (gdscript-use-tab-indents t)
  (gdscript-godot-executable
   (expand-file-name *godot-app-path*)))


;;;
;;; Load `Customize' setup
;;;


(load-file custom-file)
