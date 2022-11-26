;;  init.el --- Personal init configuration for general use, C programming, Python and R -*- lexical-binding:t; coding:utf-8 -*-
;;  Author: Lorenzo Mella <lorenzo.mella@hotmail.it>
;;  Copyright (C) 2021-2022 Lorenzo Mella


;; TODO:
;;
;; 1. org-agenda configuration
;; 2. LaTeX support (`AUCTeX', `pdf-tools', `ebib', etc.)
;; 3. EXWM support: automatically activated if no WM is found (of course, Linux only)
;; 4. all-the-icons (may become obsolete, with full emoji font support)


;;;
;;; Quick-access to user-defined variables
;;;


;; Face families meant to replace the placeholder fonts specified in faces.el
(defvar-local *face-fixed-pitch-family* "Liberation Mono")

(defvar-local *face-fixed-pitch-serif-family* "Nimbus Mono")

(defvar-local *face-variable-pitch-family* "DejaVu Serif")

;; Binaries and paths
(defvar *org-agenda-paths* '("~/notes/tasks/"))

(defvar *shell-binary* (getenv "SHELL") ; using the default for the current user
  "Preferred shell to use with term/ansi-term.")

(defvar *python-interpreter-binary* "python3"
  "Preferred python or ipython interpreter.")

(defvar *pylsp-binary* "~/.local/bin/pylsp"
    "Path to the pylsp executable.")

(defvar *gdb-binary* "/usr/bin/gdb"
  "gdb executable to use with gdb and gud.")

(defvar *ccls-binary* "/usr/bin/ccls"
  "Path to the ccls executable.")

(defvar *lisp-interpreter* "lisp"
  "The name (or path) of the Common Lisp interpreter of choice)")

(defvar *additional-texinfo-directories* '("/opt/local/share/info")
  "List of the nonstandard texinfo paths.")

(defvar *additional-python-source-paths* '("~/.local/site_packages/")
  "Paths to be added to `python-shell-extra-pythonpaths'")

(defvar *python-virtual-environment-home-path* "~/.virtualenvs"
  "The directory where the Python Virtual Environments are located. Ignored if nil.")

(defvar *texlive-bin-path* "/usr/local/texlive/bin"
  "Path to the TeXlive binaries.")

(defvar *additional-bin-paths* '("~/.local/bin")
  "List of paths to additional binaries.")

(defvar *preferred-sql-product* 'postgres
  "The most likely SQL dialect in use (a symbol).")

(defvar *ein-image-viewer* "/bin/feh --image-bg white"
  "Image viewing program used by the ein package (with arguments).")

;; Other settings
(defvar *gc-bytes* (* 50 1024 1024) ; 50MB
  "Preferred heap threshold size to start garbage collection.")

(defvar *custom-file-name*
  (expand-file-name "custom-file.el" user-emacs-directory)
  "`Customize' will save its settings in this file.
Set it to `nil' to append to this file.")

(defvar *transparency-level* 0.97
  "Global frame transparency parameter (involving both background and text).")

(defvar *preferred-browser* #'browse-url-default-browser
  "Any one of the browser symbols defined by the browse-url package.")

(defvar *initial-scratch-message*
  ";;                              __       __
;;   __/|____________________ _/ /______/ /_  __/|_
;;  |    / ___/ ___/ ___/ __ `/ __/ ___/ __ \\|    /
;; /_ __(__  ) /__/ /  / /_/ / /_/ /__/ / / /_ __|
;;  |/ /____/\\___/_/   \\__,_/\\__/\\___/_/ /_/ |/\n\n\n"
  "Replacement of the trite *scratch* message with ASCII art.")


;;;
;;; General purpose custom functions
;;;


(defun lm/custom-settings ()
  "Opens the configuration file currently defined as `user-init-file'."
  (interactive)
  (find-file-existing user-init-file))

(defalias 'init-show 'lm/custom-settings)

(defun lm/frame-resize-and-center (width-fraction)
  "Resizes the frame to about two thirds of the screen."
  (interactive (list 0.618)) ; Using the inverted golden ratio in place of 2/3
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


;;;
;;; Package management
;;;


(require 'package)

;; Add references to online repositories other than GNU ELPA
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; Use the stable Melpa repo (as a replacement for Melpa) for a more curated experience.
;; I'm not using it in this config because the package ccls is not in Stable yet!
;; (add-to-list 'package-archives '("s-melpa" . "https://stable.melpa.org/packages/"))

(package-initialize)

;; Package management will be handled by use-package
(unless (package-installed-p 'use-package)
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


;; Remove "legacy" GUI widgets
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Remap commands to more convenient keys (the defaults still work)
(bind-key "M-o" #'other-window)
(bind-key "C-M-;" #'comment-line)
(bind-key "s-=" #'text-scale-increase)
(bind-key "s--" #'text-scale-decrease)

;; Remap the insert-char shortcuts
(cond ((eq system-type 'gnu/linux)    ; Avoid Gnome incompatibilities 
       (bind-key "<s-return>" 'iso-transl-ctl-x-8-map key-translation-map)
       (bind-key "<s-return> RET" #'insert-char))
      (t 
       (bind-key "s-8" 'iso-transl-ctl-x-8-map key-translation-map)
       (bind-key "s-8 RET" #'insert-char)))
  

;; Window-resizing keybindings
(unbind-key "s-m") ; Normally bound to `iconify-frame' on MacOS. Use `C-z' instead
(bind-key "s-m f" #'toggle-frame-fullscreen)
(bind-key "s-m m" #'toggle-frame-maximized)
(bind-key "s-m c" #'lm/frame-resize-and-center)
(bind-key "s-m s" #'window-toggle-side-windows)

;; Remap keys to more convenient commands
(bind-key [remap kill-buffer] #'kill-current-buffer)
(bind-key [remap kill-buffer] #'quit-window ; Never kill *scratch* by accident
	  lisp-interaction-mode-map (string-equal (buffer-name) "*scratch*"))
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

;; Quicken many confirmation prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Mouse scrolling configuration
(customize-set-variable 'mouse-wheel-tilt-scroll t
  "Horizontal scrolling on touchpads,Apple Magic Mouse and mice with lateral wheel click")
(customize-set-variable 'mouse-wheel-flip-direction t
  "Natural orientation for horizontal scrolling")
;; These are for scrolling even when the Emacs frame is
;; in the background, to achieve Mac-like behavior
;; (works under Gnome 3.x and 40. Untested on other gtk WMs)
(when (eq (window-system) 'x)
  (bind-key "<s-mouse-4>" #'mwheel-scroll)
  (bind-key "<s-mouse-5>" #'mwheel-scroll))

;; Display Buffer customization
(use-package window
  :custom
  (display-buffer-alist
   `(
     '("\\*\\(Async \\)?Shell Command.*"
       do nothing)
     ("\\*eldoc\\*"
      display-buffer-in-side-window
      (side . bottom)
      (slot . 0)
      (dedicated . t)
      (window-parameters . ((no-other-window . t))))
     ("\\*[Pp]ython.*"
      display-buffer-in-side-window
      (side . bottom)
      (slot . 1)
      (dedicated . t)))))

;; Dired customization
(use-package dired
  :init
  (require 'dired-x)
  :custom
  (dired-auto-revert-buffer t
   "Refresh the dired buffer whenever unburied")
  (dired-use-ls-dired (if (eq system-type 'gnu/linux) 'unspecified)
   "nil on Macs to avoid a warning")
  (dired-listing-switches
   (if (eq system-type 'gnu/linux)
       "-lahFb --group-directories-first"
     "-lahFb")
   "ls -l readability adjustments. Group directories first when using coreutils ls")
  (dired-ls-F-marks-symlinks (eq system-type 'darwin)
			     "Rename symlinks correctly, when marked with '@' by ls -lF")
  :hook
  (dired-mode . hl-line-mode))

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
  ;; (org-agenda-files (list (expand-file-name "~/notes")))
  (org-todo-keywords '((sequence "TODO(t)"
				 "WAITING(w)"
				 "|"
				 "CANCELLED(c)"
				 "DONE(d)")))
  :hook
  (org-mode . visual-line-mode)
  (org-agenda-mode . hl-line-mode)
  :bind
  (:map org-mode-map
   ("C-c t" . org-tags-view))
  ("C-c a" . org-agenda)
  :config
  ;; Additional code-block expansions
  (dolist (elem '(("b" . "src bash") ("conf" . "src conf")
		  ("el" . "src emacs-lisp") ("py" . "src python")))
    (add-to-list 'org-structure-template-alist elem))
  ;; Better initial scaling of latex preview rendering in org documents
  (plist-put org-format-latex-options :scale 2.0)
  (dolist (x '(python jupyter))
    (add-to-list 'org-babel-load-languages `(,x . t))))

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
   (append Info-additional-directory-list *additional-texinfo-directories*)))

;; Doc View configuration
(use-package doc-view
  :custom
  (doc-view-continuous t
   "Change page when scrolling beyond the top/bottom"))

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

;; Resize window pixel-wise with mouse
(customize-set-variable 'frame-resize-pixelwise t)

;; Optionally transparent frame
(customize-set-variable 'default-frame-alist
			(append default-frame-alist
				`((alpha . ,*transparency-level*))))

;; Replace the default scratch message
(customize-set-variable 'initial-scratch-message *initial-scratch-message*)

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
  (display-time-format "%a %d %b %y %T"))

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


;; Add additional paths to both the environment variable PATH and the
;; Emacs exec-path list
(add-to-list 'exec-path (expand-file-name *texlive-bin-path*))
(dolist (path *additional-bin-paths*)
  (add-to-list 'exec-path (expand-file-name path)))

(setenv "PATH" (cl-reduce (lambda (path rest) (concat path ":" rest)) exec-path))

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
  (dashboard-startup-banner 'logo)
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
  :bind
  ("C-;" . avy-goto-char))

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
  ("C-x 4 b" . counsel-switch-buffer-other-window))

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
      _ "\n\n#endif /* " str " */"))
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

;; Magit: highly comfy git interface
(use-package magit
  :ensure t
  :commands magit-status
  :bind
  ("C-x g" . magit-status))

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
  (company-minimum-prefix-length 2)
  :hook
  ((prog-mode . company-mode)
   (prog-mode . yas-minor-mode)))

;; Eldoc configuration
(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t))

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
  :hook
  ((python-mode c-mode c++-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs `(c-mode ,*ccls-binary*))
  (add-to-list 'eglot-server-programs `(python-mode ,*pylsp-binary*)))

;; SQL configuration
(use-package sql
  :custom
  (sql-product *preferred-sql-product*))

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
    (save-excursion (python-shell-send-region
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
  :custom
  (python-indent-offset 4)
  (python-shell-completion-native-enable
   (not (eq system-type 'darwin))
   "Native shell completion doesn't work on MacOS")
  :bind
  (:map python-mode-map
   ;; Remaps that mimic the behavior of ESS
   ("C-c C-b" . python-shell-send-buffer)
   ("C-c C-c" . python-shell-send-paragraph-or-region))
  :init					; these can be done with local varialbes
  (setenv "PYTHONPATH"
	  (string-join
	   (mapcar #'expand-file-name *additional-python-source-paths*)
	   path-separator))
  (when *python-virtual-environment-home-path*
    (setenv "WORKON_HOME" (expand-file-name *python-virtual-environment-home-path*))))

;; ipython-shell-send: send snippets to inferior IPython shells (I
;; haven't tested it well)
;; (use-package ipython-shell-send
;; :ensure t)

;; Activate and make the inferior shell aware of virtual environments
;; FIX: I don't remember the meaning of hook and shell specifications
(use-package pyvenv
  :ensure t
  :custom
  (pyvenv-exec-shell *shell-binary*)
  :bind
  ("C-c v a" . pyvenv-activate)
  ("C-c v d" . pyvenv-deactivate)
  ("C-c v w" . pyvenv-workon)
  ("C-c v r" . pyvenv-restart-python)
  :hook
  ((python-mode inferior-python-mode) . pyvenv-mode)
  :config
  (setq pyvenv-mode-line-indicator
	'(pyvenv-virtual-env-name
	  ("[pyvenv:" pyvenv-virtual-env-name "] "))))

(use-package jupyter
  :ensure t)

;; EIN: Jupyter support (experimental setup: doesn't support lsp)
(use-package ein
  :ensure t
  :custom
  (ein:output-area-inlined-images nil)
  (ein:jupyter-default-kernel *python-interpreter-binary*)
  :init
  (with-eval-after-load 'mailcap	; FIX: probably not portable
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
  (:map ein:notebooklist ("C-c C-k" . ein:stop))) ; FIX: should be of the form 'C-c [key]'

;; C/C++ configuration and ccls

(use-package cc-mode
  :hook
  ((c-mode c++-mode) . c-toggle-hungry-state)
  :bind
  (:map c-mode-map
   ("<f5>" . compile)
   :map c++-mode-map
   ("<f5>" . compile)))

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
  (ccls-executable *ccls-binary*))

;; Debugger interface
(use-package gdb-mi
  :custom
  (gud-gdb-command-name *gdb-binary*))

;; Common Lisp support

(use-package slime
  :ensure t
  :init
  (customize-set-variable 'inferior-lisp-program *lisp-interpreter*)
  ;; :bind
  ;; (:map scheme-mode-map ("C-c C-p" . slime)) ; in analogy to Python Mode
)

;; Guile-Scheme support

(use-package geiser
  :ensure t)

(use-package geiser-guile
  :ensure t
  :after geiser
  :custom
  (geiser-debug-jump-to-debug-p nil
   "Don't jump to the debug buffer")
  (geiser-debug-show-debug-p nil
   "Don't show the debug buffer")
  (geiser-guile-manual-lookup-other-window-p t
   "Open info entries in another window")
  :bind
  (:map scheme-mode-map ("C-c C-p" . run-geiser))) ; in analogy to Python Mode

;; Meme "Wizard Book" in Texinfo format. Read it with `M-x info'.
;; Also worth checking:
;; - a homebrew LaTeX version at:
;;   https://github.com/sarabander/sicp-pdf.git
;; - and the original free html format at:
;;   https://mitpress.mit.edu/sites/default/files/sicp/index.html
(use-package sicp
  :ensure t)


;;;
;;; Load `Customize' setup
;;;


(load-file *custom-file-name*)
