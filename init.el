;;  init.el --- Personal init configuration for general use, C programming, Python and R
;;  Author: Lorenzo Mella <lorenzo.mella@hotmail.it>
;;  Copyright (C) 2021 Lorenzo Mella


;; TODO:
;;
;; 1. org-agenda configuration
;; 2. LaTeX support (`AUCTeX', `pdf-tools', `ebib', etc.)
;; 3. EXWM support: automatically activated if no WM is found (of course, Linux only)
;; 4. (optional) modern-style scrolling (high value for `scroll-conservatively', etc.)
;; 5. all-the-icons


;;;
;;; Quick-access to user-defined variables
;;;


;; Face families meant to replace the placeholder fonts specified in faces.el
(defvar-local *face-fixed-pitch-family* "Menlo")

(defvar-local *face-fixed-pitch-serif-family* "Courier")

(defvar-local *face-variable-pitch-family* "Helvetica")

;; Binaries and paths
(defvar-local *shell-binary* (getenv "SHELL") ; using the default for the current user
  "Preferred shell to use with ansi-term.")

(defvar-local *python-interpreter-binary* "python3"
  "Preferred python or ipython interpreter.")

(defvar-local *pylsp-binary* (expand-file-name "~/Library/Python/3.8/bin/pylsp")
    "Path to the pylsp executable.")

(defvar-local *additional-texinfo-directories* (list "/opt/local/share/info/")
  "List of the nonstandard texinfo paths")

(defvar-local *pdflatex-binary* "/Library/TeX/texbin/pdflatex"
  "Path to the pdflatex renderer")

(defvar-local *gdb-binary* "/usr/bin/gdb"
  "gdb executable to use with gdb and gud.")

(defvar-local *ccls-binary* "/opt/local/bin/ccls-clang-8.0"
  "Path to the ccls executable.")

(defvar-local *ein-image-viewer* "/bin/feh --image-bg white"
  "Image viewing program used by the ein package (with arguments).")

;; Other settings
(defvar-local *gc-bytes* (* 50 1024 1024) ; 50MB
  "Preferred heap threshold size to start garbage collection.")

(defvar-local *transparency-level* 0.97
  "Global frame transparency parameter (involving both background and text).")

(defvar-local *preferred-browser* #'browse-url-default-browser
  "Any one of the browser symbols defined by the browse-url package.")

(defvar-local *initial-scratch-message*
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

(defun lm/frame-resize-and-center (width-fraction)
  "Resizes the frame to about two thirds of the screen."
  (interactive (list 0.618)) ; Using the inverted golden ratio in place of 2/3
  (let* ((workarea (alist-get 'workarea (car (display-monitor-attributes-list))))
	 (new-width (floor (* (caddr workarea) width-fraction)))
	 (new-height (cadddr workarea))
	 (top-left-x (+ (car workarea) (/ (- (caddr workarea) new-width) 2)))
	 (top-left-y (cadr workarea)))
    (set-frame-position (window-frame) top-left-x top-left-y)
    (set-frame-size (window-frame) new-width new-height t))) ; TODO: somehow it is not centered. Fringes problem?

(defalias 'init-show 'lm/custom-settings)
(defalias 'custom-settings 'lm/custom-settings)


;;;
;;; Package management
;;;


;; Load the native package management functionality
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
(use-package  gnu-elpa-keyring-update
  :ensure t)


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

;; Window resizing
(bind-key "s-f" #'toggle-frame-fullscreen)
(unbind-key "s-m")			; "Iconify Window" on MacOS
(bind-key "s-m m" #'toggle-frame-maximized)
(bind-key "s-m c" #'lm/frame-resize-and-center)

;; Remap keys to more convenient commands
(bind-key [remap kill-buffer] #'kill-current-buffer)
(bind-key [remap kill-buffer] #'quit-window ; Never kill *scratch* by accident
	  lisp-interaction-mode-map (string-equal (buffer-name) "*scratch*"))
(bind-key [remap capitalize-word] #'capitalize-dwim)
(bind-key [remap downcase-word] #'downcase-dwim)
(bind-key [remap upcase-word] #'upcase-dwim)

;; Always visualize column numbers
(column-number-mode)

;; Replace/delete the active region when keys/backspace are pressed
(delete-selection-mode)

;; Follow symlinks when calling find-file (useful for git awareness)
(customize-set-variable 'find-file-visit-truename t)

;; Quicken many confirmation prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; These are for mouse scrolling even when the Emacs frame is
;; in the background, to achieve Mac-like behavior
;; (works under Gnome 3.x and 40. Untested on other gtk WMs)
(when (eq system-type 'gnu/linux)
  (bind-key "<s-mouse-4>" #'mwheel-scroll)
  (bind-key "<s-mouse-5>" #'mwheel-scroll))

;; Dired customization
(use-package dired
  :init
  (require 'dired-x)
  :custom
  (dired-auto-revert-buffer t "Refresh the dired buffer whenever unburied")
  (dired-use-ls-dired (if (eq system-type 'gnu/linux) 'unspecified) "nil on Macs to avoid a warning")
  (dired-listing-switches
   (if (eq system-type 'gnu/linux)
       "-lahF --group-directories-first"
     "-lahF")
   "ls -l readability adjustments. Group directories first when using coreutils ls")
  (dired-ls-F-marks-symlinks t "Rename symlinks correctly, if when marked with '@' by ls -lF"))

;; Org customization
(use-package org
  :custom
  (org-hide-emphasis-markers t)
  (org-ellipsis " ▸")
  :hook
  (org-mode . visual-line-mode)
  :config
  (customize-set-variable 'org-structure-template-alist
			  (append org-structure-template-alist
				  '(("b" . "src bash") ("conf" . "src conf")
				    ("el" . "src emacs-lisp") ("py" . "src python")))
			  "Additional code-block expansions."))

(use-package org-tempo
  :after org)

;; Help buffer customization
(use-package help
  :hook
  (help-mode . visual-line-mode)
  :custom
  (help-window-select t	"Switch focus to a help window automatically, when created"))

;; Man buffer customization
(use-package man
  :custom
  (Man-notify-method 'aggressive "Open Man in another window and switch focus to it"))

;; Info mode customization
(use-package info
  :hook
  (Info-mode . visual-line-mode)
  :custom
  (Info-additional-directory-list
   (append Info-additional-directory-list *additional-texinfo-directories*)))

;; GUI browser configuration
(use-package browse-url
  :custom
  (browse-url-browser-function *preferred-browser*))


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
			(append default-frame-alist `((alpha . ,*transparency-level*))))

;; Replace the default scratch message
(customize-set-variable 'initial-scratch-message *initial-scratch-message*)

;; Convert non-visible ^L (form feed) into a horizontal line
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

;; hl-line-mode in selected bundled modes
(use-package hl-line
  :hook
  ((package-menu-mode org-agenda-mode) . hl-line-mode))

;; Isolate themes not managed by `package' or `use-package' (e.g., user-created ones)
(let ((theme-directory (expand-file-name (concat user-emacs-directory "/themes"))))
  (when (file-exists-p theme-directory)
    (customize-set-variable 'custom-theme-directory theme-directory)))

;; Custom face tweaks
;;
;; For most people, it will suffice doing `M-x customize-faces' on the `default' face,
;; (deselect Foreground and Background if active, so they don't interfere with changing the theme)
;; and then applying a theme.
;;
;; - However, the standard faces `fixed-pitch', `fixed-pitch-serif' and `variable-pitch',
;;   inherited by many others, are defined with generic font names, which I modify.
;;
;; - I reduce the height of `line-number' by 20% for aesthetic reasons.
;;
;; - I modify `custom-variable-obsolete' with a strike-through, again, for emphasis.

(use-package faces
  :custom-face
  ;; Commas required for substitutions (where's the backquote? Hidden in use-package custom-face handler)
  (fixed-pitch ((t (:inherit unspecified :family ,*face-fixed-pitch-family*))))
  (fixed-pitch-serif ((t (:inherit unspecified :family ,*face-fixed-pitch-serif-family*))))
  (variable-pitch ((t (:inherit unspecified :family ,*face-variable-pitch-family*))))
  (line-number ((t (:inherit (shadow default) :height 0.8)))))

(use-package cus-edit
  :custom-face
  (custom-variable-obsolete ((t (:inherit custom-variable-tag :strike-through t :weight normal)))))


;;;
;;; Environment and Shell Configuration
;;;


;; Add local executables (e.g. Python pip installation)
(setenv "PATH" (concat (expand-file-name "~/.local/bin") ":" (getenv "PATH"))) ; In the actual process environment
(add-to-list 'exec-path (expand-file-name "~/.local/bin")) ; In the Emacs Lisp variable

;; Automate the interactive shell query of ansi-term
(advice-add 'ansi-term :filter-args '(lambda (shell-name)
				       (interactive (list *shell-binary*))
				       shell-name))

;; Set the environment locale in case Emacs defaults to nil or "C" (this may happen on MacOS)
(dolist (env-var '("LANG" "LANGUAGE" "LC_CTYPE" "LC_COLLATE" "LC_TIME" "LC_MESSAGES" "LC_MONETARY"))
  (let (locale (getenv env-var))
    (when (or (null locale) (string= locale "C"))
      (setenv env-var "en_US.UTF-8"))))


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
  (dashboard-startup-banner 'logo "Load alternative logo")
  (dashboard-set-footer nil)
  :hook
  ;; Hooks effective when dashboard is initially run
  (dashboard-mode . hl-line-mode)
  (dashboard-mode . dashboard-jump-to-recent-files)
  ;; Hooks effective after resizing the frame
  (dashboard-after-initialize . hl-line-mode)
  (dashboard-after-initialize . dashboard-jump-to-recent-files)
  :bind
  (:map dashboard-mode-map
	("n" . dashboard-next-line)
	("p" . dashboard-previous-line)))

;; Avy: jump easily to any visible text in every frame/window
(use-package avy
  :ensure t
  :bind
  ("C-;" . avy-goto-char))

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

;; Eglot support for Microsoft's Language Server Protocol (LSP)

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs `(c-mode ,*ccls-binary*))
  (add-to-list 'eglot-server-programs `(python-mode ,*pylsp-binary*)))

;; ESS - Emacs Speaks Statistics: R and R Markdown suite (FIX: check whether the keymaps are loaded appropriately)

;; Automate the pdf rendering of rmarkdown projects
(defun rmarkdown-render (filename &optional verbose)
  "Run rmarkdown::render on the chosen file.
R and the rmarkdown package, and appropriate renderers,
must be installed at a minimum."
  (interactive "FFile to render: ")
  (message "Rendering file \"%s\"..." filename)
  (shell-command
   (format "Rscript -e \"rmarkdown::render('%s')\" > /dev/null; %s %s.tex > /dev/null"
	   filename *pdflatex-binary* (file-name-sans-extension filename)))
  (message "... done!"))

(defun insert-pipe ()
  "Insert the pipe (%%>%%) operator at point, as defined by the magrittr package."
  (interactive)
  (insert "%>% "))

(use-package ess
  :ensure t
  :custom
  (ess-use-ido nil)
  (ess-style 'RStudio)
  :config
  (with-eval-after-load 'ess-r-mode
    (bind-key "C-c h" #'ess-help ess-r-mode-map)
    (bind-key "C-c r" #'rmarkdown-render ess-r-mode-map)
    (bind-key "C->" #'insert-pipe ess-r-mode-map)))

;; Poly-R: R-Markdown support based on poly-mode
(use-package poly-R
  :ensure t
  :after ess)

;; Python configuration

(use-package python
  :hook
  (python-mode . eglot-ensure)
  (python-mode . lm/venv-create-when-needed)
  :custom
  (python-indent-offset 4))

(defun lm/venv-create-when-needed ()
  "Create a Python virtual environment in the current directory.

Asks the user if he/she wants to set up a Python virtual environment
(using venv internally). Asks also for a choice of Python interpreter,
assuming that the corresponding pip version is installed."
  (interactive)
  (when (and (not (file-exists-p "pyvenv.cfg"))
	     (yes-or-no-p "No Python virtual environment detected. Create? "))
    (let* ((python-bin-name (read-shell-command "Select Python executable: "
						*python-interpreter-binary*))
	   (python-bin (executable-find python-bin-name)))
      (if (file-exists-p python-bin)
	  (progn (message "Creating Python virtual environment... ")
		 (shell-command (format "%s -m venv %s" python-bin default-directory))
		 (message "Environment created in directory %s" default-directory))
	(message "Python interpreter %s not found." python-bin)))))

;; ipython-shell-send: send snippets to inferior IPython shells (I haven't tested it well)
(use-package ipython-shell-send
  :ensure t)

;; Activate and make the inferior shell aware of virtual environments
;; FIX: I don't remember the meaning of hook and shell specifications
(use-package pyvenv
  :ensure t
  :custom
  (pyvenv-exec-shell *shell-binary*)
  :hook
  ((python-mode inferior-python-mode) . pyvenv-mode))

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
	("C-c C-g" . insert-guards)
	("<f5>" . compile)
   :map c++-mode-map
	("C-c C-g" . insert-guards)
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
  :after eglot
  :hook
  ((c-mode c++-mode) . eglot-ensure)
  :custom
  (ccls-executable *ccls-binary*))

;; Debugger interface
(use-package gdb-mi
  :custom
  (gud-gdb-command-name *gdb-binary*))

;; Insert Guards
(defun insert-guards (guard-name)
  "Insert correctly formatted header guards in the file
(for use with c-mode and c++-mode)."
  (interactive "sMacro for the guards? ")
  (when (string-equal guard-name "")
    (setq guard-name (buffer-name)))
    ;; Remove the extension
    (setq guard-name (replace-regexp-in-string "\\..*$" "" guard-name))
    ;; Replace characters forbidden in C names with underscores
    (setq guard-name (replace-regexp-in-string "[^a-zA-z0-9_]" "" guard-name))
    ;; Add bounding 'H' characters to make the macro more unique
    (setq guard-name (format "H_%s_H" (upcase guard-name)))
  (save-excursion
    (goto-char (point-min))
    (insert (format "#ifndef %s\n#define %s\n\n\n" guard-name guard-name))
    (goto-char (point-max))
    (insert (format "\n\n#endif /* %s */\n" guard-name)))
  ;; Center the point in between the guards if the window was empty.
  (when (<= (point) 2) (move-to-window-line 4))
  ;; Warning in case of suspect filename
  (when (or (null (buffer-file-name)) (not (string-match "\\.[hH]$" (buffer-file-name))))
    (message "Are you editing a header file (C/C++/Objective-C)?")))

;; Guile-Scheme support

(use-package geiser
  :ensure t)

(use-package geiser-guile
  :ensure t
  :after geiser
  :custom
  (geiser-debug-jump-to-debug-p nil "Don't jump to the debug buffer")
  (geiser-debug-show-debug-p nil "Don't show the debug buffer")
  (geiser-guile-manual-lookup-other-window-p t "Open info entries in another window")
  :bind
  (:map scheme-mode-map ("C-c C-p" . run-geiser))) ; in analogy to Python Mode

;; Meme "Wizard Book" in Texinfo format. Read it with `M-x info'.
;; Also worth checking: a homebrew LaTeX version at: https://github.com/sarabander/sicp-pdf.git
;; and the original free html format at: https://mitpress.mit.edu/sites/default/files/sicp/index.html
(use-package sicp
  :ensure t)


;;;
;;; A section managed by `customize' will be appended here
;;;
