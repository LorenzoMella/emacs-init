;;  init.el --- Personal init configuration for general use, C programming, Python and R
;;  Author: Lorenzo Mella <lorenzo.mella@hotmail.it>
;;  Copyright (C) 2021 Lorenzo Mella


;; TODO:
;;
;; 1. org-agenda configuration
;; 2. LaTeX support (AUCTeX?)
;; 3. EXWM support: automatically activated if no WM is found (of course,Linux only)
;; 4. all-the-icons
;;
;; BUGS:
;;
;; ess-mode-hook not working properly at the start of the session. Fine afterwards.


;;;
;;; Quick-access to user-defined variables
;;;


(defvar-local *gc-bytes* (* 50 1024 1024) ; 50MB
  "Preferred heap threshold size to start garbage collection.")

(defvar-local *preferred-browser* #'browse-url-default-browser
  "The browser used by browse-url.")

;; Little bash script to find all candidates for a binary (e.g., ccls)
;; (shell-command
;;  "for TOKEN in ${PATH//:/ }; do
;;     [[ -x $TOKEN ]] && CANDIDATE=$(ls $TOKEN | grep ccls)
;;     [[ $CANDIDATE ]] && which $CANDIDATE
;;   done")

(defvar-local *shell-binary* "/bin/bash"
  "Preferred shell to use with ansi-term.")

(defvar-local *python-interpreter-binary* "/opt/local/bin/python3"
  "Path to the preferred python or ipython interpreter.")

(defvar-local *ein-image-viewer* "/bin/feh --image-bg white"
  "Image viewing program used by the ein package (with arguments).")

(defvar-local *jedi-language-server-bin-path*
  (expand-file-name "~/Library/Python/3.9/bin/jedi-language-server")
  "Path to the jedi-language-server executable.")

(defvar-local *ccls-bin-path* "/opt/local/bin/ccls-clang-8.0"
  "Path to the ccls executable.")

(defvar-local *initial-scratch-message*
  ";;                              __       __
;;   __/|____________________ _/ /______/ /_  __/|_
;;  |    / ___/ ___/ ___/ __ `/ __/ ___/ __ \\|    /
;; /_ __(__  ) /__/ /  / /_/ / /_/ /__/ / / /_ __|
;;  |/ /____/\\___/_/   \\__,_/\\__/\\___/_/ /_/ |/\n\n\n"
  "Replacement of the trite *scratch* message with ASCII art.")


;;;
;;; Shortcut to this init file
;;;


(defun custom-settings ()
  "Opens the configuration file currently defined as `user-init-file'."
  (interactive)
  (find-file-existing user-init-file))


;;;
;;; Package management
;;;


;; Load the native package management functionality
(require 'package)

;; Add references to additional online repositories, besides GNU ELPA
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

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
(setq gc-cons-threshold *gc-bytes*)

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

;; Window resizing, Mac-specific. On Linux I configure equivalent DE shortcuts instead
(when (equal window-system 'ns)
  (bind-key "s-f" #'toggle-frame-fullscreen)
  (bind-key "s-m" #'toggle-frame-maximized))

;; Remap keys to more convenient commands
(bind-key [remap kill-buffer] #'kill-current-buffer)
(bind-key [remap kill-buffer] #'quit-window ; Never kill *scratch* by accident
	  lisp-interaction-mode-map	    ; FIX: *scratch* can be used with other major modes!!
	  (string-equal (buffer-name) "*scratch*"))
(bind-key [remap capitalize-word] #'capitalize-dwim)
(bind-key [remap downcase-word] #'downcase-dwim)
(bind-key [remap upcase-word] #'upcase-dwim)

;; Always visualize column numbers
(column-number-mode)

;; Replace/delete the active region when keys/backspace are pressed
(delete-selection-mode)

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
  (dired-use-ls-dired (if (eq system-type 'darwin)
			  nil
			'unspecified) "Avoids a warning")
  (dired-listing-switches (if (eq system-type 'gnu/linux)
			      "-lahF --group-directories-first"
			    "-lahF") "Group directories when using coreutils ls")
  (dired-ls-F-marks-symlinks t "Rename symlinks correctly, if when marked with '@' by ls -lF"))

;; Org customization
(use-package org-mode
  :hook
  (org-mode . visual-line-mode))

;; Activate Help windows as they are opened
(use-package help
  :custom
  (help-window-select t "Switch to help window automatically"))

;; GUI browser configuration
(use-package browse-url
  :custom
  (browse-url-browser-function		; Somehow, default browser
   (if (and (eq system-type 'gnu/linux) (file-exists-p *preferred-browser*))
       browse-url-epiphany
     browse-url-default-browser)))


;;;
;;; Aesthetic adjustments
;;;


;; Replace the default scratch message
(use-package startup
  :defer
  :custom
  (initial-scratch-message *initial-scratch-message*))

;; Convert non-visible ^L (form feed) into a horizontal line
(global-page-break-lines-mode)

;; hl-line-mode in selected bundled modes
(use-package hl-line
  :hook
  (package-menu-mode . hl-line-mode)
  (org-agenda-mode . hl-line-mode))

;; Custom face tweaks
;;
;; 1. For most people, it will suffice using `customize' on the `default' face,
;;    WITHOUT changing foreground and background colors, and then apply a theme.
;;
;; 2. `Info-quoted' and `info-menu-header' have messed up defaults as of version 27.2.
;;    I redefine them in a sane way.
;;
;; 3. I modify `custom-variable-obsolete' with a strike-through, for emphasis.
;;
;; 4. I reduced the height of `line-number' by 20% for aesthetic reasons.

(use-package info
  :custom-face
  (Info-quoted ((t (:inherit default :underline t))))
  (info-menu-header ((t (:weight bold :family "Sans Serif")))))

(use-package cus-edit
  :custom-face
  (custom-variable-obsolete ((t (:inherit custom-variable-tag :strike-through t :weight normal)))))

(use-package faces
  :custom-face
  (line-number ((t (:inherit (shadow default) :height 0.8)))))


;;;
;;; Environment and Shell Configuration
;;;


;; Add local executables (e.g. Python pip installation)
(setenv "PATH" (concat "~/.local/bin:" (getenv "PATH"))) ; In the actual process environment
(add-to-list 'exec-path "~/.local/bin")			 ; In the ELisp variable

;; Automate the interactive shell query of ansi-term
(defun lm/default-shell-name (shell-name)
  (interactive (list *shell-binary*))
  shell-name)

(advice-add 'ansi-term :filter-args 'lm/default-shell-name)


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
  (dashboard-mode . hl-line-mode)
  ;; Reposition the cursor when Emacs is initially run
  (dashboard-after-initialize . dashboard-jump-to-recent-files)
  ;; Reposition the cursor after resizing the frame
  (dashboard-mode . dashboard-jump-to-recent-files)
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
  (prescient-persist-mode))

;; Interface between the Prescient engine and Ivy
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

;; Emacs support for Microsoft Language Server Protocol
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  ((c-mode c++-mode python-mode) . lsp)
  (lsp-mode . lsp-enable-which-key-integration))

;; ESS - Emacs Speaks Statistics: R and R Markdown suite

;; Automate the pdf rendering of rmarkdown projects
(defun rmarkdown-render (filename)
  "Run rmarkdown::render on the chosen file.
R and the rmarkdown package, and appropriate renderers,
must be installed at a minimum."
  (interactive "File to render: ")
  (message "Rendering file \"%s\"..." filename)
  (shell-command
   (format "Rscript -e \"rmarkdown::render('%s')\" > /dev/null" filename))
  (message "... done!"))

(defun insert-pipe ()
  "Insert the pipe (%%>%%) operator at point, as defined by the Tidyverse magrittr package."
  (interactive)
  (insert "%>% "))

(use-package ess
  :ensure t
  :custom
  (ess-use-ido nil)
  (ess-style 'RStudio)
  :bind
  (:map ess-mode-map
	("C-c h" . ess-help)
	("C-c r" . rmarkdown-render)
	("C->" . insert-pipe)))

;; Poly-R: R-Markdown support based on poly-mode
(use-package poly-R
  :ensure t
  :after ess)

;; Python configuration and lsp-jedi

'(use-package python
  :custom
  (python-shell-interpreter *python-interpreter-binary*))

;; ipython-shell-send: send snippets to inferior IPython shells (I haven't tested it well)
'(use-package ipython-shell-send
   :ensure t)

;; Company backend for Python using jedi
(use-package company-jedi
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends #'company-jedi))

;; Jedi Language Server backend for LSP
(use-package lsp-jedi
  :ensure t
  :after lsp-mode
  :init
  (add-to-list 'lsp-enabled-clients 'jedi)
  (add-to-list 'lsp-disabled-clients 'pyls) ; Avoid using Palantir's backend if installed
  :custom
  (lsp-jedi-executable-command *jedi-language-server-bin-path*))

;; EIN: Jupyter support
(use-package ein
  :ensure t
  :bind
  ("C-c C-j" . ein:run)
  :custom
  (ein:output-area-inlined-images nil) ; The default, as of =ein= version 05/05/2021
  :init
  (with-eval-after-load 'mailcap	; FIX: probably not portable
    (add-to-list 'mailcap-user-mime-data '((viewer . (concat *ein-image-viewer* " %s"))
					   (type . "image/png"))))
  :config
  (setq ein:jupyter-default-kernel *python-interpreter-binary*))

(use-package ein-notebook
  :bind
  (:map ein:notebook-mode-map
	("<C-return>" . ein:worksheet-execute-cell-and-insert-below-km)
	("M-n" . ein:worksheet-goto-next-input-km)
	("M-p" . ein:worksheet-goto-prev-input-km)))

(use-package ein:notebooklist
  :bind
  (:map ein:notebooklist
	("C-c C-k" . ein:stop)))

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
  :after lsp-mode
  :init
  (add-to-list 'lsp-enabled-clients 'ccls)
  :custom
  (ccls-executable *ccls-bin-path*))

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
    (beginning-of-buffer)
    (insert (format "#ifndef %s\n#define %s\n\n\n" guard-name guard-name))
    (end-of-buffer)
    (insert (format "\n\n#endif /* %s */\n" guard-name)))
  ;; Center the point in between the guards if the window was empty.
  (when (<= (point) 2) (move-to-window-line 4))
  ;; Warning in case of suspect filename
  (when (or (null (buffer-file-name)) (not (string-match "\\.[hH]$" (buffer-file-name))))
    (message "Are you editing a header file (C/C++/Objective-C)?")))



;;;
;;; Section managed by `customize' will be appended here
;;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(leuven)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 170 :width normal :family "Monaco"))))
 '(Info-quoted ((t (:inherit default :underline t))))
 '(custom-variable-obsolete ((t (:inherit custom-variable-tag :strike-through t :weight normal))))
 '(info-menu-header ((t (:weight bold :family "Sans Serif"))))
 '(line-number ((t (:inherit (shadow default) :height 0.8)))))
