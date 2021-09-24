;;  root-init.el --- A minimal terminal-based Emacs init configuration for root users
;;  Author: Lorenzo Mella <lorenzo.mella@hotmail.it>
;;  Copyright (C) 2021 Lorenzo Mella


;; The configuration only relies on a basic Emacs (25+) installation. No packages from external repos are used.


;; Basic QOL modifications

(column-number-mode)
(delete-selection-mode)
(customize-set-variable 'find-file-visit-truename t
			"When opening symlinks, associate the linked-file filename to the buffer")

(customize-set-variable 'help-window-select t
			"Switch focus to a help window automatically, when created")

;; Time display on the mode-line

(customize-set-variable 'display-time-day-and-date t)
(customize-set-variable 'display-time-interval 1) ; every second
(display-time-mode)

;; Better general-purpose keybindings

(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "C-x C-b") #'switch-to-buffer)
(global-set-key (kbd "C-M-;") #'comment-line)
(global-set-key (kbd "C-x k") #'kill-current-buffer)
(global-set-key [remap capitalize-word] #'capitalize-dwim)
(global-set-key [remap downcase-word] #'downcase-dwim)
(global-set-key [remap upcase-word] #'upcase-dwim)

;; Ido and Fido/iComplete narrowing

(ido-mode)
(ido-everywhere)

(if (boundp 'fido-mode)	; fido is just icomplete with ido behavior (introduced in Emacs 27?)
    (fido-mode)
  (icomplete-mode))

(customize-set-variable 'ido-decorations '("\n-> " "" "\n" ""
					   " [" "]" " [No match]"
					   " [Matched]" " [Not readable]"
					   " [Too big]" " [Confirm]"
					   " [" "]"))

(define-key ido-common-completion-map (kbd "C-n") #'ido-next-match)
(define-key ido-common-completion-map (kbd "C-p") #'ido-prev-match)

;; Dired configuration

(require 'dired-x)
(customize-set-variable 'dired-auto-revert-buffer t
			"Refresh the dired buffer whenever unburied")
(customize-set-variable 'dired-use-ls-dired
			(if (eq system-type 'gnu/linux) 'unspecified)
			"Avoids a warning")
(customize-set-variable 'dired-listing-switches
			(if (eq system-type 'gnu/linux)
			    "-lahF --group-directories-first"
			  "-lahF")
			"ls -l readability adjustments. Group directories first when using coreutils ls")
(customize-set-variable 'dired-ls-F-marks-symlinks t
			"Rename symlinks correctly when marked with '@' by ls -lF")

;; Programming modes

(dolist (mode (list #'subword-mode
		    #'show-paren-mode
		    (if (version< emacs-version "26.1")
			#'linum-mode ; display-line-numbers optionally replaces linum starting from Emacs 26.1
		      #'display-line-numbers-mode)
		    #'electric-pair-local-mode))
  (add-hook 'prog-mode-hook mode))
