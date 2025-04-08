;;;  early-init.el  -*- lexical-binding:t; coding:utf-8 -*-
;;  Author: Lorenzo Mella <lorenzo.mella@hotmail.it>
;;  Copyright (C) 2021-2022 Lorenzo Mella


;; "Hack" to avoid native fullscreen on the Mac. If I customize
;; `ns-use-native-fullscreen' directly in `init.el', the first call to
;; `toggle-frame-fullscreen' will use the native fullscreen functionality,
;; creating a dedicated space. Subsequent calls work as intended. The unexpected
;; behavior is solved using `early-init.el' as done below.
(when (eq (window-system) 'ns)
  (add-hook 'window-setup-hook (lambda () (setq ns-use-native-fullscreen nil))))

(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp #'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp #'scroll-bar-mode)
  (scroll-bar-mode -1))
