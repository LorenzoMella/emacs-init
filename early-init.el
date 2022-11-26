;;; early-init.el --- Experimental Early Init configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Lorenzo Mella

;; Author: Lorenzo Mella <lorenzo.mella@hotmail.it>

(when (eq (window-system) 'ns)
  (setq ns-use-native-fullscreen nil))
