(deftheme adaptive-tab-bar
  "Harmonizes the Tab Bar appearance with those themes that do not customize it.")

(custom-theme-set-faces
 'adaptive-tab-bar
 '(tab-bar ((t (:inherit (mode-line-inactive variable-pitch)))))
 '(tab-bar-tab ((t (:inherit mode-line :box (:line-width (1 . 1) :style released-button)))))
 '(tab-bar-tab-inactive ((t (:inherit (tab-bar shadow)))))
 ;; The group colors are left as is, but they rely only on `shadow' and the
 ;; modified tab-bar faces
 '(tab-bar-tab-group-current ((t (:inherit tab-bar-tab :box nil :weight bold))))
 '(tab-bar-tab-group-inactive ((t (:inherit (shadow tab-bar-tab-inactive)))))
 '(tab-bar-tab-ungrouped ((t (:inherit (shadow tab-bar-tab-inactive))))))

(provide-theme 'adaptive-tab-bar)
