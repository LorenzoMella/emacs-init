(deftheme adaptive-tab-bar
  "Harmonizes the Tab Bar appearance with those themes that do not customize it.")

(custom-theme-set-faces
 'adaptive-tab-bar
 '(tab-bar ((t (:inherit (variable-pitch mode-line-inactive)))))
 '(tab-bar-tab ((t (:inherit (mode-line mode-line-buffer-id) :box (:line-width (1 . 1) :style released-button)))))
 '(tab-bar-tab-inactive ((t (:inherit (tab-bar))))))

(provide-theme 'adaptive-tab-bar)
