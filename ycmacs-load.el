(require 'ycmacs)

(add-hook 'js-mode-hook 'ycmacs-mode)
(find-file "testfiles/test1.js")

(split-window-right)
(other-window 1)
(switch-to-buffer "*Messages*")
(other-window 1)
