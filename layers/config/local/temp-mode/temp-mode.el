;; temp-mode.el
;; Temporary minor mode
;; Main use is to enable it only in specific buffers to achieve the goal of
;; buffer-specific keymaps

(defvar temp-mode-map (make-sparse-keymap)
  "Keymap while temp-mode is active.")

;;;###autoload
(define-minor-mode temp-mode
  "A temporary minor mode to be activated only specific to a buffer."
  nil
  :lighter " Temp"
  temp-mode-map)

(provide 'temp-mode)

;; Source: https://emacs.stackexchange.com/questions/519/key-bindings-specific-to-a-buffer
;; Usage:
;; eval: (temp-mode 1)
;; eval: (define-key temp-mode-map (kbd "<f10>") 'function-ONE)
