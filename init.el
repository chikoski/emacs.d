(defun kill-all-buffer ()
  (interactive)
  (let ((scratch-buff (find-buffer "*scratch*")))
    (dolist (buffer (buffer-list))
      (if (and (not (eq scratch-buff buffer))
               (not (buffer-modified-p buffer)))
          (delete-buffer buffer)))))


(when (eq window-system 'mac)
  (add-hook 'window-setup-hook
            (lambda ()
              (set-frame-parameter nil 'fullscreen 'fullboth)
              )))


(defun mac-toggle-max-window ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

;; Carbon Emacsの設定で入れられた. メニューを隠したり．
(custom-set-variables
 '(display-time-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
 )


;;Color
(if window-system (progn
   (set-background-color "Black")
   (set-foreground-color "LightGray")
   (set-cursor-color "Gray")
   (set-frame-parameter nil 'alpha 80)
   ))


;; bind C-h delete-backward-char
 (global-set-key "\C-h" 'delete-backward-char)
(put 'upcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)

;; load-path setting
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; js2-mode.el
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
