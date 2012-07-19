(defun kill-all-buffer ()
  (interactive)
  (let ((scratch-buff (find-buffer "*scratch*")))
    (dolist (buffer (buffer-list))
      (if (and (not (eq scratch-buff buffer))
               (not (buffer-modified-p buffer)))
          (delete-buffer buffer)))))


(add-hook 'window-setup-hook
	  (lambda ()
	    (ns-toggle-fullscreen)
	    ))

;; inputメソッドの指定
(setq default-input-method "MacOSX")

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

;; graphviz-dot-mode.el
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))

;; markdown-mode.el
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
