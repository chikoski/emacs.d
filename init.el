(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))


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

;; Fonts
(create-fontset-from-ascii-font
 "Monaco-12:weight=normal:slant=normal" nil "monacokakugo")
(set-fontset-font "fontset-monacokakugo"
                  'unicode
                  (font-spec :family "Hiragino Kaku Gothic ProN" :size 12)
                  nil
                  'append)
(add-to-list 'default-frame-alist '(font . "fontset-monacokakugo"))


;; bind C-h delete-backward-char
 (global-set-key "\C-h" 'delete-backward-char)
(put 'upcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)

;; load-path setting
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; js2-mode.el
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; graphviz-dot-mode.el
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))

;; markdown-mode.el
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; yaml-mode.el
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; php-mode
(autoload 'php-mode "php-mode" nil t)
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))

;; close-all-buffers
;; from http://osdir.com/ml/emacs.windows/2006-05/msg00012.html
(require 'cl)
(defun close-all-buffers ()
  (interactive)
  (loop for buffer being the buffers
     do (kill-buffer buffer)))
