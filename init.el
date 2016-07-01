(require 'cask)
(cask-initialize)

;; pallet
(require 'pallet)
(pallet-mode t)

;; start server for emacsclient use
(server-start)

;; an intractive function to kill all visited bufferes except *scratch*
(defun kill-all-buffer ()
  (interactive)
  (let ((scratch-buff (find-buffer-visiting "*scratch*")))
    (dolist (buffer (buffer-list))
      (if (and (not (eq scratch-buff buffer))
               (not (buffer-modified-p buffer)))
          (delete-buffer buffer)))))

;; define a function to toggle full screen / window
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(set-frame-parameter nil 'fullscreen 'fullboth)

;; specify input method
(setq default-input-method "MacOSX")

;; hide menu
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-comment-face ((t (:foreground "#D9333F"))))
 '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
 '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-face ((t (:foreground "#E6B422" :weight bold))))
 '(web-mode-server-comment-face ((t (:foreground "#D9333F")))))


;;Color
(if window-system
    (progn
      (set-background-color "Black")
      (set-foreground-color "LightGray")
      (set-cursor-color "Gray")
      (set-frame-parameter nil 'alpha 80)
      ))

;; Fonts
(create-fontset-from-ascii-font
 "Monaco-18:weight=normal:slant=normal" nil "monacokakugo")
(set-fontset-font
 "fontset-monacokakugo"
 'unicode
 (font-spec :family "Hiragino Kaku Gothic ProN" :size 18)
 nil
 'append)
(add-to-list 'default-frame-alist '(font . "fontset-monacokakugo"))

;; indation & tabs
(setq-default indent-tabs-mode nil) ; use space instead of tab
(setq-default c-basic-offset 2)

;; Tab
(setq-default tab-width 2)
(setq default-tab-width 2)
(setq tab-stop-list
      '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32
          36 38 40 42 44 46 48 50 52 54 56 58 60 62
          64 66 68 70 72 74 76 78 80 82 84 86 88 90
          92 94 96 98 100 102 104 106 108 110 112 114 116 120))

;; bind C-h delete-backward-char
(global-set-key "\C-h" 'delete-backward-char)

;; upcase-regtion / downcase-region を使えるように
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; js2-mode.el & js-mode
(when (require 'js2-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'auto-mode-alist '("Gruntfile". js2-mode))
  (setq js2-cleanup-whitespace nil
        js2-mirror-mode nil
        js2-bounce-indent-flag nil)
)

;; graphviz-dot-mode.el
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))

;; markdown-mode.el
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; yaml-mode.el
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; php-mode
(autoload 'php-mode "php-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.php$'" . php-mode))
(add-hook 'php-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq c-basic-offset 2)
            (setq indent-tabs-mode nil)))

;; css-mode
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(setq css-indent-offset 2)

;; less-css-mode
(autoload 'less-css-mode "less-css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.less$'" . less-css-mode))
(add-hook 'less-css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))

;; close-all-buffers
;; from http://osdir.com/ml/emacs.windows/2006-05/msg00012.html
(require 'cl)
(defun close-all-buffers ()
  (interactive)
  (loop for buffer being the buffers
     do (kill-buffer buffer)))

;; rust-mode
(when (require 'rust-mode nil t)
  (setq auto-mode-alist
        (cons '("\\.rs$'" . rust-mode) auto-mode-alist))
  (setq rust-indent-offset 2))

;; web-mode
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (defun web-mode-indent (num)
              (interactive "nIndent: ")
              (setq web-mode-markup-indent-offset num)
              (setq web-mode-css-indent-offset num)
              (setq web-mode-style-padding num)
              (setq web-mode-code-indent-offset num)
              (setq web-mode-script-padding num)
              (setq web-mode-block-padding num)
              )
            (web-mode-indent 2)))


;; start json-mode when the file name is manifest.webapp
(require 'json-mode nil t)
(add-to-list 'auto-mode-alist '("manifest.webapp" . json-mode))
(add-to-list 'auto-mode-alist '("package.manifest" . json-mode))
(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level 2)))


;; helm
(when (require 'helm-config nil t)
  (helm-mode 1)
  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

 ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern)))))))

;; auto-complete
(when (require 'auto-complete-config nil t)
  (when (require 'ac-js2-mode nil t)
    (add-hook 'js2-mode-hook 'ac-js2-mode))
  (ac-config-default))
(setq ac-auto-show-menu nil)
(setq ac-auto-start nil)
(when (and (featurep 'auto-complete-config) (require 'ac-helm nil t))
  (global-set-key (kbd "C-:") 'ac-complete-with-helm)
  (define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm))

;; buffer autoreload
(global-auto-revert-mode 1)

;; ruby-mode
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

;; typescript
(require 'typescript)
(setq typescript-indent-offset 2)

;;; apache-mode
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . apache-mode))

;; scss-mode
(require 'scss-mode)
(setq scss-compile-at-save nil)

;; emacsclient
(unless (server-running-p)
  (server-start))

;; disable dialog box, which force Emacs to be abort
(setq use-dialog-box nil)

;(mac-auto-ascii-mode 1)

; remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default show-trailing-whitespace t)

(c-set-offset 'case-label '+)
(c-set-offset 'innamespace 0)

(add-hook 'js2-mode-hook 'turn-on-auto-fill)
(add-hook 'c++-mode-hook 'turn-on-auto-fill)
