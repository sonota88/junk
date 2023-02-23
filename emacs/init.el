;; --------------------------------
;; 基本

(tool-bar-mode 0) ; ツールバーを表示しない（v24）
(setq column-number-mode t) ; 桁を表示

(setq inhibit-startup-message t) ; 起動時にメッセージを出さない
(setq initial-scratch-message nil)

(fset 'yes-or-no-p 'yes-or-no-p)

;; ファイル末尾に自動で改行追加
(setq require-final-newline nil)
;; 末尾が改行でない場合ユーザに問い合わせ
(setq mode-require-final-newline 0)

(electric-indent-mode 0)

(setq visible-bell 1)

;; 1行ずつスクロールさせる
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; --------------------------------
;; キーバインド

(global-set-key (kbd "C-h") 'backward-delete-char)

;; ウィンドウ切り替え
(global-set-key (kbd "C-t") 'other-window)

;; 折り返し
(global-set-key (kbd "C-M-y") 'toggle-truncate-lines)

;; 効かない場合は SCIM の設定を確認
(global-set-key (kbd "S-SPC") 'dabbrev-expand)

;; --------------------------------

(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; --------------------------------
;; 色

(setq default-frame-alist
      (append (list
               '(foreground-color . "#c0c0c0")  ; 文字
               '(background-color . "#282828")  ; 背景
               '(cursor-color     . "#666666")) ; カーソル
              default-frame-alist))

;; http://d.hatena.ne.jp/sonota88/20110111/1294695497
(set-face-foreground 'font-lock-regexp-grouping-backslash "#666")
(set-face-foreground 'font-lock-regexp-grouping-construct "#f60")

;; ----------------

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Steepfile\\'" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.rbs\\'" . ruby-mode))

;; ----------------

(defun insert-current-datetime ()
  "現在の日付と時刻を挿入"
  (interactive)
  (insert (format-time-string "%Y-%m-%d %T" (current-time))))

(global-set-key (kbd "C-M-t") 'insert-current-datetime)

;; --------------------------------
;; transient-mark-mode + リージョン設定 + SPC or S-SPC でインデント増減

(defun my-space-indent (arg normal-proc)
  (interactive)
  (if (and transient-mark-mode mark-active)
      (save-excursion
        (let ((deactivate-mark nil))
          ;; (if (> (point) (mark))
          ;;     (exchange-point-and-mark))
          (beginning-of-line)
          (indent-rigidly (region-beginning) (region-end) arg)))
    (funcall normal-proc)))

(global-set-key (kbd "SPC")
                (lambda () (interactive)
                  (my-space-indent 1
                                   (lambda () (insert " ")))))
(global-set-key (kbd "S-SPC")
                (lambda () (interactive)
                  (my-space-indent -1
                                   (lambda () (dabbrev-expand nil)))))


;; --------------------------------
;; 括弧のハイライト

(defface my-paren-face
  '((t (:foreground "#4ac")))
  "my paren face")

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("(\\|)" . 'my-paren-face)))))


(add-hook 'ruby-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("(\\|)\\|\\[\\|\\]\\|{\\|}" . 'my-paren-face)))))

(add-hook 'js-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("(\\|)\\|\\[\\|\\]\\|{\\|}" . 'my-paren-face)))))


;; --------------------------------

(defun delete-char-or-region (dir)
  "範囲選択している場合は delete"
  (let ((n (cond
            ((eq dir :del) 1)
            ((eq dir :bs) -1)
            (t 0))))
    (if mark-active
        (delete-region (region-beginning) (region-end))
      (delete-char n))))

(global-set-key
 (kbd "<backspace>")
 (lambda () (interactive)
   (delete-char-or-region :bs)))

(global-set-key
 (kbd "<delete>")
 (lambda () (interactive)
   (delete-char-or-region :del)))


;; --------------------------------

(defun my-beginning-of-line:space-p ()
  (string= " " (string (following-char))))

(defun my-beginning-of-line:move ()
  (interactive)
  (if (bolp)
      (when (my-beginning-of-line:space-p)
        (back-to-indentation))
    (beginning-of-line)))

(global-set-key (kbd "C-a") 'my-beginning-of-line:move)


;; ----------------
;; tramp

;; 2017-07-04 helm の初回起動に 20秒くらいかかる問題への対策
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

;; ----------------
;; markdown-mode

(setq markdown-gfm-use-electric-backquote nil)

(add-hook
 'markdown-mode-hook
 (lambda ()
   (set-face-background 'markdown-code-face "#222")
   (set-face-foreground 'markdown-code-face "#ccc")
   ))

;; ----------------
;; JavaScript

(setq js-indent-level 2)

(require 'compile)

(add-hook
 'js-mode-hook
 (lambda ()
   ;; Webpack
   (add-to-list
    'compilation-error-regexp-alist ; need to (require 'compile)
    '("^\\(.+?\\): line \\([0-9]+\\), col \\([0-9]+\\), .+$"
      1 2 3))))


;; --------------------------------
;; Compile

(defface my-compilation-message-face
  '((t (:background "#000")))
  "my compilation message face")

(setq compilation-message-face 'my-compilation-message-face)


;; ----------------

(defun eval-by-ruby ()
  "eval by Ruby"
  (interactive)
  (save-excursion
    (shell-command-on-region
     (point) (mark)
     "ruby /path/to/eval-by-ruby.rb"
     nil
     t)))

(global-set-key (kbd "C-x C-r") 'eval-by-ruby)

; --------------------------------
; web-mode

;; web-modeでペースト時に自動でインデントされるのをやめる - $shibayu36->blog;
;; https://blog.shibayu36.org/entry/2016/03/17/183209

(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-enable-auto-indentation nil)))

;; --------------------------------
;; http://www.emacswiki.org/emacs/RotateWordCapitalization

(require 'fdlcap)
(global-set-key (kbd "M-l") 'fdlcap-change-case-current-word)

;; --------------------------------
;; highlight-symbol

(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 1.0)
(set-face-foreground 'highlight-symbol-face "#f0f")
(set-face-background 'highlight-symbol-face "#400")
(global-set-key (kbd "C-.") 'highlight-symbol-prev)
(global-set-key (kbd "C-,") 'highlight-symbol-next)
(global-set-key (kbd "C-M-r") 'highlight-symbol-query-replace)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;; --------------------------------
;; undo-tree

(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C-\\") 'undo-tree-redo)
