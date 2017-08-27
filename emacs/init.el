(setq sh-basic-offset 2)
(setq sh-indentation 2)

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
