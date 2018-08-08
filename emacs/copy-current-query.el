;; Emacs: 現在位置のSQLをよしなに選択＋コピーする - そもさん＆せっぱさん
;; http://d.hatena.ne.jp/sonota88/20150927/1443353103

(defun my-sql:query-beg ()
  (let (beg beg-para beg-sc)
    (save-excursion
      (backward-paragraph)
      (setq beg-para
            (if (bobp)
                (point)
              (+ (point) 1))))
    (save-excursion
      (when (search-backward-regexp ";\n" nil t)
          (setq beg-sc (+ (point) 2))))
    (if beg-sc
        (max beg-para beg-sc)
      beg-para)))

(defun my-sql:query-end ()
  (let (end end-para end-sc)
    (save-excursion
      (forward-paragraph)
      (setq end-para (- (point) 1)))
    (save-excursion
      (when (search-forward-regexp ";\n" nil t)
          (setq end-sc (- (point) 2))))
    (if end-sc
        (min end-para end-sc)
      end-para)))

(defun my-sql:copy-current-query ()
  "Copy current query."
  (interactive)
  (let ((beg (my-sql:query-beg))
        (end (my-sql:query-end)))
    (kill-ring-save beg end)
    ;; (volatile-highlight beg end 0.3)
    ))

;; (global-set-key (kbd "C-M-h") 'my-sql:copy-current-query)
