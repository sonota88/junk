(defvar my-js-compile:script "compile_js.sh")

(defun my-js-compile:compile ()
  (let ((dir (locate-dominating-file default-directory
                                     my-js-compile:script)))
    (when dir
      (compile (concat dir my-js-compile:script)))))

;; https://www.emacswiki.org/emacs/ModeCompile
(defun my-js-compile:compilation-exit-autoclose (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  (cons msg code))

(add-hook
 'js-mode-hook
 (lambda ()
   (add-hook 'after-save-hook 'my-js-compile:compile
             nil ; append
             t ; local
             )
   ;; (add-hook 'compilation-finish-functions
   ;;           (lambda (buf str)
   ;;             (message "---- compile done (%s) (%s)" buf str)))
   (setq compilation-exit-message-function
         'my-js-compile:compilation-exit-autoclose)
   ))


;; Don't display *compilation* buffer in Emacs until the process exits with error or warning - Stack Overflow
;; https://stackoverflow.com/questions/17659212/dont-display-compilation-buffer-in-emacs-until-the-process-exits-with-error-o
