;; ugh. need to pass targets argument...

(defun spacemacs/helm-files-do-rg (&optional dir targets)
  "Search in files with `rg'."
  (interactive)
  ;; --line-number forces line numbers (disabled by default on windows)
  ;; no --vimgrep because it adds column numbers that wgrep can't handle
  ;; see https://github.com/syl20bnr/spacemacs/pull/8065
  (let* ((root-helm-ag-base-command "rg --smart-case --no-heading --color never --line-number")
         (helm-ag-base-command (if spacemacs-helm-rg-max-column-number
                                   (concat root-helm-ag-base-command " --max-columns " (number-to-string spacemacs-helm-rg-max-column-number))
                                 root-helm-ag-base-command)))
    (helm-do-ag dir targets)))
