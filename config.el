; refined init.el, gradually will mode all of my config here

(defun my/now ()
  "Insert current timestamp in org-mode format"
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %H:%M]")))


; TODO need to ignore # files?
(defun my/helm-files-do-rg-recursive (where)
  (let ((helm-ag-command-option "--follow"))
    (spacemacs/helm-files-do-rg where)))


(defun my/search-info ()
  (interactive)
  (my/helm-files-do-rg-recursive my/info-search-path)) ; path defined externally


(defun my/search-code ()
  (interactive)
  (my/helm-files-do-rg-recursive my/code-search-path)) ; path defined externally



; keybindings etc

(evil-global-set-key 'insert (kbd "C-t") #'my/now)


(spacemacs/set-leader-keys
  "S i" #'my/search-info
  "S c" #'my/search-code)


(global-set-key (kbd "<f1>") #'my/search-info)
(global-set-key (kbd "<f3>") #'my/search-code)
