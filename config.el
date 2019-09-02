; refined init.el, gradually will mode all of my config here

(defun my/now ()
  "Insert current timestamp in org-mode format"
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %H:%M]")))






(evil-global-set-key 'insert (kbd "C-t") 'my/now)
