;; TODO wtf?? ask why it can't be overridden?? it works..
(after! org
  (defun org-link-set-parameters (type &rest parameters)
    "Set link TYPE properties to PARAMETERS.
PARAMETERS should be keyword value pairs.  See
`org-link-parameters' for supported keys."
    (let ((data (assoc type org-link-parameters)))
      (if data (setcdr data (org-combine-plists (cdr data) parameters))
        (push (cons type parameters) org-link-parameters)
        (org-link-make-regexps)
        (when (featurep 'org-element) (org-element-update-syntax)))))

  )

(defun my/org-less-colors ()
  (interactive)
  (my/light)
  (setq browse-url-browser-function 'browse-url-chrome)
  ;; TODO meh also think about spelchecking etc?
  ;; do this to support inline http links (without any [])
  (set-face-attribute 'link nil            :foreground "#be5504" :weight 'bold :underline t)
  (org-link-set-parameters "fuzzy" :face '(:foreground "blue"    :weight "bold"))
  (loop for level from 1 to 8
        ;; TODO why is doom complaining at level and saying 'reference to a free variable'??
        do (set-face-attribute (intern-soft (format "outline-%d" level)) nil
                               :weight 'normal
                               :foreground "black")))
