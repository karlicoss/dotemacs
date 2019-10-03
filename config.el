; refined init.el, gradually will mode all of my config here


; (cl-defun org-files-in (path &key (archive nil))
;   (let ((regex (if archive "\\.\\(org\\|org\_archive\\)$" "\\.org$"))
;         (is-lockfile (lambda (s) (string-match-p "/\\.#.*" s)))
;     (remove-if is-lockfile (directory-files-recursively path regex))

(cl-defun org-files-in (path &key (archive nil) (follow nil))
  "This is a bit horrible, but none of standard functions or popular libs support following symlink :("
  (let* ((patterns (if archive "-g '*.org' -g '*.org_archive'" "-g '*.org'"))
         (follows (if follow "--follow" ""))
         (rg-command (format "rg --files %s -0 %s %s" follows patterns path)))
    (-map #'file-truename (s-split "\0" (shell-command-to-string rg-command) t))))

; helper functions


(defun my/now ()
  "Insert current timestamp in org-mode format"
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a %H:%M]")))


;;; searching for things
; TODO need to ignore # files?
(defun --my/helm-files-do-rg-follow (where)
  (let ((helm-ag-command-option "--follow"))
    (spacemacs/helm-files-do-rg where)))


(defun my/search-notes ()
  (interactive)
  (--my/helm-files-do-rg-follow my/notes-targets))


(defun my/search-code ()
  (interactive)
  (--my/helm-files-do-rg-follow my/code-targets))


(defun my/prepare-swoop ()
  "Swoop only works in open buffers apparently. So this opens all notes in buffers..."
  (interactive)
  (let ((files (org-files-in my/notes-targets :archive t :follow t))
        ; adjust large file size so spacemacs doesn't prompt you for opening it in fundamental mode
        (dotspacemacs-large-file-size (* 50 1024 1024)))
    (-map #'find-file-read-only files)))

;;;


;;; org-drill
(defun --my/drill-with-tag (tag)
  (require 'org-drill)
  (let ((org-drill-question-tag tag))
    (org-drill (org-files-in my/drill-targets :follow t))))

(defun my/habits ()
  (interactive)
  (--my/drill-with-tag "habit"))


(defun my/drill ()
  (interactive)
  (--my/drill-with-tag "drill"))

;;;


;;; org-agenda

(defun get-org-agenda-files ()
  (org-files-in my/agenda-targets :follow t))

; TODO hotkey to toggle private/non private?
(defun my/agenda (&optional arg)
  (interactive "P")
  (require 'org-agenda)
  (let ((org-agenda-tag-filter-preset '("-prv")))
    (org-agenda arg "a")))

(defun my/switch-to-agenda ()
  (interactive)
  (if (get-buffer "*Org Agenda*") (switch-to-buffer "*Org Agenda*") (my/agenda)))

;;;


;;; org-refile

(defun get-org-refile-targets ()
  (org-files-in my/refile-targets :follow t))

;;;


; keybindings etc

(evil-global-set-key 'insert (kbd "C-t") #'my/now)


(spacemacs/set-leader-keys
  "A"   #'my/switch-to-agenda
  "S i" #'my/search-notes
  "S c" #'my/search-code
  "S s" #'helm-multi-swoop-all)



(global-set-key (kbd "<f1>") #'my/search-notes)
(global-set-key (kbd "<f3>") #'my/search-code)
