; refined init.el, gradually will mode all of my config here

;;; random helpers

; TODO should this be macro??
(cl-defun with-error-on-user-prompt (body)
  "Useful when we want to avoid prompts"
  (interactive)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (arg) (error "IGNORING PROMPT %s" arg))))
    (eval body)))
;;;


;;; searching for things
(cl-defun my/files-in (path &key (exts nil) (follow nil))
  "This is a bit horrible, but none of standard Elisp functions or popular libs support following symlink :("
  (let* ((patterns (s-join " " (-map (lambda (i) (format "-g '*.%s'" i)) exts)))
         (follows (if follow "--follow" ""))
         (rg-command (format "rg --files %s -0 %s %s" follows patterns path)))
    (-map #'file-truename (s-split "\0" (shell-command-to-string rg-command) t))))

(cl-defun my/org-files-in (path &key (archive nil) (follow nil))
  (my/files-in path :exts (if archive '("org" "org_archive") '("org")) :follow follow))


; TODO need to ignore # files?
(defun --my/helm-files-do-rg-follow (where)
  (let ((helm-ag-command-option "--follow"))
    (spacemacs/helm-files-do-rg where)))


(defun --my/find-file-defensive (f)
  "Convenient to ignore lock files, generally race conditions and userp prompts"
  "returns filename if successful, othewise nil"
  (ignore-errors (with-error-on-user-prompt `(find-file-read-only f)) f))


;; TODO FIXME fucking hell, elisp doesn't seem to have anything similar to e.g. check_call in python
;; also no simple way to pass set -eu -o pipefail
;; so, if find or xargs fails, you'd get with garbage in the variable

;; really wish there was some sort of bridge for configuring emacs on other programming languages
;; there is zero benefit of using Elisp for most of typical emacs configs; only obstacles.
;; can't say about other lisps, but very likely it's not very beneficial either

(defun --my/git-repos-refresh ()
  (let ((search-git-repos-cmd (s-concat
                               "find " (format "'%s'" my/git-repos-search-root) " -iname '.git' -printf '%h\\0' "
                               "| xargs -0 readlink -f "
                               "| sort")))
    (progn
      (message "refreshing git repos...")
      (defconst *--my/git-repos*
        (s-split "\n" (shell-command-to-string search-git-repos-cmd) t))))) ; t for omit-nulls


(defun my/code-targets ()
  "Collect repositories across the filesystem and bootstrap the timer to update them"
  ; TODO there mustbe some generic caching mechanism for that in elisp?
  (let ((refresh-interval-seconds (* 60 5)))
    (progn
      (unless (boundp '*--my/git-repos*)
        (--my/git-repos-refresh)
        (run-with-idle-timer refresh-interval-seconds t '--my/git-repos-refresh))
      *--my/git-repos*)))


(defun my/search ()
  (interactive)
  (--my/helm-files-do-rg-follow my/search-targets))


(defun my/search-code ()
  (interactive)
  (spacemacs/helm-files-do-rg "/" (my/code-targets)))


; TODO perhaps split window and display some sort of progress?
(defun my/prepare-swoop ()
  "Swoop only works in open buffers apparently. So this opens all files in buffers..."
  (let* ((time (current-time))
         (files (my/files-in my/search-targets
                             :follow t
                             :exts '(
                                     "org" "org_archive"
                                     "txt"
                                     "page" "md" "markdown"))) ; older Markdown/gitit notes
                                        ; disable local variables, mainly so nothing prompts while opening org files with babel
         (enable-local-variables nil)
         (enable-local-eval nil)
                                        ; adjust large file size so spacemacs doesn't prompt you for opening it in fundamental mode
         (dotspacemacs-large-file-size (* 50 1024 1024))
         (with-errors (-remove '--my/find-file-defensive files))
         (buf-sizes (-sort (lambda (a b) (> (car a) (car b)))
                           (-map (lambda (b) (with-current-buffer b `(,(buffer-size) ,(buffer-file-name))))
                                 (buffer-list)))))
    ; TODO eh, maybe I need to refactor it so it's clear what's important and what isn't...
    (setq swoop-stats-buf (generate-new-buffer "*swoop-stats*"))
    (with-current-buffer swoop-stats-buf
      (insert (format "prepare-swoop took %.1f seconds!\nErrors while loading:\n%s\nBuffers opened:\n%s"
                      (float-time (time-since time))
                      (s-join "\n" with-errors)
                      (s-join "\n" (-map (lambda (l) (format "%d %s" (car l) (cdr l))) buf-sizes))))
      (switch-to-buffer swoop-stats-buf)
      (beginning-of-buffer))))

;;;


;;; org-drill
(defun --my/drill-with-tag (tag)
  (require 'org-drill)
  (let ((org-drill-question-tag tag))
    (org-drill (my/org-files-in my/drill-targets :follow t))))

(defun my/habits ()
  (interactive)
  (--my/drill-with-tag "habit"))


(defun my/drill ()
  (interactive)
  (--my/drill-with-tag "drill"))

;;;


;;; org-agenda

(defun get-org-agenda-files ()
  (my/org-files-in my/agenda-targets :follow t))

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
  (my/org-files-in my/refile-targets :follow t))

;;;


;;; misc stuff
(defun my/now ()
  "Insert current timestamp in org-mode format"
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a %H:%M]")))
;;;


;;; keybindings etc

(evil-global-set-key 'insert (kbd "C-t") #'my/now)


(spacemacs/set-leader-keys
  "A"   #'my/switch-to-agenda
  "S s" #'my/search
  "S c" #'my/search-code
  "S w" #'helm-multi-swoop-all

  ; TODO link to my post?
  "q q" #'kill-emacs)



(global-set-key (kbd "<f1>") #'my/search)
(global-set-key (kbd "<f3>") #'my/search-code)
;;;
