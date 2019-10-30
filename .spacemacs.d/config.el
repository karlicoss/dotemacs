; refined init.el, gradually will mode all of my config here
; loaded in dotspacemacs/user-config

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



(with-eval-after-load 'helm
  ;; patch spacemacs/helm-files-do-rg to support extra targets argument used in --my/helm-files-do-rg
  (load-file "~/dotfiles-emacs/patch-helm.el"))


; TODO need to ignore # files?
(cl-defun --my/helm-files-do-rg (dir &key (targets nil) (rg-opts nil))
  (require 'helm-ag)
  (let ((helm-ag-command-option (s-join " " rg-opts)))
    ;; NOTE: spacemacs/helm-files-do-rg is patched to support second argument with multiple directories
    ;; (see patch-helm.el)
    (spacemacs/helm-files-do-rg dir targets)))


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
  (let ((search-git-repos-cmd (s-join " "
                                      `(
                                        "fdfind"
                                        "--follow" ; follow symlink
                                        "--hidden" "--type d" "'.git$'" ; match git dirs
                                        ,(format "'%s'" my/git-repos-search-root)
                                        "-x" "readlink" "-f" "'{//}'")))) ; resolve symlinks
    (progn
      (message "refreshing git repos...")
      (defconst *--my/git-repos*
        (-distinct (s-split "\n" ; remove duplicates due to symlinking
                            (shell-command-to-string search-git-repos-cmd) t)))))) ; t for omit-nulls


(defun my/code-targets ()
  "Collect repositories across the filesystem and bootstrap the timer to update them"
  ; TODO there mustbe some generic caching mechanism for that in elisp?
  (let ((refresh-interval-seconds (* 60 5)))
    (progn
      (unless (boundp '*--my/git-repos*)
        (--my/git-repos-refresh)
        (run-with-idle-timer refresh-interval-seconds t '--my/git-repos-refresh))
      *--my/git-repos*)))



;; I only want helm follow when I run helm-ag against my notes,
;; but not all the time, in particular when I'm running my/search-code because it
;; triggers loading LSP etc
;; Problem is helm-follow-mode seems to be handled on per-source basis
;; and there is some logic that tries to persist it in customize-variables
;; for future emacs sessions.
;; helm-ag on one hand seems to use since source (helm-ag-source) for all searches
;; on the orther hand it does some sort of dynamic renaming and messing with source names
;; (e.g. search by "helm-attrset 'name")
;; As a result it's very unclear what's actually happening even after few hours of debugging.
;; also see https://github.com/emacs-helm/helm/issues/2006,

;; other things I tried (apart from completely random desperate attempts)
;; - setting
;;   (setq helm-follow-mode-persistent t)
;;   (setq helm-source-names-using-follow `(,(helm-ag--helm-header my/search-targets))))
;; - using different source similar to helm-ag-source, but with :follow t -- doesn't work :shrug:

;; in the end I ended up hacking hooks to enable follow mode for specific helm call
;; and disabling on closing the buffer. Can't say I like it at all and looks sort of flaky.


(defun --my/helm-follow-mode-set (arg)
  "Ugh fucking hell. Need this because helm-follow-mode works as a toggle :eyeroll:"
  (unless (eq (helm-follow-mode-p) arg)
    (helm-follow-mode)))


(defun --my/one-off-helm-follow-mode ()
  (defun --my/enable-helm-follow-mode ()
    (--my/helm-follow-mode-set t))

  (defun --my/disable-helm-follow-mode ()
    (--my/helm-follow-mode-set nil)
    (remove-hook 'helm-move-selection-before-hook '--my/enable-helm-follow-mode)
    (remove-hook 'helm-cleanup-hook '--my/disable-helm-follow-mode))

  ;; ugh, helm-move-selection-before-hook doesn seem like the right one frankly
  ;; but I haven't found anything better, e.g. helm-after-initialize-hook seems too early
  ;; helm-after-update-hook kinda worked, but immediately dropped after presenting results
  ;; as helm complains at 'Not enough candidates' :(
  (add-hook 'helm-move-selection-before-hook '--my/enable-helm-follow-mode)
  (add-hook 'helm-cleanup-hook '--my/disable-helm-follow-mode))


(defun my/search ()
  (interactive)
  (--my/one-off-helm-follow-mode)
  (--my/helm-files-do-rg my/search-targets
                         :rg-opts '("--follow")))

(defun my/search-code ()
  (interactive)
  (--my/helm-files-do-rg "/"
                         :targets (my/code-targets)
                         :rg-opts '("-T" "txt" "-T" "md" "-T" "html" "-T" "org" "-g" "!*.org_archive")))


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



(with-eval-after-load 'helm-ag
  ;; see helm-ag--construct-command. Not configurable otherwise ATM
  (defun helm-ag--construct-ignore-option (pattern)
    (concat "--glob=!" pattern)))
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

;;; misc org stuff

(with-eval-after-load 'org
  (load-file "~/dotfiles-emacs/babel-mypy.el"))

;;;

;;; misc stuff
(defun my/now ()
  "Insert current timestamp in org-mode format"
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a %H:%M]")))
;;;



;;; keybindings etc

(defun --my/org-agenda-postpone (days)
  (interactive)
  (org-agenda-schedule nil (format "+%dd" days)))

;; TODO not sure what's the difference between org-defkey and other methods of binding...
;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-02/msg00260.html

(with-eval-after-load 'org-agenda
  (loop for days from 0 to 9
        do (org-defkey
             org-agenda-mode-map
             (format "%d" days)
             `(lambda ()
                ,(format "Schedule %d days later" days)
                (interactive)
                (--my/org-agenda-postpone ,days)))))


(evil-global-set-key 'insert (kbd "C-t") #'my/now)


(spacemacs/set-leader-keys
  "A"   #'my/switch-to-agenda

  "S s" #'my/search
  "S c" #'my/search-code

  ;; TODO shit! configure other engines as well
  ;; lets you enger an interactive query
  "s G" #'engine/search-google

  ;; TODO extract search-hotkeys so it's easy to extract for the post?
  "p P" #'helm-projectile-find-file-in-known-projects

  ;; TODO don't need it anymore?
  "S w" #'helm-multi-swoop-all

  ; TODO link to my post?
  "q q" #'kill-emacs)



(global-set-key (kbd "<f1>") #'my/search)
(global-set-key (kbd "<f3>") #'my/search-code)
;;;
