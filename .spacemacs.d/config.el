; refined init.el, gradually will mode all of my config here
; loaded in dotspacemacs/user-config

;;; random helpers

; TODO should this be macro??
(cl-defun with-error-on-user-prompt (body)
  "Suppress user prompt and throw error instead. Useful when we really want to avoid prompts, e.g. in non-interactive functions"
  (interactive)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (arg) (error "IGNORING PROMPT %s" arg))))
    (eval body)))
;;;


; omg why is elisp so shit...
(defun my/get-output (cmd)
  "shell-command-to-string mixes stderr and stdout, so we can't rely on it for getting filenames etc..
   It also ignores exit code.
   (see https://github.com/emacs-mirror/emacs/blob/b7d4c5d1d1b55fea8382663f18263e2000678be5/lisp/simple.el#L3569-L3573)

   This function ignores stderr for now since I haven't figured out how to redirect it to *Messages* buffer :("
  (with-output-to-string
    (with-temp-buffer
      (shell-command
       cmd
       standard-output
       (current-buffer)))))


;;; searching for things
(cl-defun my/files-in (path &key (exts nil) (follow nil))
  "Search for files with certail extensions and potentially following symlinks.
   None of standard Elisp functions or popular libs support following symlink :(
   In addition, rg is ridiculously fast."
  (assert (stringp path)) ;; TODO surely there is a nicer way?? how to define it next to path?
  (let* ((patterns (s-join " " (-map (lambda (i) (format "-e %s" i)) exts)))
         (follows (if follow "--follow" ""))
         (rg-command (format
	              ;; TODO ugh. quoting third %s broke agenda for some reason (fd complained it wasn't a directory??)
                      "fdfind . %s %s %s -x readlink -f" ; ugh, --zero isn't supported on alpine (cloudmacs)
                      follows
                      patterns
                      path))
         (filenames (s-split "\n" (shell-command-to-string rg-command) t)))
    (-map #'file-truename filenames)))

(cl-defun my/org-files-in (path &key (archive nil) (follow nil))
  (my/files-in path :exts (if archive '("org" "org_archive") '("org")) :follow follow))



(with-eval-after-load 'helm
  ;; patch spacemacs/helm-files-do-rg to support extra targets argument used in --my/helm-files-do-rg
  (load-file "~/dotfiles-emacs/patch-helm.el"))


;; TODO rename from helm-files-do-rg?
;; TODO FIXME follow mode
(cl-defun --my/helm-files-do-rg (dir &key (targets nil) (rg-opts nil))
  "Helper function to aid with passing extra arguments to ripgrep"
  (require 'helm-rg)
  ;; TODO need to ignore # files?
  (let ((helm-ag-command-option (s-join " " rg-opts))
        (helm-rg--extra-args    (s-join " " rg-opts))
        (helm-rg-default-directory dir))
    ;; NOTE: spacemacs/helm-files-do-rg is patched to support second argument with multiple directories
    ;; (see patch-helm.el)
    (helm-rg "")))
;; TODO FIXME I guess need interactive??
;; TODO FIXME find-code


(defun --my/find-file-defensive (f)
  "Open file, ignoring lock files, various IO race conditions and user prompts.
   Returns filename if successful, othewise nil"
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
                                        ;; match git dirs, excluding bare repositories (they don't have index)
                                        "--hidden" "--full-path" "--type f" "'.git/index$'"
                                        ,(format "'%s'" my/git-repos-search-root)
                                        "-x" "readlink" "-f" "'{//}'")))) ; resolve symlinks and chop off 'index'
    (progn
      (message "refreshing git repos...")
      (defconst *--my/git-repos*
        (-distinct
         (-map (lambda (x) (s-chop-suffix "/.git" x))
               (s-split "\n" ; remove duplicates due to symlinking
                        (shell-command-to-string search-git-repos-cmd) t))))))) ; t for omit-nulls


(defun my/code-targets ()
  "Collects repositories across the filesystem and bootstraps the timer to update them"
  ; TODO there mustbe some generic caching mechanism for that in elisp?
  (let ((refresh-interval-seconds (* 60 5)))
    (progn
      (unless (boundp '*--my/git-repos*)
        (--my/git-repos-refresh)
        (run-with-idle-timer refresh-interval-seconds t '--my/git-repos-refresh))
      *--my/git-repos*)))



(defun --my/one-off-helm-follow-mode ()
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


(with-eval-after-load 'helm-ag
  ;; see helm-ag--construct-command. Not configurable otherwise ATM
  (defun helm-ag--construct-ignore-option (pattern)
    (concat "--glob=!" pattern)))
;;;


;;; org-drill

;; TODO need to fix resume to remember latest tag? not sure why it's broken..
(with-eval-after-load 'org-drill
  ;; load patch that makes org-drill detect and present empty cards (without body)
  (load "~/dotfiles-emacs/patch-org-drill.el")
  (add-to-list
   'org-drill-card-type-alist
   '(nil org-drill-present-simple-card nil t))

  ;; hack to exclude items that I'm done with (i.e. marked as DONE/CANCEL)
  (defun org-drill-entry-p (&optional marker)
    (interactive)
    (save-excursion
      (when marker
        (org-drill-goto-entry marker))
      (and
       (member org-drill-question-tag (org-get-tags nil t))
       ;; org-drill offers quick way of editing tags, so I find it useful
       (not (member "oldhabit" (org-get-tags nil t)))
       ;; TODO use org-done-keywords or something?
       (not (member (org-get-todo-state) '("DONE" "CANCEL")))))))


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

;; TODO FIXME need separate resumes?

;;;


;;; org-agenda

;; TODO my/ prefix?
(defun my/org-agenda-files-get ()
  (my/org-files-in my/agenda-targets :follow t))


(defun my/org-agenda-files-refresh ()
  (interactive)
  (setq org-agenda-files (my/org-agenda-files-get)))

(after! org-agenda
  (my/org-agenda-files-refresh))


;; TODO private groups (with fallback)?
(setq org-super-agenda-groups
      '( ;; unmatched order is 99
        (:name "Important"
               :tag "important"
               :priority "A"
               :order -50)
        (:name "Shopping"
               :tag ("shopping" "buy")
               :order -20)
        (:name "Drill"
               :tag ("drill" "habit")
               :order 999))) ;; sink drill all the way down


;; TODO hotkey to toggle private/non private?
(defun my/agenda (&optional arg)
  (interactive "P")
  (require 'org-agenda) ;; TODO require super agenda??
  (let ((org-agenda-tag-filter-preset '("-prv"))
        (org-agenda-window-setup 'only-window)) ;; TODO ??
    (org-super-agenda-mode) ;; only use org-super-agenda for global agenda
    (org-agenda arg "a")))

(defun my/switch-to-agenda ()
  "launch agenda unless it's already running"
  (interactive)
  (if (get-buffer "*Org Agenda*") (switch-to-buffer "*Org Agenda*") (my/agenda)))

;;;


;;; org-refile

(defun --my/org-refile-targets-refresh ()
  (interactive)

  (defun --my/get-org-refile-targets ()
    (my/org-files-in my/refile-targets :follow t))

  ;; todo make it async and refresh on idle?
  (defun --my/get-opened-org-files ()
    (-non-nil (-distinct (-map #'buffer-file-name (org-buffer-list)))))

  ;; I don't use cache because cache is only useful when you have to traverse filesystem to search for Org files
  ;; without cache, it's much easier to discover new refile targets
  (setq org-refile-use-cache nil)
  ;; I cache files contributing to refile here so I'm fine without refile cache.
  (setq --my/org-refile-targets (--my/get-org-refile-targets))

  (setq org-refile-targets
        '((nil                       :maxlevel . 1)
          (--my/org-refile-targets   :tag . "refile")
          (--my/get-opened-org-files :tag . "refile"))))


(with-eval-after-load 'org
  ;; TODO eh
  ;; TODO call on timer?
  (--my/org-refile-targets-refresh)

  ;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
  (setq org-refile-use-outline-path 'buffer-name) ; otherwise you can't create a top level heading
  (setq org-outline-path-complete-in-steps nil) ; otherwise you ONLY can create a top level heading!

  ;; disambiguate between buffers with same names
  ;; https://emacs.stackexchange.com/a/37610/19521
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-strip-common-suffix nil))

;;;

;;; org styling
(with-eval-after-load 'org
  (dolist (tag '("pr" "prv" "private"))
    (add-to-list 'org-tag-faces
                 `(,tag . (:foreground "red" :weight bold)))))

;;;

;;; misc org stuff

(cl-defun my/org-ql-search (query &rest args)
  (interactive)
  (require 'org-ql-search)
  (apply #'org-ql-search (my/org-agenda-files-get) query args))


(cl-defun my/org-ql-search-tags (tags)
  (interactive (list (read-minibuffer "Tags:")))
  (message "%s" tags)
  (my/org-ql-search (format "tags:%s" tags) :sort 'priority))


(with-eval-after-load 'org
  (load "~/dotfiles-emacs/org.el")
  ;; TODO ugh. I'm really not sure how should I organize my config...

  ;; TODO contribute to doom?
  ;; wonder if we can make it equal to python-shell-interpreter somehow?
  (setq org-babel-python-command "python3")

  ;; TODO not sure with doom's after! keyword, when it's the right time to do this really...
  (setq org-todo-keywords '((sequence
                             "TODO(t)"
                             ;; TODO not sure with doom's after! keyword, when it's the right time to do this really...
                             "START(!)" ;; TODO deprecate START
                             "STRT(s!)"
                             "WAIT(w@)"
                             "|"
                             "DONE(d!)"
                             "CANCEL(c@)"
                             "SKIP(k!)"))

        ;; override Doom's default
        org-enforce-todo-dependencies nil

        org-log-into-drawer t ;; log todo state changes
        org-log-states-order-reversed t) ;; default, but whatever

  (setq org-priority-lowest  ?E
        org-priority-default ?C)

  ;; eh, I'm not really using it and it breaks refiling for me due to
  ;; Debugger entered--Lisp error: (wrong-type-argument hash-table-p nil)
  ;; puthash(#("02d1370c-b581-476c-a6e4-d34dc563a9d3" 0 36 (face org-property-value fontified t)) "notes/emacs.org" nil)
  ;; TODO investigate later
  (setq org-id-track-globally nil)

  ;; TODO merge with private
  (setq org-capture-templates
        '(("t"
           "todo"
           entry
           (file --my/org-capture-file)
           "* TODO %? %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

          ("n"
           "note"
           entry
           (file --my/org-capture-file)
           "* %? %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

          ("l"
           "log entry"
           entry
           (file --my/org-capture-log-file)
           "* %U %? %^g\n")

          ("w"
           "workout"
           entry
           (file --my/org-capture-workouts-file)
           "* %U %? :wlog:\n")))


  ;; TODO hmm, is there a way to load lazily?
  (require 'org-sync)
  (require 'org-sync-github)

  (load "~/dotfiles-emacs/babel-mypy.el"))

;;;

;;; org-agenda

(with-eval-after-load 'org-agenda
  (setq-default ;; dunno why setq-default, but Doom does it...

   ;; by default agenda treats this:
   ;; * TODO [2019-12-21 Sat 13:17] some task
   ;; SCHEDULED: <2020-02-23 Sun>
   ;; as scheduled at 13:17 on 2020-02-23
   ;; this is documented in org-agenda, so it's a proper way of fixing that behaviour
   org-agenda-search-headline-for-time nil

   ;; makes it considerably quicker...
   org-agenda-span 3

   ;; start on 'today' (default in doom)
   org-agenda-start-on-weekday nil

   ;; override Doom's default
   org-agenda-start-day nil


   ;; align todo keywords in the agenda view
   ;; (6 is the longest todo keyword I use)
   org-agenda-todo-keyword-format "%-6s"

   org-agenda-scheduled-leaders '("S       "
                                  "S %-2d ago")
   org-agenda-deadline-leaders  '("D       "
                                  "D in %-3d"
                                  "D %-2d ago"))

  ;; work around issues with evil-org and super agenda headings
  ;; https://github.com/alphapapa/org-super-agenda/issues/112#issuecomment-548224512
  (setq org-super-agenda-header-map nil)


  (add-to-list 'org-agenda-prefix-format
               '(agenda  . "%i%-3:(--my/org-agenda-extras) %-12:c%?-12t% s "))

  (add-to-list 'org-agenda-prefix-format
               '(tags    . "%i%-1:(--my/org-tags-extras) %-12:c")))


;;;

;;; org-drill

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-drill))


(with-eval-after-load 'org-drill
  (setq org-drill-learn-fraction 0.3)
  (setq org-drill-pronounce-command nil)) ; disable creepy pronouncing

;;;

;;; misc stuff
(defun my/now ()
  "Insert current timestamp in org-mode format"
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a %H:%M]")))


(defun my/light ()
  (interactive)
  (load-theme 'doom-one-light t))


(defun my/dark ()
  (interactive)
  (load-theme 'doom-one t))


(with-eval-after-load 'dired
  ;; show ISO time
  (setq dired-listing-switches (concat dired-listing-switches " --full-time")))

;; otherwise it's just 'emacs'
(setq frame-title-format "emacs: %b %f")

;;;



(with-eval-after-load 'python
  (load-file "~/dotfiles-emacs/lang/python.el"))


(load-file "~/dotfiles-emacs/blog.el")

(with-eval-after-load 'graphviz-dot-mode
  (setq graphviz-dot-preview-extension "svg"))


;;; keybindings etc

(defun my/org-agenda-unschedule ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-agenda-schedule)))


(defun --my/org-agenda-postpone (days)
  (interactive)
  (org-agenda-schedule nil (format "+%dd" days)))

;; TODO not sure what's the difference between org-defkey and other methods of binding...
;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-02/msg00260.html


;; TODO ugh. fucking hell, that doesn't work
;; why is this so hard to unflatten a list??
;; (after! evil-org-agend
;;   (apply #'evil-define-key
;;          'motion
;;          evil-org-agenda-mode-map
;;          (loop for days from 0 to 9
;;                collect
;;                (list (format "%d" days)
;;                      `(lambda ()
;;                         ,(format "Schedule %d days later" days)
;;                         (interactive)
;;                         (--my/org-agenda-postpone ,days)))


;; TODO ugh. why didn't top level loop and (map! :after org-agenda) work??
;; some error about 'days' undefined..
(after! org-agenda
  (loop for days from 0 to 9
        do (map! :map org-agenda-mode-map
                 :m (format "%d" days)
                 `(lambda ()
                    ,(format "Schedule %d days later" days)
                    (interactive)
                    (--my/org-agenda-postpone ,days)))))


(with-eval-after-load 'evil
  (evil-global-set-key 'insert (kbd "C-t") #'my/now))

;; colors borrowed from doom
;; TODO ugh. how to reuse doom colors without hardcoding?
(defvar --my/org-priority-map
  '(("0" ""        'remove)
    ("1" "#ff6c6b" ?A)   ;; red ;; TODO also make bold?
    ("2" "#da8548" ?B)   ;; organge
    ("3" "#ecbe7b" ?C)   ;; yellow
    ("4" "#51afef" ?D)   ;; blue
    ("5" "#2257ao" ?E))) ;; dark-blue

(setq --my/grey "#65696e")

(after! org
  ;; TODO ugh. need to inject it into Doom theme somehow
  ;; otherwise it's lost on theme switching

  ;; apparently org-level-N is org-mode specific whereas outline-N is something lower level
  (loop for level from 1 to 7
        ;; TODO why is doom complaining at level and saying 'reference to a free variable'??
        do (set-face-attribute (intern-soft (format "outline-%d" level)) nil
                               :foreground nil))

  ;; TODO no clue why that doesn't work. it just says "invalid face: outline-1"
  ;; (set-face-attribute `,(make-symbol "outline-1") nil :foreground nil)

  ;; TODO not sure what's the precise difference between link and org-link
  (set-face-attribute 'link nil
                      ;; TODO mm. it's a bit sad wave is too spammy, dashed would be perfect
                      ;; :underline '(:color "red" :style wave)
                      :underline nil
                      :bold t
                      :foreground "royal blue")

  ;; default doom is yellow, so too bright
  (set-face-attribute 'org-date nil
                      :foreground --my/grey)

  ;; kinda violet color. default Doom is grey and very pale
  (set-face-attribute 'org-tag nil
                      :foreground "#c56ec3"
                      :weight 'bold)

  (setq org-priority-faces
        (loop for (_ col sym) in --my/org-priority-map
              collect
              `(,sym . ,col)))

  (loop for (key col sym) in --my/org-priority-map do
        (map! :map org-mode-map
              :localleader
              key `(lambda () (interactive) (org-priority ,sym))))


  (map! :map org-mode-map
        :localleader

        ;; "x" #'org-cut-subtree ; todo not sure if m is the proper way to do it
        ;; "y" #'org-paste-subtree

        ;; doom maps to clocking menu by deafult
        "c" #'my/org-inline-created
        ;; doom maps to refile menu by default
        "r" #'org-refile

        "u" #'my/org-unschedule
        "X" #'my/org-wipe-subtree
        ">" #'org-demote-subtree
        "<" #'org-promote-subtree
        "," #'my/org-quicknote))

(if (boundp 'doom-version)
    (defun     --set-leader-keys (&rest args) ()) ;; TODO warn??
  (defalias '--set-leader-keys #'spacemacs/set-leader-keys))

(--set-leader-keys
 "A"   #'my/switch-to-agenda


 "RET" #'helm-swoop
 "S s" #'my/search
 "S c" #'my/search-code
 "S q" #'my/org-ql-search
 "S t" #'my/org-ql-search-tags

 ;; TODO shit! configure other engines as well
 ;; lets you enger an interactive query
 "s G" #'engine/search-google

 ;; TODO extract search-hotkeys so it's easy to extract for the post?
 "p P" #'helm-projectile-find-file-in-known-projects

 ;; TODO link to my post?
 "q q" #'kill-emacs)


;; TODO use doom bindings?
(global-set-key (kbd "<f1>") #'my/search)
(global-set-key (kbd "<f3>") #'my/search-code)
(global-set-key (kbd "<f4>") #'org-capture)
;;;
