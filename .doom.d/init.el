;;; init.el -*- lexical-binding: t; -*-

;; NOTE: run doom sync after changing these
;; NOTE SPC h d h to access Doom's documentation.
;; NOTE press K on module's name to view its documentation. This works on flags too.
;; NOTE gd on a module to browse its source directory

(doom! :input
       :completion
       company ;; TODO do I need it with lsp?
       (helm)
       ;; meh, fuzzy makes it pretty weird..
       ;; +fuzzy)

       ;;ido               ; the other *other* search engine...
       ;;ivy               ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom
       doom-dashboard    ; a nifty splash screen for Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra
       indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       vc-gutter         ; vcs diff in the fringe
       (window-select     ; visually switch windows
        +numbers)
       ;; workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere)
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent (TODO)
       parinfer

       snippets          ; my elves. They type so I don't have to

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; a consistent, cross-platform shell (WIP)

       :checkers
       syntax
       ;; TODO just make these on demand?
       ;; spell             ;; flycheck?
       ;; grammar           ;; languagetool/writegood mode

       :tools
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       ;;docker
       ;;ein               ; tame Jupyter notebooks with emacs (TODO)
       (eval +overlay)     ; run code, run (also, repls)
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       ;; lsp
       magit
       ;;pdf               ; pdf enhancements (TODO)
       :lang
       data              ; CSV/JSON modes
       emacs-lisp
       javascript
       markdown
       (org
        +roam
        +dragndrop       ; drag & drop files/images into org buffers (TODO)
        ;;+jupyter        ; ipython/jupyter support for babel (TODO)
        +present)
       python
       sh
       web ;; TODO not sure? do I need it or js is enough?
       
       ;;rust

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;; TODO try with axol?
       ;;(rss +org)        ; emacs as an RSS reader

       :config
       ;;literate
       (default +bindings +smartparens))
