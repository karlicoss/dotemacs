;;; init.el -*- lexical-binding: t; -*-
;;; NOTE: run vimdiff init.el ~/.config/emacs/init.example.el now and then to sync changes

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

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
       (emoji +unicode)
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
       ;;lispy ;; todo maybe try it?
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
       ;; spell             ;; todo flyspell?
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

       :os
       ;; tty ;; todo try it? improves terminal emacs experience?
       :lang
       clojure
       data              ; CSV/JSON modes
       emacs-lisp
       json
       javascript
       markdown
       (org
        +roam
        +dragndrop       ; drag & drop files/images into org buffers (TODO)
        ;;+jupyter        ; ipython/jupyter support for babel (TODO)
        +present)
       python
       ;;rust
       sh
       web ;; TODO not sure? do I need it or js is enough?
       yaml

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;; TODO try rss reader with axol?
       ;;(rss +org)        ; emacs as an RSS reader

       :config
       ;;literate
       (default +bindings +smartparens))
