;;; init.el -*- lexical-binding: t; -*-

;; NOTE: run doom sync after changing these

;; NOTE 'SPC h d h' to access Doom's documentation.
;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       :completion
       ;; TODO ?
       company
       helm
       ;;ido               ; the other *other* search engine...
       ;;ivy               ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom
       doom-dashboard    ; a nifty splash screen for Emacs
       ;;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra
       ;;indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;;pretty-code       ; replace bits of code with pretty symbols
       ;;treemacs          ; a project drawer, like neotree but cooler
       vc-gutter         ; vcs diff in the fringe
       ;; TODO ??
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       (window-select     ; visually switch windows
        +numbers)
        ;; TODO not sure if I want that..
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       multiple-cursors  ; editing in many places at once
       ;; TODO?
       ;;objed             ; text object editing for the innocent
       ;; TODO enable
       parinfer          ; turn lisp into python, sort of

       ;; TODO??
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; a consistent, cross-platform shell (WIP)

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;;spell             ; tasing you for misspelling mispelling
       ;; TODO !!
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;; TODO use it
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       lsp
       magit             ; a git porcelain for Emacs
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;; TODO?
       ;;rgb               ; creating color strings
       ;; TODO?
       ;;terraform         ; infrastructure as code
       ;;upload            ; map local to remote projects via ssh/ftp

       :lang
       ;;agda              ; types of types of types of types...
       ;;cc                ; C/C++/Obj-C madness
       ;;coq               ; proofs-as-programs
       data              ; config/data formats
       emacs-lisp
       ;;(haskell +dante)
       javascript
       ;;ledger            ; an accounting system in Emacs
       markdown          ; writing docs for people to ignore
       ;;nix               ; I hereby declare "nix geht mehr!"
       (org              ; organize your plain life in plain text
        +dragndrop       ; drag & drop files/images into org buffers
	;; TODO huh?
        ;;+jupyter        ; ipython/jupyter support for babel
        +present)        ; using org-mode for presentations
       (python            ; beautiful is better than ugly
        +lsp)
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       web               ; the tubes
       
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;; TODO try with axol?
       ;;(rss +org)        ; emacs as an RSS reader
       ;; TODO?
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))
