;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; NOTE: run 'doom refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

(setq doom-theme 'doom-one)

;; TODO `relative' ?
(setq display-line-numbers-type t)

(setq doom-localleader-key ",")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K', this will open documentation for it
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


(load! "../configs/emacs/private.el")
(load! "../dotfiles-emacs/.spacemacs.d/config.el")
;; TODO move confil.el to root?


;; TODO use (loop for i from 1 to 10 do (map!... )) ??
(map! :leader
      "0" #'winum-select-window-0-or-10
      "1" #'winum-select-window-1
      "2" #'winum-select-window-2
      "3" #'winum-select-window-3
      "4" #'winum-select-window-4
      "5" #'winum-select-window-5
      "6" #'winum-select-window-6
      "7" #'winum-select-window-7
      "8" #'winum-select-window-8
      "9" #'winum-select-window-9)

(map! :map evil-window-map
      "/" #'split-window-right
      "-" #'split-window-below)


(map! :leader
      "/" #'+default/search-project
      "e" #'+eshell/here)

;; TODO eh, not convinced it's the right way, but at least works
(after! evil-org-agenda
  (evil-define-key 'motion evil-org-agenda-mode-map
    "S" #'org-save-all-org-buffers
    "U" #'my/org-agenda-unschedule))


(after! so-long
  ;; was messing with org-mode... wonder if I can adjust it just for org
  (setq so-long-threshold 1000))


;; TODO open an issue in doom emacs? seems like a wrong regex for helm
;; https://github.com/cosmicexplorer/helm-rg/blob/master/helm-rg.el#L865
(with-eval-after-load 'helm-rg
  (set-popup-rule! "^*helm-rg" :ttl nil :select t :size 0.45))

(map! :n "zz" #'org-capture)

;; TODO open issue in doom emacs?
;; must be really annoying that capture.org is always opened in a popup?
;; TODO I suppose case insensitivity doesn't help
(after! org
  (set-popup-rules!
    '(("^CAPTURE.*\\.org$"  :ignore t)
      ("^CAPTURE-.*\\.org$" :size 0.25 :quit nil :select t :autosave t))))


(after! evil
  (evil-ex-define-cmd "ww" #'save-some-buffers))
