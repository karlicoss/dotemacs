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


(after! so-long
  ;; was messing with org-mode... wonder if I can adjust it just for org
  (setq so-long-threshold 1000))


;; TODO open an issue in doom emacs? seems like a wrong regex for helm
;; https://github.com/cosmicexplorer/helm-rg/blob/master/helm-rg.el#L865
(with-eval-after-load 'helm-rg
  (set-popup-rule! "^*helm-rg" :ttl nil :select t :size 0.45))


;; fix case insensitivity in popup detection
;; (after! org
;;   (set-popup-rules!
;;     '(("^CAPTURE.*\\.org$"  :ignore t)
;;       ("^CAPTURE-.*\\.org$" :size 0.25 :quit nil :select t :autosave t))

;; right, this seems like a better fix
;; see https://github.com/hlissner/doom-emacs/pull/2619#issuecomment-592186770
(defadvice! +popup--make-case-sensitive-a (orig-fn &rest args)
  :around #'display-buffer-assq-regexp
  (let (case-fold-search)
    (apply orig-fn args)))

