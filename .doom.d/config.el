;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; NOTE: run 'doom refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Dima Gerasimov"
      user-mail-address "karlicoss@gmail.com")

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
(load! "../dotfiles-emacs/.emacs.d/config.el")


(after! so-long
  ;; was messing with org-mode... wonder if I can adjust it just for org
  (setq so-long-threshold 1000))


;; TODO open an issue in doom emacs? seems like a wrong regex for helm
;; https://github.com/cosmicexplorer/helm-rg/blob/master/helm-rg.el#L865
(with-eval-after-load 'helm-rg
  (set-popup-rule! "^*helm-rg" :ttl nil :select t :size 0.45))

(after! org-ql-view
  (set-popup-rule! "^*Org QL View:" :ignore t))


(after! org-agenda
  (set-popup-rule! my/org-agenda-buffer-name :ignore t))

(after! undo-tree
  ;; see https://github.com/hlissner/doom-emacs/issues/1407#issuecomment-491931901
  (setq undo-limit 40000
        undo-outer-limit 8000000
        undo-strong-limit 100000))
