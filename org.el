; assumes with-eval-after-load 'org

;; TODO eh, should probably reuse  org-insert-heading-after-current ?
(defun my/org-quicknote ()
  """
  Inserts child heading with an inactive timestamp, useful for adding quick notes to org items

  E.g. before:

  * parent note
    some text <cursor is here>
  ** child
     child text

  after you call this function, you end up with:
  * parent note
    some text
  ** child
     child text
  ** [YYYY-MM-DD Mom HH:MM] <cursor is here, edit mode>

  """
  (interactive)
  (let* ((has-children (save-excursion (org-goto-first-child))))
    (org-end-of-subtree)
    (let ((org-blank-before-new-entry nil)) (org-insert-heading))
    (org-time-stamp '(16) t) ; 16 means inactive
    (if has-children nil (org-do-demote)) (insert " ")
    (evil-append 1)))

(defun my/org-eshell-command (command)
  ;; run command in eshell (e.g. [[eshell: ls -al /home]]
  (interactive)
  (progn
    (eshell)
    (eshell-return-to-prompt)
    (insert command)
    (eshell-send-input)))

;; TODO export it properly?
(org-add-link-type "eshell" 'my/org-eshell-command)


;; 'p': raw prefix, 'P': numeric prefix. confusing ...
(defun my/org-wipe-subtree (&optional n)
  (interactive "p")
  (org-cut-subtree n)
  (pop kill-ring))


;; TODO extract in a sep function? e.g. with-current-entry?
;; TODO ert-deftest?
(defun my/org-inline-created ()
  "Convert CREATED property into inline date"
  (interactive)
  (let ((created (org-entry-get (point) "CREATED"))
        (heading (org-get-heading t t t t))) ;; t t t t means include everything
    (save-excursion
      (org-back-to-heading t)
      (org-edit-headline (concat created " " heading)))
    (org-entry-delete (point) "CREATED")))


;; -- various org-agenda helpers

(defun --my/org-level ()
  (let ((level (org-outline-level)))
    (if (= 1 level) " " (format "%s" level))))


(defun --my/org-has-extras ()
  (if (save-excursion (org-goto-first-child)) "▶" " "))


;; TODO hmm, display instead of "S/D" maybe?
;; TODO maybe even indicate how frequently is it repeated?
(defun --my/org-is-recurring ()
  ;; recurring if deadline is set and deadline string has repeater cookie
  ;; TODO ideally, mirror same thing in org agenda patch?
  (let* ((deadlines (org-entry-get (point) "DEADLINE"))
         (rdeadline (if deadlines (org-get-repeat deadlines) nil)))
    (if rdeadline "↻ " "  ")))
;; so, there is a unicode-fonts package, with it you can get the whole range of arrows e.g. 🔁 🗘 ↺ ↻ ⥀ ⥁ ⟲ ⟳  🗘  ⮍ ⮌ ⮏ ⮎ ⮔ 🔁 🔂 🔃 🔄
;; (this one 🔃 in particular looks nice)
;; however, extra errors not displaying as default font (eg DejaVu Sans Mono), but as Free-Symbola (try describe-char)
;; hmm, even after package removal they are still bizarre! So it could be smth else!
;; hmm, in vim they are different width too..
;; right, apparently it's that http://www.fileformat.info/info/unicode/font/dejavu_sans_mono/blockview.htm?block=arrows
;; and fallback is defined in font family itself?
;; https://www.fileformat.info/info/unicode/block/miscellaneous_symbols_and_arrows/fontsupport.htm ok that explains it...
;; so I have to use normal arrows. Source Code pro doesn't support them too?? so switch to dejavu sans mono
;; TODO also disable using different fonts for different unicode characters? It might make the whole thing kinda slow
;; also see .emacs.d/core/core-fonts-support.el, fallback fonts

(defun --my/org-agenda-extras ()
  (concat
    (--my/org-is-recurring)
    (--my/org-has-extras)))


(defun --my/org-tags-extras ()
  (concat
   (--my/org-level)))

;; --
