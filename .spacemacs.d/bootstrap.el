(setq my/spacemacs-layers
      '(
        (colors :variables colors-enable-nyan-cat-progress-bar t)
        graphviz
        ))

(setq my/additional-packages
      '(
        el-patch

        xclip
        sync-recentf

        (org-mode 
         :location (recipe
	            :fetcher github
		    :repo "emacs-straight/org-mode"
		    :commit "2096c9c76f"
		    :files ("*.el" "lisp/*.el" "contrib/lisp/*.el")))
        org-drill
        ;; org-plus-contrib
        org-super-agenda
        org-ql
        (org-sync
         :location (recipe
                    :fetcher github
                    :repo "karlicoss/org-sync"
                    :branch "fix-nondeterminsm"))

        helm-rg

        (lsp-haskell
         :location (recipe
                    :fetcher github :repo "emacs-lsp/lsp-haskell"))
        ))

(setq my/excluded-packages
      '(
        org-projectile ; disabled due to some stupid bug https://github.com/syl20bnr/spacemacs/issues/9374
        ))
