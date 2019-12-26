(setq my/spacemacs-layers
      '(
        (colors :variables colors-enable-nyan-cat-progress-bar t)
        ))

(setq my/additional-packages
      '(
        el-patch

        xclip
        sync-recentf

        org-plus-contrib
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
