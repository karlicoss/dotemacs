function em {
  # TODO gemacs?
  emacs "$@" & disown
}

function empip {
  # open installed PIP package adn search in it
  local pkg=$(find ~/.local/lib/python3.8/site-packages -maxdepth 1 -type d | fzf)
  if [ -z $pkg ]; then
     2>&1 echo "no matches"
     exit 1
  fi
  # ugh. window-setup-hook is too early apparently??
  emacs --chdir "$pkg" \
   --eval "(add-hook 'doom-load-theme-hook '+default/search-cwd)" \
   --eval "(setq projectile-project-root \"$pkg\")"               \
  & disown 
}
