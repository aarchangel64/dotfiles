# Defined via `source`
function gdt --wraps='git diff' --wraps='git difftool --no-symlinks --dir-diff' --description 'alias gdt git difftool --no-symlinks --dir-diff'
  git difftool --no-symlinks --dir-diff $argv; 
end
