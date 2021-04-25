# Defined via `source`
function cdg --wraps='cd (git rev-parse --show-toplevel)' --description 'CD to git repo root.'
  cd (git rev-parse --show-toplevel) $argv; 
end
