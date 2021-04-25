# Defined via `source`
function rgh --wraps='kitty +kitten hyperlinked_grep' --wraps=rg --description 'Search with rg using kitty hyperlinks.'
  kitty +kitten hyperlinked_grep $argv; 
end
