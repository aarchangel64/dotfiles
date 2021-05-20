# Defined via `source`
function icat --wraps='kitty +kitten icat' --description 'View image in kitty.'
  kitty +kitten icat $argv; 
end
