# Defined in - @ line 1
function lt --wraps='exa --group-directories-first -lFTL' --description 'alias lt=exa --group-directories-first -lFTL'
  exa --group-directories-first -lFTL $argv;
end
