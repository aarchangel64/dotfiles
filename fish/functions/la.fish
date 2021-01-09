# Defined in - @ line 1
function la --wraps='exa -Fla --group-directories-first' --description 'alias la=exa -Fla --group-directories-first'
  exa -Fla --group-directories-first $argv;
end
