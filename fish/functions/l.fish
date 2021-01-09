# Defined in - @ line 1
function l --wraps='exa -Fl --group-directories-first' --description 'alias l=exa -Fl --group-directories-first'
  exa -Fl --group-directories-first $argv;
end
