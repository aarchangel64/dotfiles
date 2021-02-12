#!/usr/bin/fish
set pdf (string replace "docx" "pdf" $argv)
if ! test -e $pdf
	unoconv $argv
end
zathura $pdf
