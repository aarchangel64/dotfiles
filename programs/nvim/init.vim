set relativenumber

noremap <SPACE> <Nop>
let mapleader = "\<Space>"

" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin(stdpath('data') . '/plugged')

Plug 'easymotion/vim-easymotion'
Plug 'chrisbra/vim-commentary'

" Initialize plugin system
call plug#end()

" Doom-emacs-esque key bindings
nmap <leader>fs :update<cr>
nmap <leader>qq :q<cr>

" vim-sneak like motions for vim-easymotion
nmap s <Plug>(easymotion-s2)
nmap t <Plug>(easymotion-t2)
