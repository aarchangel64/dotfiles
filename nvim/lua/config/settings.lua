vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.opt.relativenumber = true
-- vim.opt.hlsearch = false
vim.opt.ignorecase = true
vim.opt.smartcase = true
-- Sync with system clipboard
vim.opt.clipboard:append({ "unnamedplus" })
-- Open all folds by default
vim.opt.foldenable = false
