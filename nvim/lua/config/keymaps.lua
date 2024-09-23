-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
local map = vim.keymap.set

-- Remove LazyVim default window resizing
vim.keymap.del("n", "<C-Up>")
vim.keymap.del("n", "<C-Down>")
vim.keymap.del("n", "<C-Left>")
vim.keymap.del("n", "<C-Right>")

-- Add window resizing
map({ "n", "x", "i" }, "<C-S-Up>", "<cmd>resize +2<cr>", { desc = "Increase Window Height" })
map({ "n", "x", "i" }, "<C-S-Down>", "<cmd>resize -2<cr>", { desc = "Decrease Window Height" })
map({ "n", "x", "i" }, "<C-S-Left>", "<cmd>vertical resize +2<cr>", { desc = "Decrease Window Width" })
map({ "n", "x", "i" }, "<C-S-Right>", "<cmd>vertical resize -2<cr>", { desc = "Increase Window Width" })

-- Move Lines
map("n", "<A-Down>", "<cmd>m .+1<cr>==", { desc = "Move Down" })
map("n", "<A-Up>", "<cmd>m .-2<cr>==", { desc = "Move Up" })
map("i", "<A-Down>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move Down" })
map("i", "<A-Up>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move Up" })
map("v", "<A-Down>", ":m '>+1<cr>gv=gv", { desc = "Move Down" })
map("v", "<A-Up>", ":m '<-2<cr>gv=gv", { desc = "Move Up" })

-- R to delete without putting in register
map({ "n", "x" }, "R", '"_d', { desc = "Delete Without Yank" })
