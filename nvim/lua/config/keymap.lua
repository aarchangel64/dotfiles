local wk = require("which-key")

local hop = require("hop")
local hop_hint = require("hop.hint")

local bmui = require("buffer_manager.ui")

wk.add({
	{
		mode = { "n" },
		-- Move Line
		{ "<M-Up>", "<CMD>m -2<CR>", desc = "Move Line Up" },
		{ "<M-Down>", "<CMD>m +1<CR>", desc = "Move Line Down" },
	},
	{
		mode = { "n", "x" },
		-- Leader submenu maps
		{
			"<leader>t",
			group = "Toggle",
			{ "<leader>tT", "<CMD>Twilight<CR>", desc = "Twilight" },
			{ "<leader>tt", "<CMD>TodoTelescope<CR>", desc = "Todo List" },
		},
		{
			"<leader>f",
			group = "File",
			{ "<leader>fs", "<CMD>write<CR>", desc = "Save" },
		},
		{
			"<leader>q",
			group = "Quit",
			{ "<leader>qq", "<CMD>qa<CR>", desc = "Exit Neovim" },
			{ "<leader>qs", "<CMD>wqa<CR>", desc = "Exit (Save All)" },
			{ "<leader>q!", "<CMD>wq!<CR>", desc = "Exit (Without Saving)" },
		},
		{
			"<leader>b",
			group = "Buffers",
			{ "<leader><lt>", function() bmui.toggle_quick_menu() end, desc = "Switch Buffer" },
			{ "<leader>bn", function() bmui.nav_next() end, desc = "Next Buffer" },
			{ "<leader>bp", function() bmui.nav_prev() end, desc = "Previous Buffer" },
			{ "<leader>bd", "<CMD>quit<CR>", desc = "Close Buffer" },
			-- expand = function() return require("which-key.extras").expand.buf() end,
		},
		{
			"<leader>c",
			group = "Code",
			{ "<leader>cf", function() require("conform").format({ async = true }) end, desc = "Format Buffer" },
			{ "<leader>cx", "<CMD>Trouble diagnostics toggle filter.buf=0<CR>", desc = "View Diagnostics" },
			{ "<leader>cq", "<CMD>Trouble qflist toggle<CR>", desc = "View Quickfixes" },
		},
		-- File Browser
		{
			"<leader>.",
			"<CMD>Telescope file_browser path=%:p:h select_buffer=true<CR>",
			desc = "Browse Current Directory",
		},
		-- R to delete without putting in register
		{ "R", '"_d', desc = "Delete Without Yank" },
		-- proxy to window mappings
		{ "<leader>w", proxy = "<c-w>", group = "Window" },
		{ "<leader>w<Up>", "<CMD>wincmd k<CR>" },
		{ "<leader>w<Down>", "<CMD>wincmd j<CR>" },
		{ "<leader>w<Left>", "<CMD>wincmd h<CR>" },
		{ "<leader>w<Right>", "<CMD>wincmd l<CR>" },
	},
	-- hop.nvim maps
	{
		mode = { "n", "x", "o" },
		-- mode = { "n", "v" },
		{
			"f",
			function()
				hop.hint_char1({
					direction = hop_hint.HintDirection.AFTER_CURSOR,
					current_line_only = true,
				})
			end,
		},
		{
			"F",
			function()
				hop.hint_char1({
					direction = hop_hint.HintDirection.BEFORE_CURSOR,
					current_line_only = true,
				})
			end,
		},
		{ "s", function() hop.hint_char2({ direction = hop_hint.HintDirection.AFTER_CURSOR }) end },
		{ "S", function() hop.hint_char2({ direction = hop_hint.HintDirection.BEFORE_CURSOR }) end },
		{ "t", function() hop.hint_words({ direction = hop_hint.HintDirection.AFTER_CURSOR }) end },
		{ "T", function() hop.hint_words({ direction = hop_hint.HintDirection.BEFORE_CURSOR }) end },
	},
})
