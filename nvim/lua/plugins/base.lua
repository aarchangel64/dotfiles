return {
	{
		"lancewilhelm/horizon-extended.nvim",
		lazy = false, -- make sure we load this during startup if it is your main colorscheme
		priority = 1000, -- make sure to load this before all the other start plugins
		config = function()
			-- load the colorscheme here
			vim.cmd.colorscheme("horizon-extended")
		end,
	},

	{
		"nvim-tree/nvim-web-devicons",
		lazy = false,
	},

	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		opts = {
			-- your configuration comes here
			-- or leave it empty to use the default settings
			-- refer to the configuration section below
		},
		keys = {
			{
				"<leader>?",
				function()
					require("which-key").show({ global = false })
				end,
				desc = "Buffer Local Keymaps (which-key)",
			},
		},
	},

	{
		"kylechui/nvim-surround",
		version = "*", -- Use for stability; omit to use `main` branch for the latest features
		event = "VeryLazy",
		config = function()
			require("nvim-surround").setup({
				-- Configuration here, or leave empty to use defaults
			})
		end,
	},

	{
		"numToStr/Comment.nvim",
		opts = {
			-- add any options here
		},
	},

	{
		"smoka7/hop.nvim",
		version = "*",
		opts = {
			keys = "arstneiodhgjqwfpluy;",
		},
		keys = {
			{
				"f",
				function()
					require("hop").hint_char2({
						direction = require("hop.hint").HintDirection.AFTER_CURSOR,
						current_line_only = false,
					})
				end,
				mode = { "n", "x", "o" },
			},
			{
				"F",
				function()
					require("hop").hint_char2({
						direction = require("hop.hint").HintDirection.BEFORE_CURSOR,
						current_line_only = false,
					})
				end,
				mode = { "n", "x", "o" },
			},
			{
				"s",
				function()
					require("hop").hint_words({
						direction = require("hop.hint").HintDirection.AFTER_CURSOR,
						current_line_only = false,
					})
				end,
				mode = { "n", "x", "o" },
			},
			{
				"S",
				function()
					require("hop").hint_words({
						direction = require("hop.hint").HintDirection.BEFORE_CURSOR,
						current_line_only = false,
					})
				end,
				mode = { "n", "x", "o" },
			},
		},
	},

	{
		"nvim-treesitter/nvim-treesitter",
		build = function()
			require("nvim-treesitter.install").update({ with_sync = true })()
		end,
	},
}
