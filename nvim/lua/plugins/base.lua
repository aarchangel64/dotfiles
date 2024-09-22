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
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {},
	},

	{
		"petertriho/nvim-scrollbar",
		opts = {},
	},

	-- TODO: figure out why this isn't showing in the scrollbar
	{
		"kevinhwang91/nvim-hlslens",
		config = function()
			require("hlslens").setup({ calm_down = true })
			-- require('hlslens').setup() is not required
			require("scrollbar.handlers.search").setup({
				-- hlslens config overrides
			})
		end,
	},

	{
		"lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup()
			require("scrollbar.handlers.gitsigns")
		end,
	},

	{
		"nvim-telescope/telescope-file-browser.nvim",
		dependencies = {
			{
				"nvim-telescope/telescope.nvim",
				config = function() require("telescope").load_extension("file_browser") end,
			},
			"nvim-lua/plenary.nvim",
		},
	},

	{
		"j-morano/buffer_manager.nvim",
		opts = {
			show_indicators = "before",
			order_buffers = "lastused",
		},
		dependencies = { "nvim-lua/plenary.nvim" },
	},

	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		opts = {
			delay = function(ctx) return ctx.plugin and 0 or 500 end,
		},
		keys = {
			{
				"<leader>?",
				function() require("which-key").show({ global = false }) end,
				desc = "Buffer Local Keymaps (which-key)",
			},
		},
	},

	{
		"folke/trouble.nvim",
		opts = {},
		cmd = "Trouble",
	},

	{
		"kylechui/nvim-surround",
		version = "*", -- Use for stability; omit to use `main` branch for the latest features
		event = "VeryLazy",
	},

	{
		"numToStr/Comment.nvim",
		opts = {
			-- add any options here
		},
	},

	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		config = true,
	},

	{
		"windwp/nvim-ts-autotag",
		opts = {},
		-- lazy = false,
		event = { "BufReadPre", "BufNewFile" },
		dependencies = { "nvim-treesitter/nvim-treesitter" },
	},

	{
		"HiPhish/rainbow-delimiters.nvim",
		config = function()
			-- vim.api.nvim_command([[
			-- 	au ColorSchemePre horizon-extended.nvim highlight link RainbowDelimiterRed RainbowRed
			-- 	au ColorSchemePre horizon-extended.nvim highlight link RainbowDelimiterYellow RainbowYellow
			-- ]])
			-- vim.api.nvim_create_autocmd({ "ColorSchemePre" }, {
			-- 	pattern = "horizon-extended.nvim",
			-- 	command = "highlight link RainbowDelimiterRed RainbowRed",
			-- })
			--
			-- Run :so $VIMRUNTIME/syntax/hitest.vim to see highlight groups
			require("rainbow-delimiters.setup").setup({
				highlight = {
					"RainbowRed",
					"RainbowYellow",
					"RainbowBlue",
					"RainbowOrange",
					"RainbowGreen",
					"RainbowViolet",
					"RainbowCyan",
				},
			})
		end,
	},

	{
		"folke/twilight.nvim",
		opts = { context = 25 },
	},

	{
		"smoka7/hop.nvim",
		version = "*",
		opts = {
			keys = "arstneiodhgjqwfpluy;",
		},
	},

	{
		"JoosepAlviste/nvim-ts-context-commentstring",
	},

	{
		"nvim-treesitter/nvim-treesitter",
		config = function()
			require("nvim-treesitter.configs").setup({
				-- stylua: ignore start
				ensure_installed = { "angular", "arduino", "asm", "astro", "awk", "bash", "bibtex", "c", "c_sharp", "clojure", "cmake", "comment", "commonlisp", "cpp", "css", "csv", "cuda", "diff", "disassembly", "dockerfile", "doxygen", "editorconfig", "fennel", "fish", "gdscript", "gdshader", "git_config", "git_rebase", "gitattributes", "gitcommit", "gitignore", "glsl", "gnuplot", "gpg", "graphql", "haskell", "hlsl", "html", "htmldjango", "http", "hyprlang", "java", "javascript", "jq", "jsdoc", "json", "json5", "jsonc", "julia", "kconfig", "kotlin", "latex", "llvm", "lua", "luadoc", "make", "markdown", "markdown_inline", "matlab", "mermaid", "meson", "nasm", "nginx", "ninja", "nix", "objdump", "org", "passwd", "pem", "php", "powershell", "printf", "properties", "python", "r", "regex", "requirements", "robots", "ron", "rust", "scala", "scss", "sql", "ssh_config", "strace", "svelte", "systemverilog", "terraform", "toml", "tsx", "typescript", "typst", "udev", "verilog", "vhdl", "vim", "vimdoc", "vue", "wgsl", "wgsl_bevy", "xml", "yaml" },
				-- stylua: ignore end
				sync_install = false,
				auto_install = true,
				highlight = { enable = true },
			})

			vim.wo.foldmethod = "expr"
			vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"

			-- https://github.com/windwp/nvim-ts-autotag?tab=readme-ov-file#enable-update-on-insert
			-- vim.lsp.handlers["textDocument/publishDiagnostics"] =
			-- 	vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
			-- 		underline = true,
			-- 		virtual_text = {
			-- 			spacing = 5,
			-- 			severity_limit = "Warning",
			-- 		},
			-- 		update_in_insert = true,
			-- 	})
		end,
		build = function() require("nvim-treesitter.install").update({ with_sync = true })() end,
	},
}
