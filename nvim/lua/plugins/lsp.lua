return {
	{
		"williamboman/mason.nvim",
		lazy = false,
		priority = 100,
		config = function()
			require("mason").setup({})
			local registry = require("mason-registry")
			registry.refresh()
		end,
	},
	{
		"williamboman/mason-lspconfig.nvim",
		dependencies = { "williamboman/mason.nvim", "neovim/nvim-lspconfig" },
	},
	{
		"VonHeikemen/lsp-zero.nvim",
		branch = "v4.x",
		-- dependencies = { "neovim/nvim-lspconfig", "williamboman/mason-lspconfig" },
		config = function()
			local lsp_zero = require("lsp-zero")

			-- lsp_attach is where you enable features that only work
			-- if there is a language server active in the file
			local lsp_attach = function(client, bufnr)
				local wk = require("which-key")

				wk.add({
					{
						mode = { "n", "v" },
						{ "K", "<cmd>lua vim.lsp.buf.hover()<cr>", desc = "Hover" },
						{
							"<leader>c",
							group = "Code",
							buffer = bufnr,
							{ "<leader>cr", "<cmd>lua vim.lsp.buf.rename()<cr>", desc = "Rename" },
							{ "<leader>cd", "<cmd>lua vim.lsp.buf.definition()<cr>", desc = "Go To Definition" },
							{ "<leader>cD", "<cmd>lua vim.lsp.buf.declaration()<cr>", desc = "Go To Declaration" },
							{
								"<leader>ci",
								"<cmd>lua vim.lsp.buf.implementation()<cr>",
								desc = "Go To Implementation",
							},
							{
								"<leader>ct",
								"<cmd>lua vim.lsp.buf.type_definition()<cr>",
								desc = "Go To Type Definition",
							},
							{ "<leader>cR", "<cmd>lua vim.lsp.buf.references()<cr>", desc = "References" },
							{ "<leader>cS", "<cmd>lua vim.lsp.buf.signature_help()<cr>", desc = "Signature Help" },
							{ "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<cr>", desc = "Code Actions" },
						},
					},
				})

				-- vim.keymap.set({ "n", "x" }, "<F3>", "<cmd>lua vim.lsp.buf.format({async = true})<cr>", opts)
			end

			lsp_zero.extend_lspconfig({
				sign_text = true,
				lsp_attach = lsp_attach,
				capabilities = require("cmp_nvim_lsp").default_capabilities(),
			})

			-- TODO: make docker compose LSP trigger on entering a compose file
			require("mason-lspconfig").setup({
				-- stylua: ignore start
				ensure_installed = { "lua_ls", "rust_analyzer", "docker_compose_language_service", "dockerls" },
				-- stylua: ignore end
				handlers = {
					function(server_name) require("lspconfig")[server_name].setup({}) end,
					["rust_analyzer"] = function()
						require("lspconfig").rust_analyzer.setup({
							["rust-analyzer"] = {
								cargo = {
									allFeatures = true,
									loadOutDirsFromCheck = true,
									runBuildScripts = true,
								},
								-- Add clippy lints for Rust.
								checkOnSave = {
									allFeatures = true,
									command = "clippy",
									extraArgs = {
										"--",
										"--no-deps",
										"-Dclippy::correctness",
										"-Dclippy::complexity",
										"-Wclippy::perf",
										"-Wclippy::pedantic",
									},
								},
								procMacro = {
									enable = true,
									ignored = {
										["async-trait"] = { "async_trait" },
										["napi-derive"] = { "napi" },
										["async-recursion"] = { "async_recursion" },
									},
								},
							},
						})
					end,
					["lua_ls"] = function()
						require("lspconfig").lua_ls.setup({
							on_init = function(client)
								local path = client.workspace_folders[1].name
								if
									vim.loop.fs_stat(path .. "/.luarc.json")
									or vim.loop.fs_stat(path .. "/.luarc.jsonc")
								then
									return
								end

								client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
									runtime = {
										-- Tell the language server which version of Lua you're using
										-- (most likely LuaJIT in the case of Neovim)
										version = "LuaJIT",
									},
									-- Make the server aware of Neovim runtime files
									workspace = {
										checkThirdParty = false,
										library = {
											vim.env.VIMRUNTIME,
											-- Depending on the usage, you might want to add additional paths here.
											-- "${3rd}/luv/library"
											-- "${3rd}/busted/library",
										},
										-- or pull in all of 'runtimepath'. NOTE: this is a lot slower
										-- library = vim.api.nvim_get_runtime_file("", true)
									},
								})
							end,
							settings = {
								Lua = {},
							},
						})
					end,
				},
			})

			local cmp = require("cmp")
			local cmp_action = require("lsp-zero").cmp_action()
			local lspkind = require("lspkind")

			cmp.setup({
				enabled = function()
					-- disable completion in comments
					local context = require("cmp.config.context")
					-- keep command mode completion enabled when cursor is in a comment
					if vim.api.nvim_get_mode().mode == "c" then
						return true
					else
						return not context.in_treesitter_capture("comment") and not context.in_syntax_group("Comment")
					end
				end,
				sources = {
					{ name = "nvim_lsp" },
				},
				formatting = {
					format = lspkind.cmp_format(),
				},
				mapping = cmp.mapping.preset.insert({
					-- `Enter` key to confirm completion
					["<CR>"] = cmp.mapping.confirm({ select = true }),

					-- Ctrl+Space to trigger completion menu
					["<C-Space>"] = cmp.mapping.complete(),

					-- ["<Tab>"] = cmp_action.luasnip_supertab(),
					-- ["<S-Tab>"] = cmp_action.luasnip_shift_supertab(),
					["<Tab>"] = cmp_action.vim_snippet_tab_next(),
					["<S-Tab>"] = cmp_action.vim_snippet_prev(),

					-- Scroll up and down in the completion documentation
					["<C-u>"] = cmp.mapping.scroll_docs(-4),
					["<C-d>"] = cmp.mapping.scroll_docs(4),
				}),
				snippet = {
					expand = function(args) vim.snippet.expand(args.body) end,
				},
			})
		end,
	},
	{ "neovim/nvim-lspconfig" },
	{ "hrsh7th/cmp-nvim-lsp" },
	{ "hrsh7th/nvim-cmp" },
	{ "onsails/lspkind.nvim" },
}
