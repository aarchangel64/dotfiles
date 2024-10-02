return {
  -- add theme
  { "akinsho/horizon.nvim" },
  -- { "lancewilhelm/horizon-extended.nvim" },
  -- { "challenger-deep-theme/vim" },
  -- { "maxmx03/dracula.nvim" },
  -- { "Jas-SinghFSU/drappuccin" },
  -- { "catppuccin/nvim" },

  -- configure to use theme
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "horizon",
    },
  },

  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
  },

  -- Prevent mason lsp conflicting with rustaceanvim
  {
    "neovim/nvim-lspconfig",
    opts = {
      setup = {
        rust_analyzer = function()
          return true
        end,
      },
    },
  },

  {
    "HiPhish/rainbow-delimiters.nvim",
    config = function()
      -- Run :so $VIMRUNTIME/syntax/hitest.vim to see highlight groups
      require("rainbow-delimiters.setup").setup({
        -- Not needed in horizon theme
        -- highlight = {
        --   "RainbowRed",
        --   "RainbowYellow",
        --   "RainbowBlue",
        --   "RainbowOrange",
        --   "RainbowGreen",
        --   "RainbowViolet",
        --   "RainbowCyan",
        -- },
      })
    end,
  },

  {
    "dstein64/nvim-scrollview",
    opts = {
      signs_on_startup = { "all" },
    },
  },

  -- Supertab
  {
    "hrsh7th/nvim-cmp",
    ---@param opts cmp.ConfigSchema
    opts = function(_, opts)
      local has_words_before = function()
        unpack = unpack or table.unpack
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
      end

      local cmp = require("cmp")

      opts.mapping = vim.tbl_extend("force", opts.mapping, {
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            -- You could replace select_next_item() with confirm({ select = true }) to get VS Code autocompletion behavior
            cmp.select_next_item()
          elseif vim.snippet.active({ direction = 1 }) then
            vim.schedule(function()
              vim.snippet.jump(1)
            end)
          elseif has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif vim.snippet.active({ direction = -1 }) then
            vim.schedule(function()
              vim.snippet.jump(-1)
            end)
          else
            fallback()
          end
        end, { "i", "s" }),
      })
    end,
  },
}
