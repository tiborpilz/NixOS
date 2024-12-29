vim.g.airline_left_sep = ""
vim.g.airline_right_sep = ""
vim.g.airline_theme = "base16"

-- Diagnostic Settings
vim.fn.sign_define("DiagnosticSignError", { text = "", texthl = "DiagnosticSignError" })
vim.fn.sign_define("DiagnosticSignWarn", { text = "", texthl = "DiagnosticSignWarn" })
vim.fn.sign_define("DiagnosticSignInfo", { text = "", texthl = "DiagnosticSignInfo" })
vim.fn.sign_define("DiagnosticSignHint", { text = "", texthl = "DiagnosticSignHint" })

vim.diagnostic.config { float = { border = "rounded" } }

return {
  -- Icons
  {
    'nvim-tree/nvim-web-devicons',
    config = function()
      require('nvim-web-devicons').setup({
        color_icons = true,
      })
    end,
  },
  -- Status line
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      -- TODO: This doesn't work, setting the background via nord theme override
      vim.cmd [[ hi lualine_c_normal guibg=NONE ]]

      local nord_theme_custom = require('lualine.themes.nord')
      nord_theme_custom.normal.c.bg = 'none'
      nord_theme_custom.inactive.c.bg = 'none'


      require('lualine').setup({
        options = {
          theme = nord_theme_custom,
          section_separators = { left = '', right = '' },
          component_separators = { left = '', right = '' },
          globalstatus = false,
        },
        sections = {
          lualine_a = {'mode'},
          lualine_b = {'branch'},
          lualine_c = {'filename'},
          lualine_x = {
            'filetype',
            'location',
            {
              'diagnostics',

              -- Table of diagnostic sources, available sources are:
              --   'nvim_lsp', 'nvim_diagnostic', 'nvim_workspace_diagnostic', 'coc', 'ale', 'vim_lsp'.
              -- or a function that returns a table as such:
              --   { error=error_cnt, warn=warn_cnt, info=info_cnt, hint=hint_cnt }
              sources = { 'nvim_lsp', 'nvim_diagnostic' },

              -- Displays diagnostics for the defined severity types
              sections = { 'error', 'warn', 'info', 'hint' },

              diagnostics_color = {
                -- Same values as the general color option can be used here.
                error = 'DiagnosticError', -- Changes diagnostics' error color.

                info  = 'DiagnosticInfo',  -- Changes diagnostics' info color.
                hint  = 'DiagnosticHint',  -- Changes diagnostics' hint color.
              },
              symbols = {error = '', warn = '', info = '', hint = ''},
              colored = true,           -- Displays diagnostics status in color if set to true.
              update_in_insert = true, -- Update diagnostics in insert mode.
              always_visible = true,   -- Show diagnostics even if there are none.
            },
          },
          lualine_y = {},
          lualine_z = {},
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {'filename'},
          lualine_x = {},
          lualine_y = {},
          lualine_z = {},
        },
        extensions = {
          'nvim-tree',
          'mason',
          'lazy',
        },
      })
    end,
  },
  -- {
  --   "vim-airline/vim-airline",
  --   dependencies = {"vim-airline/vim-airline-themes"},
  -- },

  -- Floating Window Borders
  -- {
  --   "mikesmithgh/borderline.nvim",
  --   enabled = true,
  --   lazy = true,
  --   event = 'VeryLazy',
  --   config = function()
  --     require('borderline').setup({
  --       --  ...
  --     })
  --   end,
  -- },

  -- Colorschemes / Themes
  {"eddyekofo94/gruvbox-flat.nvim"},
  {"marko-cerovac/material.nvim"},
  {"kdheepak/monochrome.nvim"},
  {"EdenEast/nightfox.nvim"},
  {"RRethy/nvim-base16"},
  {"mcchrish/zenbones.nvim"},
  {"rktjmp/lush.nvim"},
  {"yorickpeterse/nvim-grey"},
  {
    "shaunsingh/nord.nvim",
    config = function()
      vim.cmd("colorscheme nord")
    end,
  },
  {
    'AlexvZyl/nordic.nvim',
    lazy = false,
    priority = 1000,
    config = function()
      require('nordic').load()
    end
  },

  -- Messages, Cmdline & Popupmenu replacement
  { "folke/noice.nvim" },

  -- Screenkey
  {
    "NStefan002/screenkey.nvim",
    lazy = false,
    version = "*",
    config = function()
      require("screenkey").setup({
        -- Center
        win_opts = {
          width = 20;
          col = (vim.o.columns / 2) + 10,
          row = 1,
          border = "none",
          height = 1,
        },
      })
    end,
  },
}

