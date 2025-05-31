-- Diagnostic Settings
vim.fn.sign_define("DiagnosticSignError", { text = "", texthl = "DiagnosticSignError" })
vim.fn.sign_define("DiagnosticSignWarn", { text = "", texthl = "DiagnosticSignWarn" })
vim.fn.sign_define("DiagnosticSignInfo", { text = "", texthl = "DiagnosticSignInfo" })
vim.fn.sign_define("DiagnosticSignHint", { text = "", texthl = "DiagnosticSignHint" })

-- vim.diagnostic.config { float = { border = "" } }


-- Disable white border next to file tree
vim.opt.fillchars = vim.opt.fillchars + 'vert: '

vim.g.tpipeline_autoembed = 0

return {
  -- Layout
  {
    "folke/edgy.nvim",
    event = "VeryLazy",
    opts = {}
  },
  -- Icons
  {
    'nvim-tree/nvim-web-devicons',
    config = function()
      require('nvim-web-devicons').setup({
        color_icons = true,
      })
    end,
  },
  -- Status Column
  -- {
  --   "lewis6991/gitsigns.nvim",
  --   config = function()
  --     require("gitsigns").setup({
  --       signs = {
  --         add          = { text = '┃' },
  --         change       = { text = '┃' },
  --         delete       = { text = '_' },
  --         topdelete    = { text = '‾' },
  --         changedelete = { text = '~' },
  --         untracked    = { text = '┆' },
  --       },
  --       preview_config = {
  --         border = { " ", " ", " ", " ", " ", " ", " ", " " },
  --       },
  --     })
  --
  --     vim.keymap.set("n", "<leader>gB", "<cmd>Gitsigns blame_line<cr>", { desc = "Blame Line" })
  --   end,
  -- },
  {
    "luukvbaal/statuscol.nvim",
    dependencies = {
      "fussenegger/nvim-dap",
    },
    opts = function()
      local builtin = require('statuscol.builtin')
      return {
        setopt = true,
        -- override the default list of segments with:
        -- number-less fold indicator, then signs, then line number & separator
        segments = {
          { text = { builtin.foldfunc }, click = 'v:lua.ScFa' },
          { text = { '%s' },             click = 'v:lua.ScSa' },
          {
            text = { builtin.lnumfunc, ' ' },
            condition = { true, builtin.not_empty },
            click = 'v:lua.ScLa',
          },
        },
      }
    end,
  },

  -- Zen Mode
  {
    "folke/zen-mode.nvim",
    cmd = "ZenMode",
    keys = {
      { "<leader>zz", "<cmd>ZenMode<cr>", desc = "Toggle Zen Mode" },
    },
    opts = {
      window = {
        width = 0.85, -- width will be 85% of the editor width
        options = {
          number = false,
          relativenumber = false,
          signcolumn = "no",
          cursorline = false,
          cursorcolumn = false,
          foldcolumn = "0",
          list = false,
        },
      },
      plugins = {
        gitsigns = { enabled = false },
        tmux = { enabled = false },
        diagnostics = { enabled = false }, -- disable diagnostics
        kitty = {
          enabled = false,
          font = "+2", -- font size increment
        },
      },
      on_open = function()
        -- Disable status line in zen mode
        vim.opt.laststatus = 0
      end,
      on_close = function()
        -- Re-enable status line when exiting zen mode
        vim.opt.laststatus = 2
      end,
    },
  },

  -- Switching Colorschemes
  {
    "vague2k/huez.nvim",
    -- if you want registry related features, uncomment this
    -- import = "huez-manager.import"
    branch = "stable",
    event = "UIEnter",
    config = function()
      require("huez").setup({})
    end,
  },

  -- Color Buddy <3 --
  "tjdevries/colorbuddy.nvim",

  -- Colorschemes / Themes
  {
    "shaunsingh/nord.nvim",
    config = function()
      vim.g.nord_contrast = true
      vim.g.nord_borders = false
      vim.g.nord_disable_background = true
      vim.g.nord_italic = false
      vim.g.nord_uniform_diff_background = false
      vim.g.nord_bold = true

      require('nord').set()
    end,
  },
  "rktjmp/lush.nvim",
  "kdheepak/monochrome.nvim",
  "Yazeed1s/minimal.nvim",
  "neanias/everforest-nvim",
  {
    "rose-pine/neovim",
    name = "rose-pine",
    config = function()
      vim.cmd("colorscheme rose-pine")
    end
  },
  {
    "zenbones-theme/zenbones.nvim",
    -- Optionally install Lush. Allows for more configuration or extending the colorscheme
    -- If you don't want to install lush, make sure to set g:zenbones_compat = 1
    -- In Vim, compat mode is turned on as Lush only works in Neovim.
    dependencies = "rktjmp/lush.nvim",
    lazy = false,
    priority = 1000,
    -- you can set set configuration options here
    config = function()
      vim.g.zenbones_darken_comments = 45
      vim.cmd.colorscheme('zenbones')
    end
  },

  --- Icons
  {
    'echasnovski/mini.icons',
    config = function()
      require('mini.icons').setup()
    end,
  },
  -- Screenkey
  {
    "NStefan002/screenkey.nvim",
    lazy = false,
    version = "*",
    config = function()
      require("screenkey").setup({
        -- Center
        win_opts = {
          width = 20,
          col = (vim.o.columns / 2) + 10,
          row = 1,
          border = "none",
          height = 1,
        },
      })
    end,
  },
  -- Send new BG Color to Kitty
  {
    "shaun-mathew/Chameleon.nvim",
    config = function()
      require("chameleon").setup()
    end,
  }
}
