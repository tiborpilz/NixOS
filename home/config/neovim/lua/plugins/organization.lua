return {
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      signs = false,
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    }
  },
  -- Orgmode in Vim
  {
    'nvim-orgmode/orgmode',
    event = 'VeryLazy',
    ft = { 'org' },
    config = function()
      -- Setup orgmode
      require('orgmode').setup({
        org_agenda_files = '~/org/**/*',
        org_default_notes_file = '~/org/refile.org',
        mappings = {
          global = {
            org_agenda = "<leader>na",
            org_capture = "<leader>nx",
          },
        }
      })
      -- TODO: Adjust this https://github.com/nvim-orgmode/orgmode/blob/master/DOCS.md#global-mappings with the current state in Emacs

    end,
  },
  {
    "akinsho/org-bullets.nvim",
    config = function()
      require("org-bullets").setup {
        symbols = { "◉", "○", "✸", "✿" },
      }
    end,
  },
  {
    "lukas-reineke/headlines.nvim",
    dependencies = "nvim-treesitter/nvim-treesitter",
    opts = {}
  },
  -- Org roam in vim
  -- TODO: this slows down startup, investigate
  -- {
  --   "chipsenkbeil/org-roam.nvim",
  --   dependencies = {
  --     {
  --       "nvim-orgmode/orgmode",
  --     },
  --   },
  --   lazy = true,
  --   config = function()
  --     require("org-roam").setup({
  --       directory = "~/org/roam",
  --       -- optional
  --       org_files = {
  --         "~/org",
  --       }
  --     })
  --   end
  -- },
  -- Telescope integration for finding headlines etc.
  {
    "nvim-orgmode/telescope-orgmode.nvim",
    event = "VeryLazy",
    dependencies = {
      "nvim-orgmode/orgmode",
      "nvim-telescope/telescope.nvim",
    },
    config = function()
      require("telescope").load_extension("orgmode")

      vim.keymap.set("n", "<leader>r", require("telescope").extensions.orgmode.refile_heading)
      vim.keymap.set("n", "<leader>fh", require("telescope").extensions.orgmode.search_headings)
      vim.keymap.set("n", "<leader>li", require("telescope").extensions.orgmode.insert_link)
    end,
  },
  -- {"vimwiki/vimwiki"},
  -- {
  --   "nvim-neorg/neorg",
  --   lazy = false,
  --   version = "*",
  --   config = function()
  --     require('neorg').setup {
  --       load = {
  --         ["core.defaults"] = {}, -- loads default behavior
  --         ["core.concealer"] = {}, -- allows for use of pretty icons
  --         ["core.dirman"] = { -- manages Neorg workspaces
  --           config = {
  --             workspace = {
  --               notes = "~/neorg",
  --             },
  --           },
  --         },
  --       },
  --     }
  --   end,
  -- },
}
