return {
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      signs = true,
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    },
    keys = {
      { "<leader>pT", "<cmd>TodoTelescope<cr>", desc = "Find TODOs in current project" },
    },
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
        concealcursor = true,
        symbols = { "◉", "○", "✸", "✿" },
      }
    end,
  },
  -- {
  --   "lukas-reineke/headlines.nvim",
  --   dependencies = "nvim-treesitter/nvim-treesitter",
  --   event = "VeryLazy",
  --   opts = {}
  -- },
  -- Org roam in vim
  -- TODO: this slows down startup, investigate
  {
    "chipsenkbeil/org-roam.nvim",
    dependencies = {
      {
        "nvim-orgmode/orgmode",
      },
    },
    lazy = true,
    config = function()
      local org_directory = "~/org"
      local org_roam_directory = org_directory .. "/roam"

      require("org-roam").setup({
        directory = org_roam_directory, 
        -- optional
        org_files = { org_directory },
        org_agenda_files = org_directory .. "/**/*",
        org_default_notes_file = org_roam_directory .. "/refile.org",
      })
    end
  },
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
