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
        },
        win_split_mode = "float",
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
  {
    "chipsenkbeil/org-roam.nvim",
    dependencies = {
      {
        "nvim-orgmode/orgmode",
      },
    },
    -- Nothing requires this module, so without a load trigger lazy.nvim never
    -- runs config and none of the roam keymaps get created.
    event = "VeryLazy",
    config = function()
      local org_directory = "~/org"
      local org_roam_directory = org_directory .. "/roam"

      require("org-roam").setup({
        bindings = {
          prefix = "<leader>nr",
        },
        extensions = {
          dailies = {
            directory = "daily",
            bindings = {
              goto_prev_date = "<prefix>db",
              goto_date = "<prefix>dd",
              capture_date = "<prefix>dD",
              goto_next_date = "<prefix>df",
              goto_tomorrow = "<prefix>dm",
              capture_tomorrow = "<prefix>dM",
              capture_today = "<prefix>dn",
              goto_today = "<prefix>dt",
              goto_yesterday = "<prefix>dy",
              capture_yesterday = "<prefix>dY",
              find_directory = "<prefix>d-",
            },
            templates = {
              d = {
                description = "default",
                template = "* %?",
                target = "%<%Y-%m-%d>.org",
              },
            },
          },
        },
        directory = org_roam_directory,
        -- optional
        org_files = { org_roam_directory },
        org_agenda_files = org_directory .. "/**/*",
        org_default_notes_file = org_roam_directory .. "/refile.org",
      })

      -- Doom binds capture-today twice; the plugin's config takes one lhs per action.
      vim.keymap.set("n", "<leader>nrdT", function()
        require("org-roam").ext.dailies.capture_today()
      end, { desc = "Capture today's note" })
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
}
