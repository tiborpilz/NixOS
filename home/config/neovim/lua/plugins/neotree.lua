vim.cmd("autocmd FileType nerdtree map <buffer> <Tab> o")
return {
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
      -- {"3rd/image.nvim", opts = {}}, -- Optional image support in preview window: See `# Preview Mode` for more information
    },
    lazy = false, -- neo-tree will lazily load itself
    ---@module "neo-tree"
    ---@type neotree.Config?
    config = function()
      require("neo-tree").setup({
        default_component_configs = {
          container = {
            enable_character_fade = true,
          },
        },
        padding = 1,
        sources = {
          "filesystem",
          "buffers",
          "git_status",
          "document_symbols",
        },
        window = {
          mapping_options = {
            noremap = true,
            nowait = true,
          },
          mappings = {
            ["<Tab>"] = "open_with_window_picker",
            ["<space>"] = {
              "none", -- disable space key in the tree
            },
            ["<cr>"] = "open",
            ["ov"] = "open_vsplit",
            ["oh"] = "open_split",
            ["ot"] = "open_tabnew",
            -- use vim-native search in neotree buffer
            ["/"] = "noop",
          },
        },
        filesystem = {
          filtered_items = {
            hide_dotfiles = false, -- show dotfiles
            hide_gitignored = false, -- show gitignored files
          },
          follow_current_file = {
            enabled = true,
            leave_dirs_open = false,
          },
          use_libuv_file_watcher = true, -- use libuv for file watching
        },
      })
    end,
    keys = {
      {
        "<leader>op",
        function()
          require("neo-tree.command").execute({ toggle = true, dir = vim.loop.cwd() })
        end,
        desc = "Toggle NeoTree",
      },
      {
        "<leader>oP",
        function()
          require("neo-tree.command").execute({ toggle = true, dir = vim.loop.cwd(), source = "filesystem" })
        end,
        desc = "Toggle NeoTree Filesystem",
      },
    },
  }
}
