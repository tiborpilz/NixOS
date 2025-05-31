vim.cmd("autocmd FileType nerdtree map <buffer> <Tab> o")
return {
  -- {
  --   "preservim/nerdtree",
  --   keys = {
  --     {
  --       "<leader>op",
  --       function()
  --         vim.cmd(":NERDTreeToggle")
  --       end,
  --       desc = "Toggle NERDTree",
  --     },
  --   },
  -- },
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
        padding = 2,
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
          },
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
