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
      local lsp_symbols = require("neotree-lsp-symbols")
      local events = require("neo-tree.events")
      vim.api.nvim_create_autocmd("CursorMoved", {
        group = vim.api.nvim_create_augroup("NeoTreeLspSymbolsFollowCursor", { clear = true }),
        callback = function(args)
          if vim.bo[args.buf].buftype ~= "" then return end
          if vim.bo[args.buf].filetype == "neo-tree" then return end
          lsp_symbols.follow_cursor_debounced()
        end,
      })
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
        event_handlers = {
          {
            event = events.AFTER_RENDER,
            handler = function(state)
              if state.name == "filesystem" then
                lsp_symbols.after_render(state)
              end
            end,
          },
        },
        window = {
          position = "right",
          mapping_options = {
            noremap = true,
            nowait = true,
          },
          mappings = {
            ["<Tab>"] = "toggle_symbols",
            ["<space>"] = "none",
            ["<cr>"] = "open_symbol",
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
          commands = {
            toggle_symbols = lsp_symbols.toggle_symbols,
            open_symbol = lsp_symbols.open_symbol,
          },
          components = {
            lsp_kind_icon = lsp_symbols.kind_icon_component,
          },
          renderers = {
            lsp_symbol = {
              { "indent" },
              { "lsp_kind_icon" },
              { "name" },
            },
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
