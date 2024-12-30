return {
  -- Mason for installing LSP servers
  {
    "williamboman/mason.nvim",
    opts = {
      ui = {
        border = "none",
      },
    },
    config = function()
      require("mason").setup()
    end,
  },

  -- LSP config
  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      local mason_lspconfig = require("mason-lspconfig")

      mason_lspconfig.setup({
        ensure_installed = { "lua_ls" },
      })

      mason_lspconfig.setup_handlers {
        function(server_name) -- default handler
          require("lspconfig")[server_name].setup {}
        end,

        -- need to set up lua_ls so it stops whining about global vim
        ["lua_ls"] = function()
          return require("lspconfig")["lua_ls"].setup({
            settings = {
              Lua = {
                diagnostics = {
                  globals = { "vim" },
                },
              },
            }
          })
        end
      }
    end,
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
      local lspconfig = require("lspconfig")

      lspconfig.nil_ls.setup({})
      lspconfig.gleam.setup({})
      lspconfig.volar.setup({
        cmd = { "vue-language-server", "--stdio" },
        init_options = {
          vue = {
            hybridMode = false,
          },
          typescript = {
            tsdk = vim.fn.stdpath("data") .. "/mason/packages/typescript/node_modules/typescript/lib",
          },
        },
      })

      lspconfig.ts_ls.setup {
        init_options = {
          plugins = {
            {
              name = "@vue/typescript-plugin",
              configNamespace = "typescript",
              enableForWorkspaceTypeScriptVersions = true,
              location = require("mason-registry").get_package("vue-language-server"):get_install_path() .. "/node_modules/@vue/language-server/node_modules/@vue/typescript-plugin/",
              languages = { "javascript", "typescript", "vue" },
            },
          },
        },
        filetypes = {
          "javascript",
          "javascriptreact",
          "typescript",
          "typescriptreact",
          "vue",
        },
      }

      -- suppress error messages from lang servers
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.notify = function(msg, log_level, _)
        if msg:match "exit code" then
          return
        end
        if log_level == vim.log.levels.ERROR then
          vim.api.nvim_err_writeln(msg)
        else
          vim.api.nvim_echo({ { msg } }, true, {})
        end
      end

      -- Set border for LSP Hover
      vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
        vim.lsp.handlers.hover,
        { border = { " ", " ", " ", " ", " ", " ", " ", " " } }
      )

      -- Set diagnostic sign
      -- Change diagnostic signs.

      -- LSP Keymaps
      vim.keymap.set('n', '<Leader>cg', '<cmd>lua vim.lsp.buf.hover()<cr>', { desc = 'Show hover information' })
      -- vim.keymap.set('n', '<Leader>ca', '<cmd>lua vim.lsp.buf.code_action()<cr>', { desc = 'Show code actions' })
      vim.keymap.set('n', '<Leader>cr', '<cmd>lua vim.lsp.buf.rename()<cr>', { desc = 'Rename symbol' })
      vim.keymap.set({ 'n', 'x' }, '<Leader>cf', '<cmd>lua vim.lsp.buf.format({async = true})<cr>',
        { desc = 'Format code' })
      -- Hide diagnostic float per default
      vim.diagnostic.config({ virtual_text = false })
      -- Bind diagnostic to <Leader> c e
      vim.keymap.set('n', '<Leader>ce', '<cmd>lua vim.diagnostic.open_float(nil, {focus=false})<CR>',
        { desc = 'Open Diagnostic Float', noremap = true, silent = true })

      vim.keymap.set('n', '<Leader>cd', '<cmd>lua vim.lsp.buf.definition()<cr>', { desc = 'Go to definition' })
      vim.keymap.set('n', '<Leader>cD', '<cmd>lua vim.lsp.buf.declaration()<cr>', { desc = 'Go to declaration' })
      vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', { desc = 'Go to definition' })
      vim.keymap.set('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', { desc = 'Go to declaration' })
    end,
  },

  -- LSP Actions Preview
  {
    "aznhe21/actions-preview.nvim",
    config = function()
      vim.keymap.set({ "v", "n" }, "<Leader>ca", require("actions-preview").code_actions, { desc = "Code Actions" })
    end,
    setup = function()
      require("actions-preview").setup({
        telescope = {
          defaults = {
            border = {
              prompt = { 0, 0, 0, 0 },
              results = { 0, 0, 0, 0 },
              preview = { 0, 0, 0, 0 },
            },
            layout_config = {
              prompt_position = "top",
              width = 0.5,
              height = 0.5,
            },
          },
        },
      })
    end,
  },

  -- LSP Signatures
  {
    "ray-x/lsp_signature.nvim",
    setup = function()
      require('lsp_signature').setup({
        bind = true,
        floating_window = false,
        doc_lines = 2,
        use_lspsaga = true,
        padding = ' ',
        shadow_blend = 36,
        floating_window_above_cur_line = true,
        handler_opts = {
          border = "none"
        }
      })

      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
          local bufnr = args.buf
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          if vim.tbl_contains({ 'null-ls' }, client.name) then -- blacklist lsp
            return
          end
          require("lsp_signature").on_attach({}, bufnr)
        end,
      })
    end,
  },

  -- Treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    config = function()
      require('nvim-treesitter.configs').setup({
        ignore_install = {},

        ensure_installed = {
          "typescript",
          "javascript",
          "vue",
          "tsx",
          "rust",
          "lua",
          "vim"
        },

        -- Install parsers asynchronously...
        sync_install = false,
        -- ...but make sure they'll get installed when they're missing (in the current buffer)
        auto_install = true,

        highlight = { enable = true },
        incremental_selection = { enable = true },
        textobjects = { enable = true },
      })
    end,
  },

  -- DAP - Debug Adapter Protocol
  {
    "jay-babu/mason-nvim-dap.nvim",
    config = function()
      local mason_nvim_dap = require("mason-nvim-dap")

      mason_nvim_dap.setup({
        ensure_installed = {
          "node2",
          "firefox",
        },
        automatic_installation = true,
        handlers = {
          function(config)
            mason_nvim_dap.default_setup(config)
          end,
        },
      })
    end,
  },
  { "mfussenegger/nvim-dap" },
  {
    "theHamsta/nvim-dap-virtual-text",
    config = function()
      require("nvim-dap-virtual-text").setup({
        commented = true,
      })
    end,
  },
  { "nvim-telescope/telescope-dap.nvim" },

  --- Terraform
  { "hashivim/vim-terraform" },

  --- Nix
  { "LnL7/vim-nix" },

  --- Lua
  {
    "folke/lazydev.nvim",
    ft = "lua", -- only load on lua files
    opts = {
      library = {
        -- See the configuration section for more details
        -- Load luvit types when the `vim.uv` word is found
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  },
}
