-- Hide diagnostic float per default
vim.diagnostic.config({ virtual_text = false })

-- Bind diagnostic to <Leader> c d
vim.api.nvim_set_keymap('n', '<Leader>cd', '<cmd>lua vim.diagnostic.open_float(nil, {focus=false})<CR>',
  { noremap = true, silent = true })

return {
  -- Mason for installing LSP servers
  {
    "williamboman/mason.nvim",
    opts = {
      ui = {
        border = "single",
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

                workspace = {
                  library = {
                    [vim.fn.expand("$VIMRUNTIME/lua")] = true,
                    [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
                  },
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

      -- LSP Keymaps
      vim.keymap.set('n', '<Leader>cg', '<cmd>lua vim.lsp.buf.hover()<cr>', { desc = 'Show hover information' })
      vim.keymap.set('n', '<Leader>ca', '<cmd>lua vim.lsp.buf.code_action()<cr>', { desc = 'Show code actions' })
      vim.keymap.set('n', '<Leader>cr', '<cmd>lua vim.lsp.buf.rename()<cr>', { desc = 'Rename symbol' })
      vim.keymap.set({ 'n', 'x' }, '<Leader>cf', '<cmd>lua vim.lsp.buf.format({async = true})<cr>',
        { desc = 'Format code' })
      vim.keymap.set('n', '<Leader>cd', '<cmd>lua vim.lsp.buf.definition()<cr>', { desc = 'Go to definition' })
      vim.keymap.set('n', '<Leader>cD', '<cmd>lua vim.lsp.buf.declaration()<cr>', { desc = 'Go to declaration' })
      vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', { desc = 'Go to definition' })
      vim.keymap.set('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', { desc = 'Go to declaration' })
      vim.keymap.set('n', '<Leader>cx', '<cmd>lua vim.diagnostic.open_float(nil, {focus=false})<CR>',
        { noremap = true, silent = true })
    end,
  },

  -- LSP Lightbulb (Code Actions)
  { "kosayoda/nvim-lightbulb" },

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
        shadow_guibg = "green",
        floating_window_above_cur_line = false,
        handler_opts = {
          border = "single"
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
      require'nvim-treesitter.configs'.setup {

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
      }
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

  -- Tests
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter",
      "nvim-neotest/neotest-jest",
    },
    config = function()
      local neotest = require("neotest")
      local neotest_jest = require("neotest-jest")

      neotest.setup({
        status = {
          virtual_text = true,
        },
        output = {
          open_on_run = true,
        },
        config = function(_, opts)
          local neotest_ns = vim.api.nvim_create_namespace("neotest")
          vim.diagnostic.config({
            virtual_text = {
              format = function(diagnostic)
                -- Replace newline and tab characters with space for more compact diagnostics
                local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
                return message
              end,
            },
          }, neotest_ns)
        end,
        adapters = {
          neotest_jest({}),
        },
      })

      vim.keymap.set(
        'n',
        '<leader>tr',
        '<cmd>lua require("neotest").run.run()<cr>',
        { desc = 'Run nearest test' }
      )

      vim.keymap.set(
        'n',
        '<leader>tR',
        '<cmd>lua require("neotest").run.run(vim.fn.expand("%"))<cr>',
        { desc = 'Run all tests in file' }
      )

      vim.keymap.set(
        'n',
        '<leader>tl',
        '<cmd>lua require("neotest").run.run_last()<cr>',
        { desc = 'Run last test' }
      )

      vim.keymap.set(
        'n',
        '<leader>tw',
        '<cmd>lua require("neotest").watch.toggle()<cr>',
        { desc = 'Watch nearest test' }
      )

      vim.keymap.set(
        'n',
        '<leader>tW',
        '<cmd>lua require("neotest").watch.toggle(vim.fn.expand("%"))<cr>',
        { desc = 'Watch all tests in file' }
      )

      vim.keymap.set(
        'n',
        '<leader>tS',
        '<cmd>lua require("neotest").run.stop()<cr>',
        { desc = 'Stop running tests' }
      )

      vim.keymap.set(
        'n',
        '<leader>ta',
        '<cmd>lua require("neotest").run.attach()<cr>',
        { desc = 'Attach to test runner' }
      )

      vim.keymap.set(
        'n',
        '<leader>ts',
        '<cmd>lua require("neotest").summary.toggle()<cr>',
        { desc = 'Toggle test summary' }
      )

      vim.keymap.set(
        'n',
        '<leader>to',
        '<cmd>lua require("neotest").output.open({ enter = true, auto_close = true })<cr>',
        { desc = 'Open test output' }
      )

      vim.keymap.set(
        'n',
        '<leader>tO',
        '<cmd>lua require("neotest").output_panel.toggle()<cr>',
        { desc = 'Toggle test output panel' }
      )
    end,
  },

  -- Run Snippets
  -- TODO: set up correctly
  {
    "michaelb/sniprun",
    build = "sh install.sh",
    config = function()
      require("sniprun").setup({})

      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities.textDocument.completion.completionItem.snippetSupport = true
    end,
  },

}
