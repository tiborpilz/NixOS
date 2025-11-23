-- LSP Keymaps
-- vim.keymap.set('n', '<Leader>cg', '<cmd>lua vim.lsp.buf.hover()<cr>', { desc = 'Show hover information' })
-- vim.keymap.set('n', '<Leader>ca', '<cmd>lua vim.lsp.buf.code_action()<cr>', { desc = 'Show code actions' })
vim.keymap.set('n', '<Leader>cr', '<cmd>lua vim.lsp.buf.rename()<cr>', { desc = 'Rename symbol' })
vim.keymap.set({ 'n', 'x' }, '<Leader>cf', '<cmd>lua vim.lsp.buf.format({async = true})<cr>',
  { desc = 'Format code' })
-- Hide diagnostic float per default
vim.diagnostic.config({ virtual_text = false })
-- Bind diagnostic to <Leader> c e
-- vim.keymap.set('n', '<Leader>ce', '<cmd>lua vim.diagnostic.open_float(nil, {focus=false})<CR>',
--   { desc = 'Open Diagnostic Float', noremap = true, silent = true })

vim.keymap.set('n', '<Leader>cd', '<cmd>lua vim.lsp.buf.definition()<cr>', { desc = 'Go to definition' })
vim.keymap.set('n', '<Leader>cD', '<cmd>lua vim.lsp.buf.declaration()<cr>', { desc = 'Go to declaration' })
vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', { desc = 'Go to definition' })
vim.keymap.set('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', { desc = 'Go to declaration' })

-- Set border for LSP Hover
-- vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
--   vim.lsp.handlers.hover,
--   { border = { " ", " ", " ", " ", " ", " ", " ", " " } }
-- )
--
--


-- Detect Gitlab CI Yaml files so that gitlab-ci-ls can use them
vim.filetype.add({
  pattern = {
    ['%.gitlab%-ci%.ya?ml'] = 'yaml.gitlab',
  },
})

return {
  -- -- Single languages
  -- Rust
  {
    'mrcjkb/rustaceanvim',
    version = '^6',
    lazy = false,
    config = function()
      vim.g.rustaceanvim = {
        -- Plugin configuration
        tools = {
        },
        -- LSP configuration
        server = {
          on_attach = function(client, bufnr)
            -- you can also put keymaps in here
          end,
          default_settings = {
            -- rust-analyzer language server configuration
            ['rust-analyzer'] = {
              cargo = {
                allFeatures = true,
              },
            },
          },
        },
        -- DAP configuration
        -- dap = {
        -- },
      }
    end
  },
  -- LSP config
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    event = "VeryLazy",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "neovim/nvim-lspconfig",
      -- "jay-babu/mason-nvim-dap.nvim",
      -- "rcarriga/nvim-dap-ui",
      -- "mfussenegger/nvim-dap",
      "nvim-neotest/nvim-nio",
      "nvimtools/none-ls.nvim",
      "nvimtools/none-ls-extras.nvim",
      "zapling/mason-lock.nvim",
      "folke/lazydev.nvim",
      "artemave/workspace-diagnostics.nvim",
    },
    config = function()
      require("mason").setup({})

      require("mason-tool-installer").setup({
        -- a list of all tools you want to ensure are installed upon start
        ensure_installed = {
          "ts_ls",
          "volar",
          "eslint",
          "eslint_d",
        },
        auto_update = true,  -- Default: false
        run_on_start = true, -- Default: true
        start_delay = 1000,  -- 1 second delay ( Default: 0 )
        debounce_hours = 1,  -- at least 1 hour between attempts to install/update
      })

      local mason_packages = vim.fn.stdpath("data") .. "/mason/packages"
      local volar_path = mason_packages .. "/vue-language-server/node_modules/@vue/language-server"

      require("mason-lock").setup({
        lockfile_path = vim.fn.stdpath("config") .. "/mason-lock.json", -- (default)
      })

      require("lazydev").setup({
        enabled = true,
        runtime = vim.env.RUNTIME,
        library = vim.api.nvim_get_runtime_file("", true),
        integrations = {
          lspconfig = true,
          cmp = true,
          coq = false,
        },
        debug = false,
      })

      require("mason-lspconfig").setup({
        automatic_installation = true,
        ensure_installed = {},

        handlers = {
          -- function(server_name) -- default handler
          --   require("lspconfig")[server_name].setup({})
          -- end,

          -- ["volar"] = function()
          --   vim.lsp.config("volar", {
          --     init_options = {
          --       vue = {
          --         hybridMode = false, -- Use false as in the second block, change to true if needed
          --       },
          --       typescript = {
          --         tsdk = vim.fn.stdpath("data") .. "/mason/packages/typescript-language-server/node_modules/typescript/lib",
          --         -- Uncomment below if you want to use project-local typescript
          --         -- tsdk = vim.fn.getcwd() .. "/node_modules/typescript/lib",
          --       },
          --     },
          --     settings = {
          --       typescript = {
          --         inlayHints = {
          --           enumMemberValues = {
          --             enabled = true,
          --           },
          --           functionLikeReturnTypes = {
          --             enabled = true,
          --           },
          --           propertyDeclarationTypes = {
          --             enabled = true,
          --           },
          --           parameterTypes = {
          --             enabled = true,
          --             suppressWhenArgumentMatchesName = true,
          --           },
          --           variableTypes = {
          --             enabled = true,
          --           },
          --         },
          --       },
          --     },
          --     -- NOTE: Uncomment to enable volar in file types other than vue.
          --     -- (Similar to Takeover Mode)
          --     filetypes = { "vue", "javascript", "typescript", "javascriptreact", "typescriptreact", "json" },
          --
          --     -- NOTE: Uncomment to restrict Volar to only Vue/Nuxt projects. This will enable Volar to work alongside other language servers (tsserver).
          --     -- root_dir = require("lspconfig").util.root_pattern(
          --       --   "vue.config.js",
          --       --   "vue.config.ts",
          --       --   "nuxt.config.js",
          --       --   "nuxt.config.ts"
          --       -- ),
          --     })
          --   end,
          --
          --
            ["ts_ls"] = function()
              vim.lsp.config("ts_ls", {
                filetypes = { "typescript", "javascript", "javascriptreact", "typescriptreact", "vue" },
                init_options = {
                  plugins = {
                    {
                      name = "@vue/typescript-plugin",
                      location = volar_path,
                      languages = { "vue" },
                    },
                  },
                },
                settings = {
                  typescript = {
                    inlayHints = {
                      includeInlayParameterNameHints = "all",
                      includeInlayParameterNameHintsWhenArgumentMatchesName = true,
                      includeInlayFunctionParameterTypeHints = true,
                      includeInlayVariableTypeHints = true,
                      includeInlayVariableTypeHintsWhenTypeMatchesName = true,
                      includeInlayPropertyDeclarationTypeHints = true,
                      includeInlayFunctionLikeReturnTypeHints = true,
                      includeInlayEnumMemberValueHints = true,
                    },
                  },
                },
              })

              vim.lsp.config("ts_ls", {
                filetypes = { "typescript", "javascript", "javascriptreact", "typescriptreact", "vue" },
                init_options = {
                  plugins = {
                    {
                      name = "@vue/typescript-plugin",
                      location = volar_path,
                      languages = { "vue" },
                    },
                  },
                },
                settings = {
                  typescript = {
                    inlayHints = {
                      includeInlayParameterNameHints = "all",
                      includeInlayParameterNameHintsWhenArgumentMatchesName = true,
                      includeInlayFunctionParameterTypeHints = true,
                      includeInlayVariableTypeHints = true,
                      includeInlayVariableTypeHintsWhenTypeMatchesName = true,
                      includeInlayPropertyDeclarationTypeHints = true,
                      includeInlayFunctionLikeReturnTypeHints = true,
                      includeInlayEnumMemberValueHints = true,
                    },
                  },
                },
              })
          end,


          ["lua_ls"] = function()
            vim.lsp.config("lua", {
              on_init = function(client)
                local path = client.workspace_folders[1].name
                if vim.loop.fs_stat(path .. "/.luarc.json") or vim.loop.fs_stat(path .. "/.luarc.jsonc") then
                  return
                end

                client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
                  runtime = {
                    -- Tell the language server which version of Lua you're using
                    -- (most likely LuaJIT in the case of Neovim)
                    version = "LuaJIT",
                  },
                  -- Make the server aware of Neovim runtime files
                  workspace = {
                    checkThirdParty = false,
                    library = {
                      -- vim.env.VIMRUNTIME,
                      -- Depending on the usage, you might want to add additional paths here.
                      -- "${3rd}/luv/library",
                      -- "${3rd}/busted/library",
                      -- "~/.local/share/nvim/lazy",
                    },
                  },
                  hint = {
                    enable = true,
                    arrayIndex = "Auto",
                    await = true,
                    paramName = "All",
                    paramType = true,
                    semicolon = "SameLine",
                    setType = true,
                  },
                })
              end,
              settings = {
                Lua = {},
              },
            })
          end,
        },
      })

      --
      vim.lsp.config("gleam", require("lspconfig.configs.gleam"))
      vim.lsp.config("sourcekit", require("lspconfig.configs.sourcekit"))
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
  -- LSP File Actions
  {
    "antosha417/nvim-lsp-file-operations",
    event = "VeryLazy",
    dependencies = {
      "nvim-lua/plenary.nvim",
      -- Uncomment whichever supported plugin(s) you use
      "nvim-tree/nvim-tree.lua",
      -- "nvim-neo-tree/neo-tree.nvim",
      -- "simonmclean/triptych.nvim"
    },
    config = function()
      require("lsp-file-operations").setup()
    end,
  },
  -- LSP Symbol view
  {
    "stevearc/aerial.nvim",
    config = function()
      require("aerial").setup({
        attach_mode = "global",
        backends = { "lsp", "markdown" },
        filter_kind = false,
        show_guides = true,
        layout = {
          -- margin
          win_opts = {
            winblend = 10,
            winhighlight = "Normal:Normal,FloatBorder:FloatBorder,CursorLine:Visual,Search:None",
          },
        },
        lsp = {
          diagnostics_trigger_update = true,
        }
      })
    end,
    keys = {
      { "<leader>cs", "<cmd>AerialToggle!<cr>", desc = "Show LSP Symbols" },
    },
  },
  -- LSP Glances
  {
    'dnlhc/glance.nvim',
    cmd = 'Glance'
  },

  -- Formatting via LSP/Mason
  {
    "jay-babu/mason-null-ls.nvim",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "williamboman/mason.nvim",
      "nvimtools/none-ls.nvim",
    },
    config = function()
      require("mason-null-ls").setup({})
    end,
  },

  -- Treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    config = function()
      require('nvim-treesitter.configs').setup({
        modules = {},
        ignore_install = {},

        ensure_installed = {
          "typescript",
          "javascript",
          "vue",
          "tsx",
          "rust",
          "lua",
          "vim",
          "gleam"
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
  -- Treesitter context
  {
    "nvim-treesitter/nvim-treesitter-context",
    config = function()
      require("treesitter-context").setup()
    end,
  },

  -- DAP - Debug Adapter Protocol
  -- {
  --   "jay-babu/mason-nvim-dap.nvim",
  --   config = function()
  --     local mason_nvim_dap = require("mason-nvim-dap")
  --
  --     mason_nvim_dap.setup({
  --       ensure_installed = {
  --         "node2",
  --         "firefox",
  --       },
  --       automatic_installation = true,
  --       handlers = {
  --         function(config)
  --           mason_nvim_dap.default_setup(config)
  --         end,
  --       },
  --     })
  --   end,
  -- },
  -- { "mfussenegger/nvim-dap" },
  -- {
  --   "theHamsta/nvim-dap-virtual-text",
  --   opts = {
  --     commented = true,
  --   },
  -- },
  -- { "nvim-telescope/telescope-dap.nvim" },
  --
  -- Web dev stuff
  {
    "roobert/tailwindcss-colorizer-cmp.nvim",
    -- optionally, override the default options:
    config = function()
      require("tailwindcss-colorizer-cmp").setup()
    end
  },

  --  Increase TS error messages
  { 'dmmulroy/ts-error-translator.nvim' },


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
