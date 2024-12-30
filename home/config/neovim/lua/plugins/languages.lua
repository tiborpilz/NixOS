return {
  -- Mason for installing LSP servers
  -- {
  --   "williamboman/mason.nvim",
  --   opts = {
  --     ui = {
  --       border = "none",
  --     },
  --   },
  --   config = function()
  --     require("mason").setup()
  --   end,
  -- },

  -- LSP config
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    event = "VeryLazy",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "neovim/nvim-lspconfig",
      "jay-babu/mason-null-ls.nvim",
      "jay-babu/mason-nvim-dap.nvim",
      "rcarriga/nvim-dap-ui",
      "mfussenegger/nvim-dap",
      "nvim-neotest/nvim-nio",
      "nvimtools/none-ls.nvim",
      "nvimtools/none-ls-extras.nvim",
      "zapling/mason-lock.nvim",
      "folke/lazydev.nvim",
    },
    config = function()
      require("mason").setup({})

      require("mason-tool-installer").setup({
        -- a list of all tools you want to ensure are installed upon start
        ensure_installed = {
          "ts_ls",
          "volar",
        },
        auto_update = true, -- Default: false
        run_on_start = true, -- Default: true
        start_delay = 1000, -- 1 second delay ( Default: 0 )
        debounce_hours = 1, -- at least 1 hour between attempts to install/update
      })

      require("mason-lock").setup({
        lockfile_path = vim.fn.stdpath("config") .. "/mason-lock.json", -- (default)
      })

      require("mason-null-ls").setup({
        ensure_installed = {},
        automatic_installation = { exclude = { "stylua", "gitsigns" } },
        handlers = {},
      })

      require("null-ls").setup({
        sources = {
          -- Anything not supported by mason.
          require("null-ls").builtins.formatting.stylua,
          require("null-ls").builtins.code_actions.gitsigns,
          require("null-ls").builtins.diagnostics.zsh,
          require("null-ls").builtins.diagnostics.selene,
          -- Anythng not supported by none-ls.
          require("none-ls.diagnostics.eslint_d"),
          require("none-ls.formatting.eslint_d").with({ timeout = 5000 }),
          require("none-ls.code_actions.eslint_d"),
        },
        -- Format on save using null-ls instead of lsp server.
        on_attach = function(current_client, bufnr)
          if current_client.supports_method("textDocument/formatting") then
            vim.api.nvim_create_autocmd("BufWritePre", {
              group = augroup,
              buffer = bufnr,
              callback = function()
                vim.lsp.buf.format({
                  filter = function(client)
                    return client.name == "null-ls"
                  end,
                  bufnr = bufnr,
                })
              end,
            })
          end
        end,
      })

      require("lazydev").setup({
        library = vim.api.nvim_get_runtime_file("", true),
      })

      require("mason-lspconfig").setup({
        automatic_installation = true,

        handlers = {
          function(server_name) -- default handler
            require("lspconfig")[server_name].setup({})
          end,

          ["volar"] = function()
            require("lspconfig").volar.setup({
              -- NOTE: Uncomment to enable volar in file types other than vue.
              -- (Similar to Takeover Mode)

              -- filetypes = { "vue", "javascript", "typescript", "javascriptreact", "typescriptreact", "json" },

              -- NOTE: Uncomment to restrict Volar to only Vue/Nuxt projects. This will enable Volar to work alongside other language servers (tsserver).

              -- root_dir = require("lspconfig").util.root_pattern(
              --   "vue.config.js",
              --   "vue.config.ts",
              --   "nuxt.config.js",
              --   "nuxt.config.ts"
              -- ),
              init_options = {
                vue = {
                  hybridMode = false,
                },
                -- NOTE: This might not be needed. Uncomment if you encounter issues.

                -- typescript = {
                --   tsdk = vim.fn.getcwd() .. "/node_modules/typescript/lib",
                -- },
              },
              settings = {
                typescript = {
                  inlayHints = {
                    enumMemberValues = {
                      enabled = true,
                    },
                    functionLikeReturnTypes = {
                      enabled = true,
                    },
                    propertyDeclarationTypes = {
                      enabled = true,
                    },
                    parameterTypes = {
                      enabled = true,
                      suppressWhenArgumentMatchesName = true,
                    },
                    variableTypes = {
                      enabled = true,
                    },
                  },
                },
              },
            })
          end,

          ["ts_ls"] = function()
            local mason_packages = vim.fn.stdpath("data") .. "/mason/packages"
            local volar_path = mason_packages .. "/vue-language-server/node_modules/@vue/language-server"

            require("lspconfig").ts_ls.setup({
              -- NOTE: To enable hybridMode, change HybrideMode to true above and uncomment the following filetypes block.

              -- filetypes = { "typescript", "javascript", "javascriptreact", "typescriptreact", "vue" },
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
            require("lspconfig").lua_ls.setup({
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
