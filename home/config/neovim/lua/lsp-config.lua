local lspconfig_present, lspconfig = pcall(require, 'lspconfig')
local mason_present, mason = pcall(require, 'mason')
local mason_lspconfig_present, mason_lspconfig = pcall(require, 'mason-lspconfig')
local mason_nvim_dap_present, mason_nvim_dap = pcall(require, 'mason-nvim-dap')

if not (lspconfig_present) then
  return
end

if not (mason_present) then
  print('mason not found')
  return
end

if not (mason_lspconfig_present) then
  print('mason-lspconfig not found')
  return
end

if not (mason_nvim_dap_present) then
  print('mason-nvim-dap not found')
  return
end

mason.setup()
mason_nvim_dap.setup()

mason_lspconfig.setup({
  ensure_installed = {
    "lua_ls",
  },
})

mason_lspconfig.setup_handlers {
  function(server_name)  -- default handler
    require("lspconfig")[server_name].setup {}
  end,

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

lspconfig.nil_ls.setup({})

-- Languages
lspconfig.gleam.setup({})

-- Vue
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

-- Typescript
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


-- Hide diagnostic float per default
vim.diagnostic.config({ virtual_text = false })

-- add snippet support
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

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

vim.keymap.set('n', '<Leader>cg', '<cmd>lua vim.lsp.buf.hover()<cr>', { desc = 'Show hover information' })
vim.keymap.set('n', '<Leader>ca', '<cmd>lua vim.lsp.buf.code_action()<cr>', { desc = 'Show code actions' })
vim.keymap.set('n', '<Leader>cr', '<cmd>lua vim.lsp.buf.rename()<cr>', { desc = 'Rename symbol' })
vim.keymap.set({ 'n', 'x' }, '<Leader>cf', '<cmd>lua vim.lsp.buf.format({async = true})<cr>', { desc = 'Format code' })
vim.keymap.set('n', '<Leader>cd', '<cmd>lua vim.lsp.buf.definition()<cr>', { desc = 'Go to definition' })
vim.keymap.set('n', '<Leader>cD', '<cmd>lua vim.lsp.buf.declaration()<cr>', { desc = 'Go to declaration' })
vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', { desc = 'Go to definition' })
vim.keymap.set('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', { desc = 'Go to declaration' })
vim.keymap.set('n', '<Leader>cx', '<cmd>lua vim.diagnostic.open_float(nil, {focus=false})<CR>', { noremap = true, silent = true })
