local lspconfig_present, lspconfig = pcall(require, 'lspconfig')
local mason_present, mason = pcall(require, 'mason')
local mason_lspconfig_present, mason_lspconfig = pcall(require, 'mason-lspconfig')

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

mason.setup()
mason_lspconfig.setup()

mason_lspconfig.setup_handlers {
  function (server_name) -- default handler
    require("lspconfig")[server_name].setup {}
  end
}

-- add snippet support
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

-- suppress error messages from lang servers
vim.notify = function(msg, log_level)
   if msg:match "exit code" then
      return
   end
   if log_level == vim.log.levels.ERROR then
      vim.api.nvim_err_writeln(msg)
   else
      vim.api.nvim_echo({ { msg } }, true, {})
   end
end
