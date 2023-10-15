local mason_present, mason = pcall(require, 'mason')
local mason_lspconfig_present, mason_lspconfig = pcall(require, 'mason-lspconfig')

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

