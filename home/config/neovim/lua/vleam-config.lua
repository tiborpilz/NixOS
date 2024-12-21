local lspconfig = require("lspconfig")
local lspconfig_configs = require("lspconfig.configs")
local lspconfig_util = require("lspconfig.util")

lspconfig.vleam.setup({
  cmd = { "npx", "vleam", "lsp" },
  filetypes = { "vue" },
  root_dir = function(fname)
    local gleam_root = lspconfig_util.root_pattern("gleam.toml")(fname)
    if not gleam_root then
      return false
    else
      local javascript_root = lspconfig_util.root_pattern("package.json")(fname)

      if not javascript_root then
        return false
      end

      return javascript_root
    end
  end,
  settings = {},
})

if not lspconfig_configs.vleam then
  lspconfig_configs.vleam = {
    default_config = {
      cmd = { "npx", "vleam", "lsp" },
      filetypes = { "vue" },
      root_dir = function(fname)
        local gleam_root = lspconfig_util.root_pattern("gleam.toml")(fname)
        print("Getting gleam root")
        if not gleam_root then
          return false
        else
          print("gleam root", gleam_root)
          local javascript_root = lspconfig_util.root_pattern("package.json")(fname)

          if not javascript_root then
            return false
          end

          print("javascript root", javascript_root)
          return javascript_root
        end
      end,
      settings = {},
    },
  }
end

local conform = require("conform")
local conform_util = require("conform.util")

if not conform.formatters.vleam then
  conform.formatters.vleam = {
    stdin = true,
    command = conform_util.from_node_modules("vleam"),
    args = { "format", "--stdin" },
    condition = function(_, ctx)
      local language_tree = vim.treesitter.get_parser(ctx.buf)

      local is_gleam = false

      language_tree:children(function(tree, lang)
        if lang == "gleam" then
          is_gleam = true
        end
      end)

      return is_gleam
    end,
  }
end
