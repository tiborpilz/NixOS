return {
  {
    "lewis6991/hover.nvim",
    event = "VeryLazy",
    config = function()
      require("hover").setup {
        init = function()
          require("hover.providers.lsp")
          require("hover.providers.fold_preview")
          require("hover.providers.diagnostic")
        end,
        preview_opts = {
          border = "solid",
        },
        title = false,
      }

      vim.keymap.set("n", "<leader>cg", function()
        require("hover").hover({ providers = { "LSP" }})
      end, { desc = "Glance at LSP" })

      vim.keymap.set("n", "<leader>ce", function()
        require("hover").hover({ providers = { "Diagnostics" }})
      end, { desc = "Diagnostic Hover" })
    end,
  },
}
