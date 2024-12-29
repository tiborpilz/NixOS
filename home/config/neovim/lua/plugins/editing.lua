-- Formatting
vim.keymap.set("n", "<leader>cf", ":Format<CR>", { desc = "Format Buffer" })

-- Treesitter
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"

local function border(hl_name)
  return {
    { "┌", hl_name },
    { "─", hl_name },
    { "┐", hl_name },
    { "│", hl_name },
    { "┘", hl_name },
    { "─", hl_name },
    { "└", hl_name },
    { "│", hl_name },
  }
end

return {
  -- Comments
  {"tpope/vim-commentary"},

  -- Formatting
  {
    "stevearc/conform.nvim",
    dependencies = {
      {"williamboman/mason.nvim"},
      {"zapling/mason-conform.nvim"},
    },
    config = function()
      require("conform").setup({})
      -- Create a command for formatting the entire buffer
      vim.api.nvim_create_user_command("Format", function(args)
        local range = nil
        if args.count ~= -1 then
          local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
          range = {
            start = { args.line1, 0 },
            ["end"] = { args.line2, end_line:len() },
          }
        end
        require("conform").format({ async = true, lsp_format = "fallback", range = range })
      end, { range = true })

      -- Call that formatting command with `<leader>cf`
    end,
  },

  -- Autocompletion
  {
    "hrsh7th/nvim-cmp",
    config = function()
      local cmp = require("cmp")

      cmp.setup({
        sources = {
          {name = 'nvim_lsp'},
        },
        mapping = cmp.mapping.preset.insert({
          ['<Enter>'] = cmp.mapping.confirm({select = true}),
          ['<C-u>'] = cmp.mapping.scroll_docs(-4),
          ['<C-d>'] = cmp.mapping.scroll_docs(4),
        }),
      })
    end,
  },
  {"hrsh7th/cmp-nvim-lsp"},

  -- Run Snippets
  -- TODO: set up correctly
  {
    "michaelb/sniprun",
    build = "sh install.sh",
    config = function()
      require("sniprun").setup({})
    end,
  },

  -- Tree-like undo history
  {
    "mbbill/undotree",
    keys = {
      {
        "<leader>su",
        function()
          vim.cmd(":UndotreeToggle")
        end,
        desc = "Toggle Undo Tree",
      },
    },
    setup = function()
      vim.g.undotree_WindowLayout = 3 -- Right hand side
      vim.g.undotree_SetFocusWhenToggle = 1
    end,
  },

}
