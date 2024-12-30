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
  { "tpope/vim-commentary" },

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
        view = {
          entries = "native",
        },
        window = {
          completion = cmp.config.window.bordered({
            border = { " ", " ", " ", " ", " ", " ", " ", " " },
          }),
          documentation = cmp.config.window.bordered({
            border = { " ", " ", " ", " ", " ", " ", " ", " " },
            winhighlight = "NormalFloat:NormalFloat,FloatBorder:NormalFloat",
          }),
        },
        mapping = {
          ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
          ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
          ["<C-d>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.close(),
          ["<CR>"] = cmp.mapping.confirm({ select = false }),
        },
        sources = {
          { name = 'nvim_lsp' },
        },
        -- formatting = {
        --   fields = { "abbr", "kind", "menu" },
        --   expandable_indicator = false,
        --   format = require("lspkind").cmp_format({
        --     mode = "symbol_text",
        --     ellipsis_char = '...', -- when popup menu exceed maxwidth, the truncated part would show ellipsis_char instead (must define maxwidth first)
        --     show_labelDetails = true, -- show labelDetails in menu. Disabled by default

        --     -- The function below will be called before any actual modifications from lspkind
        --     -- so that you can provide more controls on popup customization. (See [#30](https://github.com/onsails/lspkind-nvim/pull/30))
        --     before = function (entry, vim_item)
        --       -- ...
        --       return vim_item
        --     end
        --   })
        -- },
      })
    end,
  },
  { "hrsh7th/cmp-nvim-lsp" },

  -- Tree-like undo history
  {
    "mbbill/undotree",
    keys = {
      {
        "<leader>ou",
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
