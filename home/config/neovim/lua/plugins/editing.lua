-- Formatting
vim.keymap.set("n", "<leader>cf", ":Format<CR>", { desc = "Format Buffer" })

-- Limit height of floating windows

vim.opt.pumheight = 10

return {
  -- Comments
  {
    'numToStr/Comment.nvim',
    opts = {
      -- add any options here
    }
  },

  -- Formatting
  {
    "stevearc/conform.nvim",
    dependencies = {
      { "williamboman/mason.nvim" },
      { "zapling/mason-conform.nvim" },
    },
    config = function()
      require("conform").setup({
        formatters_by_ft = {
          typescript = { lsp_format = "last" },
        },
      })

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

  -- Completions
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      { "hrsh7th/cmp-nvim-lsp" },
      { "onsails/lspkind-nvim" },
      { "sudo-burger/cmp-org-roam", dependencies = { 'chipsenkbeil/org-roam.nvim' } },
      { "brenoprata10/nvim-highlight-colors" },
    },
    event = "VeryLazy",
    config = function()
      local cmp = require("cmp")
      local lspkind = require("lspkind")
      local highlight_colors = require("nvim-highlight-colors")

      highlight_colors.setup({})

      cmp.setup({
        window = {
          completion = cmp.config.window.bordered({
            border = "none",
            winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
          }),
          documentation = cmp.config.window.bordered({
            border = { " ", " ", " ", " ", " ", " ", " ", " " },
            winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
          }),
        },
        mapping = {
          ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
          ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
          ["<C-d>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-e>"] = cmp.mapping.close(),
          ["<CR>"] = cmp.mapping.confirm({ select = false }),
        },
        sources = {
          { name = 'nvim_lsp' },
        },
        formatting = {
          fields = { "abbr", "kind", "menu" },
          expandable_indicator = false,
          format = function(entry, item)
            -- Check Tailwind first
            local tw_item = require("tailwindcss-colorizer-cmp").formatter(entry, item)
            if tw_item.kind == "XX" then
              return tw_item
            end

            local color_item = highlight_colors.format(entry, { kind = item.kind })

            item = lspkind.cmp_format({ mode = "symbol", preset = "codicons"})(entry, item)
            if color_item.abbr1_hl_group then
              item.kind_hl_group = color_item.abbr_hl_group
              item.kind = color_item.abbr
            end
            return item
          end
        },
      })

      cmp.setup.filetype("org", {
        sources = {
          { name = "org-roam" },
        },
      })
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
    config = function()
      vim.g.undotree_WindowLayout = 1 -- Left hand side
      vim.g.undotree_SetFocusWhenToggle = 1
    end,
  },

  -- Zen Mode
  {
    "folke/zen-mode.nvim",
    keys = {
      {
        "<leader>Tz",
        function()
          require("zen-mode").toggle()
        end,
        desc = "Toggle Zen Mode",
      },
    },
    opts = {
      window = {
        backdrop = 0.95, -- Darken the background
        width = 120, -- Width of the zen window
      },
      plugins = {
        gitsigns = { enabled = false }, -- Disable gitsigns in zen mode
        twilight = { enabled = true }, -- Enable twilight for dimming inactive code
        tmux = { enabled = false }, -- Disable TMUX Status mode
        kitty = {
          enabled = true,
          font = "+4",
        }, -- Enable Kitty mode
      },
    },
  }
}
