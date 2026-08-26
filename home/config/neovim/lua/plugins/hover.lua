return {
  {
    'patrickpichler/hovercraft.nvim',

    dependencies = {
      { 'nvim-lua/plenary.nvim' },
    },

    -- this is the default config and can be skipped
    opts = function()
      return {
        providers = {
          providers = {
            {
              'LSP',
              require('hovercraft.provider.lsp.hover').new(),
            },
            {
              'Man',
              require('hovercraft.provider.man').new(),
            },
            -- {
            --   'Dictionary',
            --   require('hovercraft.provider.dictionary').new(),
            -- },
            {
              'Diagnostics',
              require('hovercraft.provider.diagnostics').new(),
            },
            {
              'TS Type',
              require('hovercraft-ts-type').new(),
            },
          }
        },

        window = {
          border = { ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' },

          -- enable this if you are a user of the MeanderingProgrammer/render-markdown.nvim plugin
          render_markdown_compat_mode = false,
        },

        keys = {
          { '<C-u>',   function() require('hovercraft').scroll({ delta = -4 }) end },
          { '<C-d>',   function() require('hovercraft').scroll({ delta = 4 }) end },
          { '<TAB>',   function() require('hovercraft').hover_next() end },
          { '<S-TAB>', function() require('hovercraft').hover_next({ step = -1 }) end },
          { '+',       function() require('hovercraft-ts-type').expand() end },
          { '-',       function() require('hovercraft-ts-type').collapse() end },
        }
      }
    end,

    config = function(_, opts)
      local hovercraft = require('hovercraft')
      hovercraft.setup(opts)

      -- hovercraft has no winhighlight option, so blend the blank border into
      -- the float the way noice and telescope do. window_config is populated
      -- before onshow fires.
      hovercraft.ui:register_onshow(function()
        local winnr = hovercraft.ui.window_config and hovercraft.ui.window_config.winnr
        if winnr and vim.api.nvim_win_is_valid(winnr) then
          vim.wo[winnr].winhighlight = 'FloatBorder:NormalFloat,NormalFloat:NormalFloat'
        end
      end)
    end,

    keys = {
      { "<leader>cg", function()
        local hovercraft = require("hovercraft")

        if hovercraft.is_visible() then
          hovercraft.enter_popup()
        else
          -- TS Type carries the same quickinfo the LSP hover is built from and
          -- can expand it, so lead with it where vtsls is attached. <TAB> still
          -- reaches LSP, which renders JSDoc tags far better.
          local vtsls = #vim.lsp.get_clients({ bufnr = 0, name = 'vtsls' }) > 0
          hovercraft.hover({ current_provider = vtsls and 'TS Type' or 'LSP' })
        end
      end },
      { "<leader>ce", function()
        local hovercraft = require("hovercraft")

        if hovercraft.is_visible() then
          hovercraft.enter_popup()
        else
          hovercraft.hover({ current_provider = 'Diagnostics' })
        end
      end }
    },
  },
  {
    'nemanjamalesija/ts-expand-hover.nvim',
    ft = { 'typescript', 'typescriptreact' },
    opts = {
      keymaps = {
        hover = '<leader>ct',
      },
    },
  },
  {
    'dnlhc/glance.nvim',
    cmd = 'Glance',
    keys = {
      { '<leader>cGD', '<CMD>Glance definitions<CR>' },
      { '<leader>cGR', '<CMD>Glance references<CR>' },
      { '<leader>cGT', '<CMD>Glance type_definitions<CR>' },
      { '<leader>cGI', '<CMD>Glance implementations<CR>' },
    },
  },
};

