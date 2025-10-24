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
            {
              'Dictionary',
              require('hovercraft.provider.dictionary').new(),
            },
            {
              'Diagnostics',
              require('hovercraft.provider.diagnostics').new(),
            },
          }
        },

        window = {
          border = 'single',

          -- enable this if you are a user of the MeanderingProgrammer/render-markdown.nvim plugin
          render_markdown_compat_mode = true,
        },

        keys = {
          { '<C-u>',   function() require('hovercraft').scroll({ delta = -4 }) end },
          { '<C-d>',   function() require('hovercraft').scroll({ delta = 4 }) end },
          { '<TAB>',   function() require('hovercraft').hover_next() end },
          { '<S-TAB>', function() require('hovercraft').hover_next({ step = -1 }) end },
        }
      }
    end,

    keys = {
      { "<leader>cg", function()
        local hovercraft = require("hovercraft")

        if hovercraft.is_visible() then
          hovercraft.enter_popup()
        else
          hovercraft.hover({ current_provider = 'LSP' })
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

