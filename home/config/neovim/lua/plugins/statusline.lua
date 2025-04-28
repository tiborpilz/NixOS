vim.g.airline_left_sep = ""
vim.g.airline_right_sep = ""
vim.g.airline_theme = "base16"

return {
  -- Status line
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      -- TODO: This doesn't work, setting the background via nord theme override
      vim.cmd [[ hi lualine_c_normal guibg=NONE ]]

      local nord_theme_custom = require('lualine.themes.nord')
      nord_theme_custom.normal.c.bg = 'none'
      -- nord_theme_custom.normal.c.fg = 'darker_white'
      nord_theme_custom.inactive.c.bg = 'none'

      local mode_map = {
        ['NORMAL'] = 'N',
        ['O-PENDING'] = 'N?',
        ['INSERT'] = 'I',
        ['VISUAL'] = 'V',
        ['V-BLOCK'] = 'VB',
        ['V-LINE'] = 'VL',
        ['V-REPLACE'] = 'VR',
        ['REPLACE'] = 'R',
        ['COMMAND'] = '!',
        ['SHELL'] = 'SH',
        ['TERMINAL'] = 'T',
        ['EX'] = 'X',
        ['S-BLOCK'] = 'SB',
        ['S-LINE'] = 'SL',
        ['SELECT'] = 'S',
        ['CONFIRM'] = 'Y?',
        ['MORE'] = 'M',
      }

      require('lualine').setup({
        options = {
          theme = nord_theme_custom,
          section_separators = { left = ' ', right = ' ' },
          component_separators = { left = ' ', right = ' ' },
          globalstatus = true,
        },
        sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {},
          lualine_x = {
            'overseer',
            {
              'diagnostics',

              sources = { 'nvim_lsp', 'nvim_diagnostic' },
              sections = { 'error', 'warn', 'info', 'hint' },

              diagnostics_color = {
                error = 'DiagnosticError', -- Changes diagnostics' error color.
                info  = 'DiagnosticInfo',  -- Changes diagnostics' info color.
                hint  = 'DiagnosticHint',  -- Changes diagnostics' hint color.
              },
              -- symbols = { error = '', warn = '', info = '', hint = '' },
              symbols = { error = ' ', warn = ' ', info = ' ' },
              colored = true,          -- Displays diagnostics status in color if set to true.
              update_in_insert = true, -- Update diagnostics in insert mode.
              always_visible = false,  -- Show diagnostics even if there are none.
            },
            {
              'filename',
              path = 1,
              symbols = {
                modified = '',
                readonly = '',
                unnamed = '',
                newfile = '',
              },
            },
            {
              'filetype',
              colored = true,
              icon_only = true,
            },
          },
          lualine_y = {
            { 'b:gitsigns_head', icon = '' },
          },
          lualine_z = {
            {
              'mode',
              fmt = function(mode)
                return mode_map[mode] or mode
              end,
            },
          },
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {},
          lualine_x = {},
          lualine_y = { 'filename' },
          lualine_z = {},
        },
        extensions = {
          'nvim-tree',
          'mason',
          'lazy',
        },
      })
    end,
  },
}
