vim.g.airline_left_sep = ""
vim.g.airline_right_sep = ""
vim.g.airline_theme = "base16"

return {
  -- Status line
  {
    'nvim-lualine/lualine.nvim',
    dependencies = {
      'nvim-tree/nvim-web-devicons',
      'arkav/lualine-lsp-progress',
    },
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
          theme = 'auto',
          section_separators = { left = ' ', right = ' ' },
          component_separators = { left = ' ', right = ' ' },
          globalstatus = true,
        },
        sections = {
          lualine_a = {
            {
              'mode',
              fmt = function(mode)
                return mode_map[mode] or mode
              end,
            },
          },
          lualine_b = {
            {
              'filename',
              file_status = true,
              newfile_status = true,
              path = 1,
              shorting_target = 40,

              buffers_color = {
                active = 'lualine_{section}_normal',
                inactive = 'lualine_{section}_inactive',
              },
              symbols = {
                modified = ' ●',
                readonly = ' ',
                alternate_file = ' ',
                directory = ' ',
              },
            },
            {
              'filetype',
              colored = true,
              icon_only = true,
              icon = { align = 'right' },
            },
          },
          lualine_c = {},

          lualine_x = {
            'lsp_status',
            {
              'lsp_progress',
              display_components = { 'lsp_client_name', 'spinner', { 'title', 'percentage', 'message' }},
            },
            {
              'diagnostics',
              sources = { 'nvim_lsp', 'nvim_diagnostic' },
              sections = { 'error', 'warn', 'info', 'hint' },

              diagnostics_color = {
                error = 'DiagnosticError', -- Changes diagnostics' error color.
                info  = 'DiagnosticInfo',  -- Changes diagnostics' info color.
                hint  = 'DiagnosticHint',  -- Changes diagnostics' hint color.
              },
              symbols = { error = ' ', warn = ' ', info = ' ', hint = '' },
              colored = true,          -- Displays diagnostics status in color if set to true.
              update_in_insert = true, -- Update diagnostics in insert mode.
              always_visible = false,  -- Show diagnostics even if there are none.
            },
          },
          lualine_y = {
            {
              'branch',
              {
                'diff',
                colored = true, -- Displays a colored diff status if set to true
                -- diff_color = {
                --   -- Same color values as the general color option can be used here.
                --   added    = 'LuaLineDiffAdd',                  -- Changes the diff's added color
                --   modified = 'LuaLineDiffChange',               -- Changes the diff's modified color
                --   removed  = 'LuaLineDiffDelete',               -- Changes the diff's removed color you
                -- },
                symbols = { added = '+', modified = '~', removed = '-' }, -- Changes the symbols used by the diff.
                -- source = nil,                                   -- A function that works as a data source for diff.
                -- It must return a table as such:
                --   { added = add_count, modified = modified_count, removed = removed_count }
                -- or nil on failure. count <= 0 won't be displayed.
              }
            },
            {
              'overseer',
              lualine_z = {
                {
                  'progress',
                },
                {
                  'location',
                  padding = 0,
                },
              },
            },
          },
        },
        -- tabline = {
        --   lualine_a = {},
        --   lualine_b = {},
        --   lualine_c = {},
        --   lualine_x = {},
        --   lualine_y = {},
        --   lualine_z = {},
        -- },
        -- winbar = {},
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
          'quickfix',
          'symbols-outline',
          'ctrlspace',
        },
      })
    end,
  },
}
