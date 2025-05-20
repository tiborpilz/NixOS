vim.cmd("autocmd FileType nerdtree map <buffer> <Tab> o")
return {
  -- {
  --   "preservim/nerdtree",
  --   keys = {
  --     {
  --       "<leader>op",
  --       function()
  --         vim.cmd(":NERDTreeToggle")
  --       end,
  --       desc = "Toggle NERDTree",
  --     },
  --   },
  -- },
  {
    'nvim-tree/nvim-tree.lua',
    version = '*',
    lazy = false,
    dependencies = {
      'nvim-tree/nvim-web-devicons',
    },
    keys = {
      {
        '<leader>op',
        '<cmd>NvimTreeToggle<cr>',
        desc = 'Toggle NvimTree',
      },
    },
    config = function()
      require('nvim-tree').setup {
        sync_root_with_cwd = true,
        respect_buf_cwd = true,
        update_focused_file = {
          enable = true,
          update_root = true,
        },
        on_attach = function(bufnr)
          local api = require 'nvim-tree.api'

          -- Default Keybindings
          -- api.config.mappings.default_on_attach(bufnr)

          -- Custom keybindings similar to NERDTree
          local function opts(desc)
            return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
          end

          -- Basic operations
          vim.keymap.set('n', 'o', api.node.open.edit, opts('Open'))
          vim.keymap.set('n', '<CR>', api.node.open.edit, opts('Open'))
          vim.keymap.set('n', '<Tab>', api.node.open.edit, opts('Open'))
          vim.keymap.set('n', 'O', api.node.open.no_window_picker, opts('Open: No Window Picker'))
          vim.keymap.set('n', 'i', api.node.open.vertical, opts('Open: Vertical Split'))
          vim.keymap.set('n', 's', api.node.open.horizontal, opts('Open: Horizontal Split'))
          vim.keymap.set('n', 't', api.node.open.tab, opts('Open: New Tab'))

          -- File operations
          vim.keymap.set('n', 'm', api.fs.create, opts('Create'))
          vim.keymap.set('n', 'd', api.fs.remove, opts('Delete'))
          vim.keymap.set('n', 'r', api.fs.rename, opts('Rename'))
          vim.keymap.set('n', 'c', api.fs.copy.node, opts('Copy'))
          vim.keymap.set('n', 'p', api.fs.paste, opts('Paste'))
          vim.keymap.set('n', 'x', api.fs.cut, opts('Cut'))

          -- Navigation
          vim.keymap.set('n', 'P', api.node.navigate.parent, opts('Parent Directory'))
          vim.keymap.set('n', 'K', api.node.navigate.sibling.first, opts('First Sibling'))
          vim.keymap.set('n', 'J', api.node.navigate.sibling.last, opts('Last Sibling'))
          vim.keymap.set('n', 'C', api.tree.change_root_to_node, opts('CD'))
          vim.keymap.set('n', 'u', api.tree.change_root_to_parent, opts('Up'))

          -- Tree manipulation
          vim.keymap.set('n', 'R', api.tree.reload, opts('Refresh'))
          vim.keymap.set('n', 'a', api.tree.toggle_hidden_filter, opts('Toggle Dotfiles'))
          vim.keymap.set('n', 'I', api.tree.toggle_gitignore_filter, opts('Toggle Git Ignore'))
          vim.keymap.set('n', 'H', api.tree.toggle_hidden_filter, opts('Toggle Hidden Files'))
          vim.keymap.set('n', '?', api.tree.toggle_help, opts('Help'))

          -- Filesystem operations
          vim.keymap.set('n', 'q', api.tree.close, opts('Close'))
          vim.keymap.set('n', 'g?', api.tree.toggle_help, opts('Help'))
        end,

        -- Other configuration options
        view = {
          adaptive_size = true,
        },
        renderer = {
          group_empty = true,
        },
        filters = {
          dotfiles = false,
        },
      }
    end,
  }
}
