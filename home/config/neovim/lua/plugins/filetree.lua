vim.cmd("autocmd FileType nerdtree map <buffer> <Tab> o")
return {
  {
    "preservim/nerdtree",
    keys = {
      {
        "<leader>op",
        function()
          vim.cmd(":NERDTreeToggle")
        end,
        desc = "Toggle NERDTree",
      },
    },
  },
  -- {
  --   'nvim-tree/nvim-tree.lua',
  --   version = '*',
  --   lazy = false,
  --   dependencies = {
  --     'nvim-tree/nvim-web-devicons',
  --   },
  --   keys = {
  --     {
  --       '<leader>op',
  --       '<cmd>NvimTreeToggle<cr>',
  --       desc = 'Toggle NvimTree',
  --     },
  --   },
  --   config = function()
  --     require('nvim-tree').setup {
  --       on_attach = function(bufnr)
  --         local api = require 'nvim-tree.api'

  --         -- Default Keybindings
  --         api.config.mappings.default_on_attach(bufnr)
  --       end,
  --     }
  --   end,
  -- }
}
