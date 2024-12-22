vim.g.airline_left_sep = ""
vim.g.airline_right_sep = ""
vim.g.airline_theme = "base16"

return {
  -- Status line
  {
    "vim-airline/vim-airline",
    dependencies = {"vim-airline/vim-airline-themes"},
  },

  -- Floating Window Borders
  {
    "mikesmithgh/borderline.nvim",
    enabled = true,
    lazy = true,
    event = 'VeryLazy',
    config = function()
      require('borderline').setup({
        --  ...
      })
    end,
  },

  -- Colorschemes / Themes
  {"eddyekofo94/gruvbox-flat.nvim"},
  {"marko-cerovac/material.nvim"},
  {"kdheepak/monochrome.nvim"},
  {"EdenEast/nightfox.nvim"},
  {"RRethy/nvim-base16"},
  {"mcchrish/zenbones.nvim"},
  {"rktjmp/lush.nvim"},
  {"yorickpeterse/nvim-grey"},
  {
    "shaunsingh/nord.nvim",
    config = function()
      vim.cmd("colorscheme nord")
    end,
  },

  -- Screenkey
  {
    "NStefan002/screenkey.nvim",
    lazy = false,
    version = "*",
    config = function()
      require("screenkey").setup({
        -- Center
        win_opts = {
          width = 20;
          col = (vim.o.columns / 2) + 10,
          row = 1,
          border = "none",
          height = 1,
        },
      })
    end,
  },
}

