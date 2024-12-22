return {
  {
    {"AstroNvim/astrocommunity"},
    { import = "astrocommunity.recipes.telescope-nvchad-theme" },
  },
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {"nvim-lua/plenary.nvim"},
    config = function()
      local telescope = require("telescope")
      telescope.setup({
        defaults = {
          prompt_prefix =" ",
          entry_prefix = " ",
          selection_caret = " ",
          layout_strategy = "vertical",
          layout_config = {
            prompt_position = "bottom",

            width = 0.5,
            height = 0.8,
            preview_height = 0.6,
          }
        }
      })
    end,
    keys = {
      {
        "<leader>pf",
        function() require("telescope.builtin").find_files() end,
        desc = "Find Files",
      },
      {
        "<leader>sp",
        function() require("telescope.builtin").live_grep() end,
        desc = "Search Project",
      },
      {
        "<leader>bb",
        function() require("telescope.builtin").buffers() end,
        desc = "List Buffers",
      },
      {
        "<leader>fh",
        function() require("telescope.builtin").help_tags() end,
        desc = "Find Help",
      },
      {
        "<leader>cX",
        function() require("telescope.builtin").diagnostics() end,
        desc = "Show Diagnostics",
      },
    },
  },
}
