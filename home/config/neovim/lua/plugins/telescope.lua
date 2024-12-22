return {
  -- {
  --   {"AstroNvim/astrocommunity"},
  --   { import = "astrocommunity.recipes.telescope-nvchad-theme" },
  -- },
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
          border = {
            prompt = { 0, 0, 0, 0 },
            results = { 0, 0, 0, 0 },
            preview = { 0, 0, 0, 0 },
          },
          layout_strategy = "vertical",
          layout_config = {
            prompt_position = "bottom",
            width = 0.5,
            height = 0.8,
            preview_height = 0.6,
          }
        }
      })
      -- CONFIG
      local blend = 85

      vim.api.nvim_create_autocmd("FileType", {
        pattern = "TelescopePrompt",
        callback = function(ctx)
          local backdropName = "TelescopeBackdrop"
          local telescopeBufnr = ctx.buf

          -- `Telescope` does not set a zindex, so it uses the default value
          -- of `nvim_open_win`, which is 50: https://neovim.io/doc/user/api.html#nvim_open_win()
          local telescopeZindex = 50

          local backdropBufnr = vim.api.nvim_create_buf(false, true)
          local winnr = vim.api.nvim_open_win(backdropBufnr, false, {
            relative = "editor",
            row = 0,
            col = 0,
            width = vim.o.columns,
            height = vim.o.lines,
            focusable = false,
            style = "minimal",
            zindex = telescopeZindex - 1, -- ensure it's below the reference window
          })

          vim.api.nvim_set_hl(0, backdropName, { bg = "#000000", default = true })
          vim.wo[winnr].winhighlight = "Normal:" .. backdropName
          vim.wo[winnr].winblend = blend
          vim.bo[backdropBufnr].buftype = "nofile"

          -- close backdrop when the reference buffer is closed
          vim.api.nvim_create_autocmd({ "WinClosed", "BufLeave" }, {
            once = true,
            buffer = telescopeBufnr,
            callback = function()
              if vim.api.nvim_win_is_valid(winnr) then vim.api.nvim_win_close(winnr, true) end
              if vim.api.nvim_buf_is_valid(backdropBufnr) then
                vim.api.nvim_buf_delete(backdropBufnr, { force = true })
              end
            end,
          })
        end,
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
