return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    },
    config = function()
      local telescope = require("telescope")
      telescope.setup({
        defaults = {
          prompt_prefix = "> ",
          entry_prefix = " ",
          selection_caret = " ",
          borderchars = {
            prompt = { " ", " ", " ", " ", " ", " ", " ", " " },
            results = { " ", " ", " ", " ", " ", " ", " ", " " },
            preview = { " ", " ", " ", " ", " ", " ", " ", " " },
          },
          border = {
            prompt = { 1, 1, 1, 1 },
            results = { 1, 1, 0, 1 },
            preview = { 1, 1, 0, 1 },
          },
          layout_strategy = "vertical",
          layout_config = {
            prompt_position = "bottom",
            width = 0.5,
            height = 0.8,
          }
        },
        extensions = {
          fzf = {
            fuzzy = true,
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
          },
          frecency = {
            db_safe_mode = true,
            db_version = "v2",
            matcher = "fuzzy",
          },
          cmdline = {
            overseer = {
              enabled = true,
            },
          },
        },
      })
      -- CONFIG
      local blend = 85

      vim.api.nvim_create_autocmd("FileType", {
        -- Telescope, Mason
        pattern = "TelescopePrompt,Mason,LspInfo",
        callback = function(ctx)
          local backdropName = "TelescopeBackdrop"
          local telescopeBufnr = ctx.buf

          local telescopeZindex = 2

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

          -- show line numbers and highlight the current line in preview
          vim.api.nvim_create_autocmd('User', {
            pattern = 'TelescopePreviewerLoaded',
            callback = function(args)
              vim.wo.number     = true
              vim.wo.cursorline = true

              local ok, picker = pcall(
                require('telescope.actions.state').get_current_picker,
                args.data.prompt_bufnr
              )
            end,
          })
        end,
      })
    end,
    keys = {
      {
        "<leader>pf",
        function()
          vim.cmd("Telescope frecency workspace=CWD")
        end,
        desc = "Find Files",
      },
      {
        "<leader>sp",
        function()
          require("telescope.builtin").live_grep()
        end,
        desc = "Search in Project",
      },
      {
        "<leader>bb",
        function() require("telescope.builtin").buffers() end,
        desc = "List Buffers",
      },
      {
        "<leader>bp",
        desc = "Go to Previous Buffer",
        function()
          local last_buffer = vim.fn.bufnr("#")
          if last_buffer > 0 then
            vim.cmd("buffer " .. last_buffer)
          end
        end,
      },
      {
        "<leader>bn",
        desc = "Go to Next Buffer",
        function()
          local last_buffer = vim.fn.bufnr("#")
          if last_buffer > 0 then
            vim.cmd("buffer " .. last_buffer)
          end
        end,
      },
      {
        "<leader>bk",
        desc = "Close Buffer",
        function()
          local current_buffer = vim.fn.bufnr("%")
          if current_buffer > 1 then
            vim.cmd("bdelete " .. current_buffer)
          end
        end,
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
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    event = "VeryLazy",
    build = "make",
    config = function()
      require("telescope").load_extension "fzf"
    end,
  },
  {
    "nvim-telescope/telescope-frecency.nvim",
    event = "VeryLazy",
    config = function()
      require("telescope").load_extension "frecency"
    end,
  },
  {
    "jonarrien/telescope-cmdline.nvim",
    event = "VeryLazy",
    config = function()
      require("telescope").load_extension "cmdline"
    end,
    keys = {
      {
        "<leader><leader>",
        function()
          require("telescope").extensions.cmdline.cmdline {
            cmd = "command",
            prompt_title = "Command History",
            previewer = false,
          }
        end,
        desc = "Command",
      },
    },
  },
}
