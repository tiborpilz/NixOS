return {
  {
    "folke/which-key.nvim",
    opts = {
      delay = 300,
      loop = true,
      win = {
        border = { " ", " ", " ", " ", " ", " ", " ", " " },
        height = { min = 3, max = 32 },
        padding = { 3, 3 },
        title_pos = "center",
        col = 0.5,
        title = true,
      },
      plugins = {
        marks = true,
        registers = true,
        spelling = {
          enabled = true,
          suggestions = 40,
        },
        presets = {
          operators = true,
          motions = true,
          text_objects = true,
          windows = true,
          nav = true,
          z = true,
          g = true,
        },
      },
      icons = {
        breadcrumb = "»",
        separator = "",
        group = "+",
      },
      preset = "modern",
      layout = {
        align = "center",
        spacing = 1,
        height = { min = 4 },
      },
    },
    config = function(_, opts)
      local wk = require("which-key")

      wk.setup(opts)

      wk.add({
        -- TODO: Add more icons, reorganize a bit and remove duplicates
        --- Groups
        { "<leader>d", group = "Debug", icon = "" },
        { "<leader>f", group = "File" },
        { "<leader>g", group = "Git", icon = "" },
        { "<leader>n", group = "Notes" },
        { "<leader>b", group = "Buffer" },
        { "<leader>s", group = "Search" },
        { "<leader>v", group = "View", icon = "󰕮" },
        { "<leader>x", group = "Diagnostics", icon = "󰒋" },
        { "<leader>c", group = "Code" },
        { "<leader>o", group = "Open" },
        { "<leader>t", group = "Test", icon = "" },
        { "<leader>p", group = "Project" },
        { "<leader>w", group = "Window" },
        { "<leader>z", group = "Zen" },
        { "<leader>T", group = "Toggle", icon = "󰑭" },
        { "<leader>W", group = "Window", icon = "󰕮" },

        --- Unsorted key commands
        { "<leader>wv", "<cmd>vsplit<cr>", desc = "Split Vertical" },
        { "<leader>wh", "<cmd>split<cr>", desc = "Split Horizontal" },
        { "<leader>Tz", "<cmd>ZenMode<cr>", desc = "Toggle Zen Mode" },
      })
    end,
  },
}
