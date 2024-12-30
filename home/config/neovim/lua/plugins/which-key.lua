return {
  {
    "folke/which-key.nvim",
    opts = {
      delay = 300,
      loop = true,
      win = {
        border = "single",
        height = { min = 1, max = 10 },
        padding = { 1, 8 },
        title_pos = "center",
        col = 0.5,
      },
      plugins = {
        marks = true,
        registers = true,
        spelling = {
          enabled = true,
          suggestions = 20,
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
      },
    },
    config = function(_, opts)
      local wk = require("which-key")

      wk.setup(opts)

      wk.add({
        { "<leader>b", group = "Buffer" },
        { "<leader>s", group = "Search" },
        { "<leader>c", group = "Code" },
        { "<leader>o", group = "Toggle" },
        { "<leader>t", group = "Test", icon = "" },
        { "<leader>p", group = "Project" },
        { "<leader>w", group = "Window" },
      })
    end,
  },
}
