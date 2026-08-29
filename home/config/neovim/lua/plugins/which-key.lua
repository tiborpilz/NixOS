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
        { "<leader>nr", group = "Roam" },
        { "<leader>nra", group = "Alias" },
        { "<leader>nrd", group = "By Date" },
        { "<leader>nro", group = "Origin" },
        { "<leader>m", group = "Markdown" },
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

        --- org-roam ships paragraph-length descriptions; these shorten them
        --- to Doom's wording for the popup.
        { "<leader>nr.", desc = "Complete at point" },
        { "<leader>nraa", desc = "Add alias" },
        { "<leader>nrar", desc = "Remove alias" },
        { "<leader>nrb", desc = "Toggle buffer (fixed)" },
        { "<leader>nrc", desc = "Capture" },
        { "<leader>nrf", desc = "Find node" },
        { "<leader>nri", desc = "Insert node" },
        { "<leader>nrl", desc = "Toggle buffer" },
        { "<leader>nrm", desc = "Insert node (immediate)" },
        { "<leader>nrn", desc = "Next node" },
        { "<leader>nroa", desc = "Add origin" },
        { "<leader>nror", desc = "Remove origin" },
        { "<leader>nrp", desc = "Prev node" },
        { "<leader>nrq", desc = "Backlinks quickfix" },

        { "<leader>nrd-", desc = "Find directory" },
        { "<leader>nrdb", desc = "Goto previous note" },
        { "<leader>nrdd", desc = "Goto date" },
        { "<leader>nrdD", desc = "Capture date" },
        { "<leader>nrdf", desc = "Goto next note" },
        { "<leader>nrdm", desc = "Goto tomorrow" },
        { "<leader>nrdM", desc = "Capture tomorrow" },
        { "<leader>nrdn", desc = "Capture today" },
        { "<leader>nrdt", desc = "Goto today" },
        { "<leader>nrdy", desc = "Goto yesterday" },
        { "<leader>nrdY", desc = "Capture yesterday" },

        --- Unsorted key commands
        { "<leader>wv", "<cmd>vsplit<cr>", desc = "Split Vertical" },
        { "<leader>wh", "<cmd>split<cr>", desc = "Split Horizontal" },
        { "<leader>Tz", "<cmd>ZenMode<cr>", desc = "Toggle Zen Mode" },
      })
    end,
  },
}
