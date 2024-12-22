return {
  {
    "folke/which-key.nvim",
    opts = {
      loop = true,
      triggers = {
        { "<leader>", mode = { "n", "v" } },
      },
      win = {
        -- border = "none",
        height = { min = 1, max = 10 },
        padding = { 1, 8 },
        title = "Henlo",
        title_pos = "center",
        col = 0.5,
      },
      preset = "helix",
      layout = {
        align = "center",
        width = 20,
      },
    },
  },
}
