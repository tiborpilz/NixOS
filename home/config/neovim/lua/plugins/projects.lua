return {
  {
    "coffebar/neovim-project",
    opts = {
      projects = {
        "~/Code/*",
      },
      picker = {
        type = "telescope",
      },
      dashboard_mode = true,
    },
    init = function()
      -- enable saving the state of plugins in the session
      vim.opt.sessionoptions:append("globals")
      vim.keymap.set("n", "<leader>pp", "<cmd>NeovimProjectDiscover<CR>", { desc = "Switch Project" })
    end,
    dependencies = {
      { "nvim-lua/plenary.nvim" },
      { "nvim-telescope/telescope.nvim" },
      { "Shatur/neovim-session-manager" },
    },
    lazy = false,
    priority = 100,
  },
  {
    'stevearc/overseer.nvim',
    opts = {},
    init = function()
      vim.keymap.set("n", "<leader>pt", "<cmd>OverseerToggle<CR>", { desc = "Open Project Tasks" })
      vim.keymap.set("n", "<leader>pr", "<cmd>OverseerRun<CR>", { desc = "Run Project Tasks" })
    end,
  },
}
