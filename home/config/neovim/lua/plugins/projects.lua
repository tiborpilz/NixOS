return {
  -- {
  --   "coffebar/neovim-project",
  --   opts = {
  --     projects = {
  --       "~/Code/*",
  --     },
  --     picker = {
  --       type = "telescope",
  --       preview = {
  --         enabled = true, -- show directory structure in Telescope preview
  --         git_status = true, -- show branch name, an ahead/behind counter, and the git status of each file/folder
  --         git_fetch = true, -- fetch from remote, used to display the number of commits ahead/behind, requires git authorization
  --         show_hidden = true, -- show hidden files/folders
  --       },
  --     },
  --     dashboard_mode = true,
  --   },
  --   init = function()
  --     -- enable saving the state of plugins in the session
  --     vim.opt.sessionoptions:append("globals")
  --     vim.keymap.set("n", "<leader>pp", "<cmd>NeovimProjectDiscover<CR>", { desc = "Switch Project" })
  --   end,
  --   dependencies = {
  --     { "nvim-lua/plenary.nvim" },
  --     { "nvim-telescope/telescope.nvim" },
  --     { "Shatur/neovim-session-manager" },
  --   },
  --   lazy = false,
  --   priority = 100,
  -- },
  {
    'stevearc/overseer.nvim',
    opts = {},
    init = function()
      vim.keymap.set("n", "<leader>pt", "<cmd>OverseerToggle<CR>", { desc = "Open Project Tasks" })
      vim.keymap.set("n", "<leader>pr", "<cmd>OverseerRun<CR>", { desc = "Run Project Tasks" })
    end,
  },
  {
    "ahmedkhalf/project.nvim",
    config = function()
      require("project_nvim").setup({
        manual_mode = false,
      })
      require("telescope").load_extension("projects")
      vim.keymap.set("n", "<leader>pp", "<cmd>Telescope projects<CR>", { desc = "Switch Project" })
    end
  }
}
