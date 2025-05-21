-- use "git jump" to list all merge conflicts and feed them into telescope
-- TODO: Find out why this doesn't work
-- TODO: restructure lua files so these functions have a better home
--
-- local git_conflicts = function()
--   require("telescope.pickers")
--     .new({
--       finder = require("telescope.finders").new_oneshot_job({ "git", "jump", "--stdout", "merge" }, {
--         entry_maker = function(line)
--           local filename, lnum_string = line:match("([^:]+):(%d+).*")
--
--           -- if filename is /dev/null, skip it (it was deleted)
--           if filename.match("^/dev/null") then
--             return nil
--           end
--
--           return {
--             value = filename,
--             display = line,
--             ordinal = line,
--             filename = filename,
--             lnum = tonumber(lnum_string),
--           }
--         end,
--       }),
--       sorter = require("telescope.sorters").get_generic_fuzzy_sorter(),
--       previewer = require("telescope.config").values.grep_previewer({}),
--       results_title = "Git Merge Conflicts",
--       prompt_title = "Git Merge Conflicts",
--     }, {})
--     :find()
--   end
--
-- vim.keymap.set("n", "<leader>gc", git_conflicts, { desc = "List Git Conflicts" })

return {
  {
    "sindrets/diffview.nvim",
    setup = function()
      require("diffview").setup()
    end,
  },
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "isakbm/gitgraph.nvim",
      "nvim-telescope/telescope.nvim",
    },
    opts = {
      disable_signs = false,
      disable_context_highlighting = false,
      disable_commit_confirmation = false,
      disable_builtin_notifications = false,
      auto_refresh = true,
      disable_insert_on_commit = false,
      graph_style = "kitty",
      commit_popup = {
        kind = "split",
      },
      -- customize displayed signs
      signs = {
        -- { CLOSED, OPENED }
        section = { "", "" },
        item = { "", "" },
        hunk = { "", "" },
      },
      integrations = {
        diffview = true,
        telescope = true,
      },
    },
    keys = {
      { "<leader>gg", function() require("neogit").open() end, desc = "Open Neogit" },
      { "<leader>gl", function() require("neogit").open({ "log" }) end, desc = "Git Log" },
      { "<leader>gb", function() require("neogit").open({ "branch" }) end, desc = "Git branch" },
    },
  },
  --- Show Pipeline information for Github and Gitlab
  {
    'topaxi/pipeline.nvim',
    keys = {
      { '<leader>gp', '<cmd>Pipeline<cr>', desc = 'Open pipeline.nvim' },
    },
    -- optional, you can also install and use `yq` instead.
    build = 'make',
    opts = {},
  },
  --- Move backwards and forwards in Git history
  {
    'fredeeb/tardis.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    opts = {
      keymap = {
        ["next"] = 'p',             -- next entry in log (older)
        ["prev"] = 'n',             -- previous entry in log (newer)
        ["quit"] = 'q',                 -- quit all
        ["revision_message"] = '<C-m>', -- show revision message for current revision
        ["commit"] = '<C-g>',           -- replace contents of origin buffer with contents of tardis buffer
      },
      initial_revisions = 128,
    },
    config = function(_, opts)
      require("tardis-nvim").setup(opts)
      vim.keymap.set("n", "<leader>gt", "<cmd>Tardis<cr>", { noremap = true, silent = true, desc = "Open Time Machine" })
    end,
  },
}
