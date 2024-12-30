function neogit_action(popup, action, args)
  return function()
    require'plenary.async'.run(function()
      require('neogit.popups.' .. popup ..'.actions')[action] {
        get_arguments = function()
          return args
        end
      }
    end)
  end
end

return {
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "isakbm/gitgraph.nvim",
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
      },
    },
    keys = {
      { "<leader>gg", function() require("neogit").open() end, desc = "Open Neogit" },
      { "<leader>gl", function() require("neogit").open({ "log" }) end, desc = "Git Log" },
    },
  },
  {
    'topaxi/pipeline.nvim',
    keys = {
      { '<leader>gp', '<cmd>Pipeline<cr>', desc = 'Open pipeline.nvim' },
    },
    -- optional, you can also install and use `yq` instead.
    build = 'make',
    opts = {},
  },
}
