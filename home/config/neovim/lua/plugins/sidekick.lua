return {
  {
    "folke/sidekick.nvim",
    version = "*",
    opts = {
      cli = {
        mux = {
          backend = "tmux",
          enabled = true,
        },
      },
    },
    -- <leader>a is claudecode.nvim's, so sideKick gets <leader>k
    keys = {
      {
        "<tab>",
        function()
          if not require("sidekick").nes_jump_or_apply() then
            return "<Tab>"
          end
        end,
        expr = true,
        desc = "Goto/Apply Next Edit Suggestion",
      },
      { "<c-.>", function() require("sidekick.cli").focus() end, mode = { "n", "t", "i", "x" }, desc = "Sidekick focus" },
      { "<leader>kk", function() require("sidekick.cli").toggle() end, desc = "Toggle sidekick CLI" },
      { "<leader>kc", function() require("sidekick.cli").toggle({ name = "claude", focus = true }) end, desc = "Toggle Claude" },
      { "<leader>ks", function() require("sidekick.cli").select() end, desc = "Select CLI" },
      { "<leader>kd", function() require("sidekick.cli").close() end, desc = "Detach CLI session" },
      { "<leader>kt", function() require("sidekick.cli").send({ msg = "{this}" }) end, mode = { "n", "x" }, desc = "Send this" },
      { "<leader>kf", function() require("sidekick.cli").send({ msg = "{file}" }) end, desc = "Send file" },
      { "<leader>kv", function() require("sidekick.cli").send({ msg = "{selection}" }) end, mode = "x", desc = "Send selection" },
      { "<leader>kp", function() require("sidekick.cli").prompt() end, mode = { "n", "x" }, desc = "Select prompt" },
      -- review.nvim exports its comments through sidekick.cli.send
      { "<leader>gck", "<cmd>Review sidekick<cr>", desc = "Send review comments to sidekick" },
    },
  },
}
