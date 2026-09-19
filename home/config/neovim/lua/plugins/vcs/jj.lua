return {
  --- Gutter signs for the current jj change, @- against the working copy
  {
    "evanphx/jjsigns.nvim",
    event = "VeryLazy",
    -- Its highlights link to the GitSigns* groups when they exist at setup
    dependencies = { "lewis6991/gitsigns.nvim" },
    opts = {},
  },
  --- A colocated jj checkout is also a Git repo, leave its signs to jjsigns
  {
    "lewis6991/gitsigns.nvim",
    opts = {
      on_attach = function(bufnr) return vim.fs.root(bufnr, ".jj") == nil end,
    },
  },
  --- jj log/status with rebase, squash, split, abandon and bookmarks from the log buffer
  {
    "NicolasGB/jj.nvim",
    version = "*",
    dependencies = { { "esmuellert/codediff.nvim", version = "v2.49.2" } },
    cmd = { "J", "Jdiff", "Jvdiff", "Jhdiff", "Jbrowse", "ReviewChange", "ReviewStack", "ReviewTrunk" },
    keys = {
      { "<leader>gjs", function() require("jj.cmd").status() end, desc = "jj status" },
      { "<leader>gjl", function() require("jj.cmd").log() end, desc = "jj log" },
      { "<leader>gjb", function() require("jj.cmd").bookmark_create() end, desc = "jj bookmark" },
      { "<leader>gjB", function() require("jj.annotate").line() end, desc = "jj annotate line" },
      { "<leader>gjr", "<cmd>ReviewChange<cr>", desc = "Review current jj change" },
      { "<leader>gjR", "<cmd>ReviewStack<cr>", desc = "Review jj stack" },
      { "<leader>gjd", "<cmd>ReviewTrunk<cr>", desc = "Review trunk() vs working copy" },
      { "<leader>gjD", ":ReviewTrunk ", desc = "Review <rev> vs working copy" },
    },
    opts = {
      diff = { backend = "codediff" },
    },
    config = function(_, opts)
      require("jj").setup(opts)

      -- review.nvim only resets its comment store on its own code paths
      local function reset_store()
        local store = require("review.store")
        store.reset()
        store.load()
      end

      -- In a jj checkout HEAD is always detached, so review.nvim's per-branch
      -- storage would put every review in one file. Key by change ID instead,
      -- which also keeps the comments when the change is rewritten.
      local function change_id(rev)
        local res = vim.system({ "jj", "log", "--no-graph", "-r", rev, "-T", "change_id" }):wait()
        return vim.trim(res.stdout)
      end

      vim.api.nvim_create_user_command("ReviewChange", function(o)
        local rev = o.args ~= "" and o.args or "@"
        require("review.storage").set_revisions(change_id(rev), "change")
        reset_store()
        -- For @ this diffs against the working copy, so LSP attaches on the right
        require("jj.diff").show_revision({ rev = rev })
      end, { nargs = "?", desc = "Review a jj change against its parent (default: @)" })

      vim.api.nvim_create_user_command("ReviewStack", function()
        require("review.storage").set_revisions(change_id("@"), "stack")
        reset_store()
        -- jj.nvim's codediff backend takes `left` as the new side
        require("jj.diff").diff_revisions({ left = "@", right = "fork_point(trunk() | @)" })
      end, { desc = "Review the jj stack since it forked off trunk()" })

      vim.api.nvim_create_user_command("ReviewTrunk", function(o)
        require("review.storage").set_revisions(change_id("@"), "diff")
        reset_store()
        require("jj.diff").diff_revisions({ left = "@", right = o.args ~= "" and o.args or "trunk()" })
      end, { nargs = "?", desc = "Review the jj working copy against <rev> (default: trunk())" })
    end,
  },
}
