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
  --- Git Signs
  {
    'lewis6991/gitsigns.nvim',
    event = 'VeryLazy',
    keys = {
      { "<leader>gB", function () require('gitsigns').blame_line() end, desc = 'Git Blame Line' },
      { "<leader>gtB", function () require('gitsigns').toggle_current_line_blame() end, desc = 'Toggle Blame' },
    },
    config = function(_, opts)
      require('gitsigns').setup()
    end,
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
  -- Loaded on demand, not at startup: its highlight groups resolve DiffAdd/DiffDelete
  -- once at load time, and eager loading resolves them before the colorscheme settles
  {
    "esmuellert/codediff.nvim",
    version = "v2.49.2",
    cmd = { "CodeDiff" },
    opts = {
      -- Defaults are "DiffAdd"/"DiffDelete", but nord defines those as reverse
      -- video with a foreground green, so codediff ends up painting #A3BE8C as a
      -- background and the syntax colours on top go unreadable. Dark tints instead.
      highlights = {
        line_insert = 0x2f3d2c,
        line_delete = 0x402a2e,
      },
    },
    config = function(_, opts)
      require("codediff").setup(opts)

      -- codediff scrollbinds the panes but never syncs the cursor, and plain
      -- cursorbind would drift because alignment is done with virtual filler
      -- lines. Map through the hunk list instead.
      local function map_line(changes, line, from_original)
        local from = from_original and "original" or "modified"
        local to = from_original and "modified" or "original"
        local offset = 0
        for _, c in ipairs(changes) do
          local f, t = c[from], c[to]
          if line < f.start_line then break end
          if line < f.end_line then
            local span = math.max(0, t.end_line - t.start_line - 1)
            return t.start_line + math.min(line - f.start_line, span)
          end
          offset = offset + (t.end_line - t.start_line) - (f.end_line - f.start_line)
        end
        return line + offset
      end

      local syncing = false
      vim.api.nvim_create_autocmd("CursorMoved", {
        group = vim.api.nvim_create_augroup("CodediffCursorSync", { clear = true }),
        callback = function()
          if syncing then return end
          local ok, lifecycle = pcall(require, "codediff.ui.lifecycle")
          if not ok then return end
          local sess = lifecycle.get_session(vim.api.nvim_get_current_tabpage())
          if not sess then return end

          local win = vim.api.nvim_get_current_win()
          local from_original = win == sess.original_win
          if not from_original and win ~= sess.modified_win then return end
          local other = from_original and sess.modified_win or sess.original_win
          if not (other and vim.api.nvim_win_is_valid(other)) then return end

          local changes = sess.stored_diff_result and sess.stored_diff_result.changes or {}
          local target = map_line(changes, vim.api.nvim_win_get_cursor(win)[1], from_original)
          target = math.max(1, math.min(target, vim.api.nvim_buf_line_count(vim.api.nvim_win_get_buf(other))))

          syncing = true
          pcall(vim.api.nvim_win_set_cursor, other, { target, 0 })
          syncing = false
        end,
      })
    end,
  },
  --- Review local diffs with inline comments, exports as markdown for agents
  {
    "georgeguimaraes/review.nvim",
    version = "*",
    -- Upstream PR #31 fixes focus stealing on file select and review keymaps
    -- leaking into the explorer. Open and unmerged since 2026-03-26, repo dormant.
    build = function(plugin)
      local patch = vim.fn.stdpath("config") .. "/patches/pr31-focus.patch"
      vim.system({ "git", "-C", plugin.dir, "checkout", "--", "." }):wait()
      local res = vim.system({ "git", "-C", plugin.dir, "apply", patch }):wait()
      if res.code ~= 0 then
        error("review.nvim: pr31-focus.patch did not apply: " .. (res.stderr or ""))
      end
    end,
    dependencies = {
      -- v2.50.0 made get_paths return typed Path tables, review.nvim still wants strings
      { "esmuellert/codediff.nvim", version = "v2.49.2" },
      "MunifTanjim/nui.nvim",
    },
    cmd = { "Review" },
    -- codediff can be opened directly, so attach the review layer however it starts
    event = "User CodeDiffOpen",
    -- :Review can't express "<rev> vs working tree" - it folds a lone rev into
    -- rev^..rev. :CodeDiff <rev> does exactly that, so drive codediff itself and
    -- reset the comment store, which review.nvim otherwise only does for itself.
    init = function()
      local function default_branch()
        local head = vim.system({ "git", "symbolic-ref", "--short", "refs/remotes/origin/HEAD" }):wait()
        if head.code == 0 then
          return vim.trim(head.stdout):gsub("^origin/", "")
        end
        for _, b in ipairs({ "main", "master" }) do
          if vim.system({ "git", "rev-parse", "--verify", "--quiet", b }):wait().code == 0 then
            return b
          end
        end
        return "HEAD"
      end

      -- review.nvim only resets its comment store on its own code paths
      local function reset_store()
        local store = require("review.store")
        store.reset()
        store.load()
      end

      local function err(msg)
        vim.notify(msg, vim.log.levels.ERROR, { title = "ReviewDiff" })
      end

      vim.api.nvim_create_user_command("ReviewDiff", function(o)
        local rev = o.args ~= "" and o.args or default_branch()
        require("review.storage").clear_revisions()
        reset_store()
        vim.cmd("CodeDiff " .. rev)
      end, {
        nargs = "?",
        desc = "Review <rev> against the working tree (default: default branch)",
        complete = function(lead)
          local res = vim.system({ "git", "for-each-ref", "--format=%(refname:short)" }):wait()
          if res.code ~= 0 then return {} end
          return vim.tbl_filter(function(r)
            return r ~= "" and r:find(lead, 1, true) == 1
          end, vim.split(res.stdout, "\n"))
        end,
      })

      -- Review a GitHub PR the way the PR page shows it: merge-base against the
      -- head, so commits the base picked up meanwhile (and merges of base into
      -- the branch) stay out of the diff.
      vim.api.nvim_create_user_command("ReviewPR", function(o)
        -- gh resolves the repo without needing a github remote, git fetch does not
        local remote = "origin"
        local remotes = vim.system({ "git", "remote", "-v" }):wait()
        for line in (remotes.stdout or ""):gmatch("[^\n]+") do
          local name, url = line:match("^(%S+)%s+(%S+)%s+%(fetch%)$")
          if name and url:find("github.com", 1, true) then
            remote = name
            break
          end
        end

        -- Both network calls are async, they take seconds and would block the UI
        local view = { "gh", "pr", "view" }
        if o.args ~= "" then table.insert(view, o.args) end
        vim.list_extend(view, { "--json", "number,baseRefName" })

        vim.notify("Fetching PR" .. (o.args ~= "" and " #" .. o.args or "") .. "…", vim.log.levels.INFO,
          { title = "ReviewPR" })

        vim.system(view, { text = true }, function(res)
          if res.code ~= 0 then
            return vim.schedule(function() err("gh pr view failed: " .. (res.stderr or "")) end)
          end
          local ok, meta = pcall(vim.json.decode, res.stdout)
          if not ok or not meta.number then
            return vim.schedule(function() err("could not read PR metadata") end)
          end

          local num = tostring(meta.number)

          -- With a bang, check the branch out so the right pane is a real file on
          -- disk and LSP attaches. Plain :ReviewPR diffs two revisions, and neither
          -- side exists on disk, so no server can attach to either.
          if o.bang then
            -- back on the main loop, :wait() is not allowed in a callback context
            return vim.schedule(function()
              local dirty = vim.system({ "git", "status", "--porcelain" }):wait()
              if vim.trim(dirty.stdout or "") ~= "" then
                return err("working tree is dirty, cannot check out PR #" .. num)
              end
              vim.system({ "gh", "pr", "checkout", num }, { text = true }, function(co)
                vim.schedule(function()
                  if co.code ~= 0 then
                    return err("gh pr checkout failed: " .. (co.stderr or ""))
                  end
                  require("review.storage").set_revisions("pr" .. num, "base")
                  reset_store()
                  vim.cmd("CodeDiff " .. meta.baseRefName .. "...")
                end)
              end)
            end)
          end

          local head_ref, base_ref = "refs/pr/" .. num, "refs/pr/" .. num .. "-base"

          vim.system({
            "git", "fetch", "--force", remote,
            "pull/" .. num .. "/head:" .. head_ref,
            meta.baseRefName .. ":" .. base_ref,
          }, { text = true }, function(fetched)
            if fetched.code ~= 0 then
              return vim.schedule(function() err("fetch failed: " .. (fetched.stderr or "")) end)
            end
            vim.schedule(function()
              -- Keyed by PR, not by ref: storage truncates revs to 8 chars, so a
              -- refs/... name would collide and put a / in the filename
              require("review.storage").set_revisions("pr" .. num, "base")
              reset_store()
              vim.cmd("CodeDiff " .. base_ref .. "..." .. head_ref)
            end)
          end)
        end)
      end, {
        nargs = "?",
        bang = true,
        desc = "Review a GitHub PR's changes (! checks the branch out so LSP works)",
      })
    end,
    keys = {
      { "<leader>gr", "<cmd>Review<cr>", desc = "Review working tree" },
      { "<leader>gR", "<cmd>Review commits<cr>", desc = "Review commit range" },
      { "<leader>gd", "<cmd>ReviewDiff<cr>", desc = "Review default branch vs working tree" },
      { "<leader>gD", ":ReviewDiff ", desc = "Review <rev> vs working tree" },
      { "<leader>gP", "<cmd>ReviewPR<cr>", desc = "Review GitHub PR" },
      -- Comments, mirroring the buffer-local keys review.nvim sets inside the diff
      { "<leader>gcc", function() require("review.comments").add_with_menu() end, desc = "Add comment (pick type)" },
      { "<leader>gcc", function() require("review.comments").add_for_range() end, mode = "v", desc = "Comment on selection" },
      { "<leader>gcn", function() require("review").add_note() end, desc = "Add note" },
      { "<leader>gcs", function() require("review").add_suggestion() end, desc = "Add suggestion" },
      { "<leader>gci", function() require("review").add_issue() end, desc = "Add issue" },
      { "<leader>gcp", function() require("review").add_praise() end, desc = "Add praise" },
      { "<leader>gcf", function() require("review.comments").file_comment() end, desc = "Comment on whole file" },
      { "<leader>gce", function() require("review.comments").edit_at_cursor() end, desc = "Edit comment" },
      { "<leader>gcd", function() require("review.comments").delete_at_cursor() end, desc = "Delete comment" },
      { "<leader>gcl", function() require("review.comments").list() end, desc = "List comments" },
      { "<leader>gcx", function() require("review").export() end, desc = "Export comments to clipboard" },
      { "<leader>gcv", function() require("review").preview() end, desc = "Preview exported markdown" },
      { "<leader>gcX", function() require("review").clear() end, desc = "Clear all comments" },
    },
    opts = {},
  },
  --- Same idea as review.nvim but standalone, annotates any buffer, no codediff coupling
  {
    "eltonsst/postilla.nvim",
    -- v0.1.4 is still the pre-rename local-review.nvim, and v0.2.0 was never tagged
    commit = "6a622cf7a68116164544d85f068b0bbad4b0b54d",
    cmd = {
      "PostillaStart", "PostillaComment", "PostillaList", "PostillaDone",
      "PostillaEdit", "PostillaDelete", "PostillaStatus", "PostillaAbort",
    },
    keys = {
      { "<leader>gN", "<cmd>PostillaStart<cr>", desc = "Start local review session" },
      { "<leader>gn", "<cmd>PostillaComment<cr>", desc = "Add review note at line" },
    },
    -- keymap stays nil, the plugin default is <leader>rc which orgmode owns
    opts = { context_lines = 5 },
  },
  --- Review actual PRs/MRs, needs a GitHub or GitLab remote
  {
    "afewyards/codereview.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = {
      "CodeReview", "CodeReviewAI", "CodeReviewAIFile",
      "CodeReviewStart", "CodeReviewSubmit", "CodeReviewApprove",
      "CodeReviewOpen", "CodeReviewPipeline", "CodeReviewComments",
      "CodeReviewFiles", "CodeReviewToggleScroll", "CodeReviewCommits",
    },
    keys = {
      { "<leader>gv", "<cmd>CodeReview<cr>", desc = "Review PR/MR" },
    },
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
    keymap = {
      ["next"] = '<C-n>',             -- next entry in log (older)
      ["prev"] = '<C-p>',             -- previous entry in log (newer)
      ["quit"] = 'q',                 -- quit all
      ["revision_message"] = '<C-m>', -- show revision message for current revision
      ["commit"] = '<C-g>',           -- replace contents of origin buffer with contents of tardis buffer
    },
    config = function(_, opts)
      require("tardis-nvim").setup(opts)
      vim.keymap.set("n", "<leader>gT", "<cmd>Tardis<cr>", { noremap = true, silent = true, desc = "Open Time Machine" })
    end,
  },
}
