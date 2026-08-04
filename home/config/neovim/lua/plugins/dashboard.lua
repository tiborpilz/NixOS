vim.g.have_nerd_font = true

local function apply_dashboard_hl()
  vim.api.nvim_set_hl(0, "SnacksDashboardHeader",  { fg = "#5e81ac" })
  vim.api.nvim_set_hl(0, "SnacksDashboardFooter",  { fg = "#4c566a", italic = true })
  vim.api.nvim_set_hl(0, "SnacksDashboardSpecial", { fg = "#4c566a", italic = true })
  vim.api.nvim_set_hl(0, "SnacksDashboardTitle",   { fg = "#88c0d0", bold = true })
  vim.api.nvim_set_hl(0, "SnacksDashboardKey",     { fg = "#88c0d0" })
  vim.api.nvim_set_hl(0, "SnacksDashboardDesc",    { fg = "#d8dee9" })
  vim.api.nvim_set_hl(0, "SnacksDashboardFile",    { fg = "#d8dee9" })
  vim.api.nvim_set_hl(0, "SnacksDashboardDir",     { fg = "#616e88" })
  vim.api.nvim_set_hl(0, "SnacksDashboardIcon",    { fg = "#81a1c1" })
end

return {
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    opts = {
      -- takes over vim.ui.select, which is what review.nvim's comment list uses
      picker = {
        enabled = true,
        ui_select = true,
      },
      dashboard = {
        enabled = true,
        preset = {
          header = "n  e  o  v  i  m",
          keys = {
            { icon = " ", key = "f", desc = "Find File", action = ":Telescope frecency workspace=CWD" },
            { icon = " ", key = "g", desc = "Grep Project", action = ":Telescope live_grep" },
            { icon = " ", key = "r", desc = "Recent Files", action = ":Telescope oldfiles" },
            { icon = " ", key = "n", desc = "New File", action = ":ene | startinsert" },
            { icon = " ", key = "t", desc = "Run Task", action = ":OverseerRun" },
            { icon = " ", key = "c", desc = "Config", action = ":Telescope find_files cwd=" .. vim.fn.stdpath("config") },
            { icon = "󰒲 ", key = "l", desc = "Lazy", action = ":Lazy" },
            { icon = " ", key = "q", desc = "Quit", action = ":qa" },
          },
        },
        sections = {
          { section = "header", padding = 2 },
          { pane = 1, icon = " ", title = "Actions", section = "keys", indent = 2, padding = 1 },
          {
            pane = 2,
            icon = " ",
            title = "Recent Files",
            section = "recent_files",
            limit = 6,
            -- git scratch buffers aren't files you want to reopen
            filter = function(file) return not file:match("/%.git/") end,
            indent = 2,
            padding = 1,
          },
          { pane = 2, icon = " ", title = "Projects", section = "projects", limit = 4, indent = 2, padding = 1 },
          {
            pane = 2,
            icon = " ",
            title = "Git Status",
            section = "terminal",
            enabled = function() return require("snacks").git.get_root() ~= nil end,
            cmd = "git status --short --branch --renames",
            height = 6,
            indent = 2,
            padding = 1,
            ttl = 60,
          },
          { section = "startup" },
        },
      },
    },
    config = function(_, opts)
      require("snacks").setup(opts)

      local group = vim.api.nvim_create_augroup("DashboardNordHL", { clear = true })
      apply_dashboard_hl()
      vim.api.nvim_create_autocmd("ColorScheme", {
        group = group,
        callback = apply_dashboard_hl,
      })
      vim.api.nvim_create_autocmd("User", {
        group = group,
        pattern = { "SnacksDashboardOpened", "SnacksDashboardUpdatePost" },
        callback = apply_dashboard_hl,
      })
    end,
  },
}
