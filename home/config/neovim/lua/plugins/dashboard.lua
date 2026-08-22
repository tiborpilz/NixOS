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

local roam_dir = vim.fn.expand("~/org/roam")

local function roam_title(path)
  local fh = io.open(path, "r")
  if not fh then return nil end
  local title
  for _ = 1, 20 do
    local line = fh:read("*l")
    if not line then break end
    title = line:match("^#%+[tT][iI][tT][lL][eE]:%s*(.+)$")
    if title then break end
  end
  fh:close()
  return title
end

local function truncate(str, width)
  if vim.fn.strchars(str) <= width then return str end
  return vim.fn.strcharpart(str, 0, width - 1) .. "…"
end

-- roam notes, newest first. logseq/ is an imported mirror, not notes edited here
local function roam_notes()
  local notes = {}
  for _, file in ipairs(vim.fn.globpath(roam_dir, "**/*.org", false, true)) do
    if not file:match("/logseq/") then
      notes[#notes + 1] = { file = file, mtime = vim.fn.getftime(file) }
    end
  end
  table.sort(notes, function(a, b) return a.mtime > b.mtime end)
  return notes
end

local function note_title(file)
  return roam_title(file) or vim.fn.fnamemodify(file, ":t:r")
end

local function recent_notes(limit)
  return function()
    local notes = roam_notes()
    local items = {}
    for i = 1, math.min(limit, #notes) do
      local file = notes[i].file
      items[#items + 1] = {
        icon = "󰎚 ",
        desc = truncate(note_title(file), 44),
        action = ":edit " .. vim.fn.fnameescape(file),
        autokey = true,
      }
    end
    return items
  end
end

-- own picker rather than org-roam's: its db load trips over duplicate ids in logseq/bak
local function pick_note()
  local items = {}
  for i, note in ipairs(roam_notes()) do
    items[i] = { text = note_title(note.file), file = note.file }
  end
  require("snacks").picker.pick({
    source = "roam_notes",
    title = "Roam Notes",
    items = items,
    format = "text",
  })
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
            { icon = " ", key = "x", desc = "Scratch Buffer", action = ":enew | setlocal buftype=nofile bufhidden=hide noswapfile" },
            { icon = " ", key = "c", desc = "Config", action = ":Telescope find_files cwd=" .. vim.fn.stdpath("config") },
            { icon = " ", key = "l", desc = "Lazy", action = ":Lazy" },
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
            filter = function(file) return not file:match("/%.git/") end,
            indent = 2,
            padding = 1,
          },
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
