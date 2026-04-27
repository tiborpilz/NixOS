local SPLASH = "shader"

vim.g.have_nerd_font = true

local NORD_RAMP = {
  "#2e3440", -- polar night 0
  "#3b4252", -- polar night 1
  "#434c5e", -- polar night 2
  "#4c566a", -- polar night 3
  "#81a1c1", -- frost — mid blue
  "#e5e9f0", -- snow storm 1 foo
}

local function map_hex(hex)
  if type(hex) ~= "string" or hex == "NONE" or #hex < 7 then return hex end
  local r = tonumber(hex:sub(2, 3), 16)
  local g = tonumber(hex:sub(4, 5), 16)
  local b = tonumber(hex:sub(6, 7), 16)
  if not (r and g and b) then return hex end
  local lum = (0.299 * r + 0.587 * g + 0.114 * b) / 255
  local curved = lum ^ 0.7
  local idx = math.floor(curved * (#NORD_RAMP - 1) + 0.5) + 1
  return NORD_RAMP[math.max(1, math.min(#NORD_RAMP, idx))]
end

local function recolor(data)
  if not data.colors then return data end
  for _, frame in ipairs(data.colors) do
    for _, row in ipairs(frame) do
      for _, run in ipairs(row) do
        run[3] = map_hex(run[3])
        run[4] = map_hex(run[4])
      end
    end
  end
  return data
end

local function apply_dashboard_hl()
  vim.api.nvim_set_hl(0, "SnacksDashboardHeader",  { fg = "#3b4252" })
  vim.api.nvim_set_hl(0, "SnacksDashboardFooter",  { fg = "#4c566a", italic = true })
  vim.api.nvim_set_hl(0, "SnacksDashboardSpecial", { fg = "#4c566a", italic = true })
  vim.api.nvim_set_hl(0, "SnacksDashboardKey",     { fg = "#88c0d0" })
  vim.api.nvim_set_hl(0, "SnacksDashboardDesc",    { fg = "#d8dee9" })
  vim.api.nvim_set_hl(0, "SnacksDashboardIcon",    { fg = "#81a1c1" })
  vim.api.nvim_set_hl(0, "DashboardSubheader",     { fg = "#d8dee9" })
end

return {
  {
    "amansingh-afk/milli.nvim",
    lazy = false,
  },
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    dependencies = { "amansingh-afk/milli.nvim" },
    opts = function()
      local splash = require("milli").load({ splash = SPLASH })
      return {
        dashboard = {
          enabled = true,
          preset = {
            header = table.concat(splash.frames[1], "\n"),
            keys = {},
          },
          sections = {
            { section = "header", padding = 1 },
            {
              text = { { "n  e  o  v  i  m", hl = "DashboardSubheader" } },
              align = "center",
              padding = 1,
            },
            { section = "keys", gap = 1, padding = 1 },
          },
        },
      }
    end,
    config = function(_, opts)
      require("snacks").setup(opts)

      local data = vim.deepcopy(require("milli").load({ splash = SPLASH }))
      recolor(data)
      require("milli").snacks({ data = data, loop = true })

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

      vim.api.nvim_create_autocmd("User", {
        group = group,
        pattern = "SnacksDashboardOpened",
        callback = function()
          pcall(vim.api.nvim_clear_autocmds, {
            event = { "WinResized", "VimResized" },
            group = "snacks_dashboard",
          })
        end,
      })
    end,
  },
}
