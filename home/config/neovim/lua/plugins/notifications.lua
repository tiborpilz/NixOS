local BACKDROP_BLEND = 85

local function open_backdrop(reference_win)
  local cfg = vim.api.nvim_win_get_config(reference_win)
  local zindex = cfg.zindex or 50
  local buf = vim.api.nvim_create_buf(false, true)
  local win = vim.api.nvim_open_win(buf, false, {
    relative = "editor",
    row = 0,
    col = 0,
    width = vim.o.columns,
    height = vim.o.lines,
    focusable = false,
    style = "minimal",
    zindex = math.max(1, zindex - 1),
  })
  vim.api.nvim_set_hl(0, "NoiceBackdrop", { bg = "#000000", default = true })
  vim.wo[win].winhighlight = "Normal:NoiceBackdrop"
  vim.wo[win].winblend = BACKDROP_BLEND
  vim.bo[buf].buftype = "nofile"
  return win, buf
end

local function find_noice_window()
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    local cfg = vim.api.nvim_win_get_config(win)
    if cfg.relative ~= "" then
      local hl = vim.wo[win].winhighlight or ""
      if hl:find("Noice") then return win end
    end
  end
end

local backdrops = {}
local function attach_backdrop(key)
  if backdrops[key] then return end
  vim.schedule(function()
    local target = find_noice_window()
    if not target then return end
    local win, buf = open_backdrop(target)
    backdrops[key] = { win = win, buf = buf }
  end)
end

local function detach_backdrop(key)
  local b = backdrops[key]
  if not b then return end
  backdrops[key] = nil
  pcall(vim.api.nvim_win_close, b.win, true)
  pcall(vim.api.nvim_buf_delete, b.buf, { force = true })
end

local borderless = {
  border = {
    style = { " ", " ", " ", " ", " ", " ", " ", " " },
    padding = { 0, 1 },
  },
  win_options = {
    winhighlight = {
      Normal = "NormalFloat",
      FloatBorder = "NormalFloat",
    },
  },
}

return {
  {
    "folke/snacks.nvim",
    opts = {
      notifier = {
        enabled = false,
        timeout = 3000,
        style = "compact",
        top_down = true,
      },
      styles = {
        notification = {
          border = "none",
          wo = {
            winhighlight = "Normal:NormalFloat,FloatBorder:NormalFloat,FloatTitle:NormalFloat",
            winblend = 0,
          },
        },
        notification_history = {
          border = "none",
          wo = {
            winhighlight = "Normal:NormalFloat,FloatBorder:NormalFloat,FloatTitle:NormalFloat",
          },
        },
      },
    },
  },
  {
    "folke/noice.nvim",
    event = "VeryLazy",
    dependencies = {
      "MunifTanjim/nui.nvim",
      "folke/snacks.nvim",
    },
    opts = {
      cmdline = {
        enabled = true,
        view = "cmdline_popup",
      },
      popupmenu = { enabled = false },
      messages = {
        enabled = true,
        view = "notify",
        view_error = "notify",
        view_warn = "notify",
        view_history = "messages",
        view_search = false,
      },
      notify = { enabled = true, view = "notify" },
      lsp = {
        progress = { enabled = false },
        hover = { enabled = false },
        signature = { enabled = false },
        message = { enabled = false },
      },
      views = {
        cmdline_popup = vim.tbl_deep_extend("force", borderless, {
          position = { row = "40%", col = "50%" },
          size = { width = 60, height = "auto" },
        }),
        confirm = vim.tbl_deep_extend("force", borderless, {
          position = { row = "40%", col = "50%" },
        }),
      },
      routes = {
        {
          filter = { event = "msg_show", kind = "search_count" },
          opts = { skip = true },
        },
        {
          filter = { event = "msg_show", find = "written" },
          opts = { skip = true },
        },
      },
      presets = {
        bottom_search = false,
        command_palette = false,
        long_message_to_split = true,
      },
    },
    config = function(_, opts)
      vim.notify = require("snacks").notifier.notify
      require("noice").setup(opts)

      local group = vim.api.nvim_create_augroup("NoiceBackdrop", { clear = true })
      vim.api.nvim_create_autocmd("User", {
        group = group,
        pattern = { "NoiceCmdlinePopupShow", "NoiceConfirmShow" },
        callback = function(args) attach_backdrop(args.match) end,
      })
      vim.api.nvim_create_autocmd("User", {
        group = group,
        pattern = { "NoiceCmdlinePopupHide", "NoiceConfirmHide" },
        callback = function(args)
          local key = args.match:gsub("Hide$", "Show")
          detach_backdrop(key)
        end,
      })
    end,
  },
}
