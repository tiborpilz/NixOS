local function file_exists(file)
  local f = io.open(file, "rb")
  if f then f:close() end
  return f ~= nil
end


local load_neovim_text = function()
  local config_path = vim.fn.stdpath("config")
  local ascii_text_file = config_path .. "/dashboard/neovim_slanted.txt"

  if not file_exists(ascii_text_file) then return { "NEOVIM" } end
  local lines = {}
  for line in io.lines(ascii_text_file) do
    lines[#lines + 1] = line
  end
  return lines
end

local load_neovim_logo = function()
  local config_path = vim.fn.stdpath("config")
  local ascii_text_file = config_path .. "/dashboard/neovim_logo_48.txt"

  if not file_exists(ascii_text_file) then return { "NEOVIM" } end
  local lines = {}
  for line in io.lines(ascii_text_file) do
    lines[#lines + 1] = line
  end
  return lines
end

local generate_random_neovim_text = function()
  local figlist_file = assert(io.popen("figlist"))
  local figlist_output = figlist_file:read('*all')

  local figlist = {}

  for line in figlist_output:gmatch("[^\r\n]+") do
    -- if line doesn't contain ":" we're good to go
    if not line:find(":") then
      figlist[#figlist + 1] = line
    end

    -- If line starts with figlet control, we're done
    if line:find("Control files") then
      break
    end
  end

  math.randomseed(os.time())
  print(math.random(#figlist))

  local font = figlist[math.random(#figlist)]

  local file = assert(io.popen("figlet -f " .. font .. " NEOVIM"))
  local output = file:read('*all')
  file:close()
  return output
end

local generate_slanted_neovim_text = function()
  local file = assert(io.popen("figlet -f peaksslant NEOVIM"))
  local output = file:read('*all')
  file:close()
  return output
end
  -- get config data
  -- local config_path = vim.fn.stdpath("config")
  -- local ascii_text_file = config_path .. "/dashboard/neovim_slanted.txt"
  -- local f = io.open(ascii_text_file, "r")

vim.g.have_nerd_font = true

return {
  -- Dasbhoard
  {
    "goolord/alpha-nvim",
    dependencies = {
      "echasnovski/mini.icons",
      { "juansalvatore/git-dashboard-nvim", dependencies = { "nvim-lua/plenary.nvim" } },
      { "MaximilianLloyd/ascii.nvim", dependencies = { "MunifTanjim/nui.nvim" } },
      { "MarcHamamji/ascii-text.nvim", dependencies = { "nvim-lua/plenary.nvim" } },
    },
    config = function()
      local dashboard = require("alpha.themes.dashboard")

      local neovim_logo = load_neovim_logo()

      dashboard.section.buttons.val = {
        -- Browser projects
        dashboard.button("SPC p f", "  Files", ":Telescope frecency workspace=CWD<CR>", { desc = "Find Files" }),
        dashboard.button("SPC f h", "  Help", ":Telescope help_tags<CR>", { desc = "Help" }),
        dashboard.button("SPC n a", "󰃭  Agenda", "<Cmd>lua require('orgmode').action('agenda.prompt')<CR>", { desc = "Agenda" }),
      }


      dashboard.section.header.val = neovim_logo
      dashboard.section.subheader = {
        type = "text",
        val = "n  e  o  v  i  m",
        opts = {
          position = "center",
          hl = "Type",
        },
      }

      dashboard.config.opts.margin = 0
      dashboard.config.layout = {
        { type = "padding", val = 0 },
        dashboard.section.header,
        { type = "padding", val = 3 },
        dashboard.section.subheader,
        { type = "padding", val = 8 },
        dashboard.section.buttons,
      }

      require("alpha").setup(
        dashboard.config
      )
    end,
  },
}
