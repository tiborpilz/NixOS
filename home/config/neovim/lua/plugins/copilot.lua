vim.api.nvim_create_user_command(
  'CopilotToggle',
  function()
    vim.g.copilot_enabled = not vim.g.copilot_enabled
    if vim.g.copilot_enabled then
      vim.cmd('Copilot disable')
      print("Copilot OFF")
    else
      vim.cmd('Copilot enable')
      print("Copilot ON")
    end
  end,
  { nargs = 0 }
)

-- Accept copilot suggestion with <C-Space>
-- `replace_keycodes` is important to not insert the key code in the buffer
vim.keymap.set(
  'i',
  '<C-Space>', 'copilot#Accept("<CR>")',
  { noremap = true, silent = true, expr = true, replace_keycodes = false, desc = "Copilot accept" }
)


return {
  -- Regular Autocomplete
  {
    "github/copilot.vim",
    lazy = false,
    keys = {
      { "<leader>oc", "<cmd>CopilotToggle<cr>", desc = "Toggle Copilot" },
    }
  },
  {
    "yetone/avante.nvim",
    build = vim.fn.has("win32") ~= 0
    and "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false"
    or "make",
    event = "VeryLazy",
    version = false, -- Never set this value to "*"! Never!
    ---@module 'avante'
    ---@type avante.Config
    opts = {
      instructions_file = "avante.md",
      provider = "copilot",
      providers = {
        claude = {
          endpoint = "https://api.anthropic.com",
          model = "claude-sonnet-4-20250514",
          timeout = 30000, -- Timeout in milliseconds
          extra_request_body = {
            temperature = 0.75,
            max_tokens = 20480,
          },
        },
      },
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      "nvim-mini/mini.pick",
      "nvim-telescope/telescope.nvim",
      "hrsh7th/nvim-cmp",
      "ibhagwan/fzf-lua",
      "stevearc/dressing.nvim",
      "folke/snacks.nvim",
      "nvim-tree/nvim-web-devicons",
      "zbirenbaum/copilot.lua",
      {
        -- support for image pasting
        "HakonHarnes/img-clip.nvim",
        event = "VeryLazy",
        opts = {
          -- recommended settings
          default = {
            embed_image_as_base64 = false,
            prompt_for_file_name = false,
            drag_and_drop = {
              insert_mode = true,
            },
            -- required for Windows users
            use_absolute_path = true,
          },
        },
      },
    },
  },
}
