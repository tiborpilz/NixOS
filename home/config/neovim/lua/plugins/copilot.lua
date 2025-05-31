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
  -- Chat with the Codebase/Agent
  {
    "CopilotC-Nvim/CopilotChat.nvim",
    dependencies = {
      { "github/copilot.vim" },                       -- or zbirenbaum/copilot.lua
      { "nvim-lua/plenary.nvim", branch = "master" }, -- for curl, log and async functions
    },
    build = "make tiktoken",                          -- Only on MacOS or Linux
    opts = {
      -- See Configuration section for options
    },
    -- See Commands section for default commands if you want to lazy load on them
  },
  -- {
  --   "yetone/avante.nvim",
  --   event = "VeryLazy",
  --   lazy = false,
  --   version = false, -- set this if you want to always pull the latest change
  --   opts = {
  --     provider = "openai",
  --     auto_suggestions_provider = "openai",
  --     dual_boost = {
  --       enabled = false,
  --       provider = "openai",
  --       second_provider = "openai",
  --     },
  --     -- add any opts here
  --   },
  --   -- if you want to build from source then do `make BUILD_FROM_SOURCE=true`
  --   build = "make",
  --   -- build = "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false" -- for windows
  --   dependencies = {
  --     "stevearc/dressing.nvim",
  --     "nvim-lua/plenary.nvim",
  --     "MunifTanjim/nui.nvim",
  --     --- The below dependencies are optional,
  --     "hrsh7th/nvim-cmp",          -- autocompletion for avante commands and mentions
  --     "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
  --     "zbirenbaum/copilot.lua",    -- for providers='copilot'
  --     {
  --       -- support for image pasting
  --       "HakonHarnes/img-clip.nvim",
  --       event = "VeryLazy",
  --       opts = {
  --         -- recommended settings
  --         default = {
  --           embed_image_as_base64 = false,
  --           prompt_for_file_name = false,
  --           drag_and_drop = {
  --             insert_mode = true,
  --           },
  --           -- required for Windows users
  --           use_absolute_path = true,
  --         },
  --       },
  --     },
  --     {
  --       -- Make sure to set this up properly if you have lazy=true
  --       'MeanderingProgrammer/render-markdown.nvim',
  --       opts = {
  --         file_types = { "markdown", "Avante" },
  --       },
  --       ft = { "markdown", "Avante" },
  --     },
  --   },
  -- },
}
