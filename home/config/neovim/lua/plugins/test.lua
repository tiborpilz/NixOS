return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-lua/plenary.nvim",
    "antoinemadec/FixCursorHold.nvim",
    "nvim-treesitter/nvim-treesitter",
    "nvim-neotest/neotest-jest",
  },
  event = "VeryLazy",
  lazy = true,
  config = function()
    local neotest = require("neotest")
    local neotest_jest = require("neotest-jest")

---@diagnostic disable-next-line: missing-fields
    neotest.setup({
      status = {
        enabled = true,
        virtual_text = true,
        signs = true,
      },
      output = {
        enabled = true,
        open_on_run = true,
      },
      config = function(_, opts)
        local neotest_ns = vim.api.nvim_create_namespace("neotest")
        vim.diagnostic.config({
          virtual_text = {
            format = function(diagnostic)
              -- Replace newline and tab characters with space for more compact diagnostics
              local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
              return message
            end,
          },
        }, neotest_ns)
      end,
      adapters = {
        require('neotest-jest')({
          jestCommand = "npm test --",
          jestConfigFile = "jest.config.ts",
          env = { CI = true },
          cwd = function(path)
            return vim.fn.getcwd()
          end,
        }),
      },
    })

    vim.keymap.set(
      'n',
      '<leader>tr',
      '<cmd>lua require("neotest").run.run()<cr>',
      { desc = 'Run nearest test' }
    )

    vim.keymap.set(
      'n',
      '<leader>tR',
      '<cmd>lua require("neotest").run.run(vim.fn.expand("%"))<cr>',
      { desc = 'Run all tests in file' }
    )

    vim.keymap.set(
      'n',
      '<leader>tl',
      '<cmd>lua require("neotest").run.run_last()<cr>',
      { desc = 'Run last test' }
    )

    vim.keymap.set(
      'n',
      '<leader>tw',
      '<cmd>lua require("neotest").watch.toggle()<cr>',
      { desc = 'Watch nearest test' }
    )

    vim.keymap.set(
      'n',
      '<leader>tW',
      '<cmd>lua require("neotest").watch.toggle(vim.fn.expand("%"))<cr>',
      { desc = 'Watch all tests in file' }
    )

    vim.keymap.set(
      'n',
      '<leader>tS',
      '<cmd>lua require("neotest").run.stop()<cr>',
      { desc = 'Stop running tests' }
    )

    vim.keymap.set(
      'n',
      '<leader>ta',
      '<cmd>lua require("neotest").run.attach()<cr>',
      { desc = 'Attach to test runner' }
    )

    vim.keymap.set(
      'n',
      '<leader>ts',
      '<cmd>lua require("neotest").summary.toggle()<cr>',
      { desc = 'Toggle test summary' }
    )

    vim.keymap.set(
      'n',
      '<leader>to',
      '<cmd>lua require("neotest").output.open({ enter = true, auto_close = true })<cr>',
      { desc = 'Open test output' }
    )

    vim.keymap.set(
      'n',
      '<leader>tO',
      '<cmd>lua require("neotest").output_panel.toggle()<cr>',
      { desc = 'Toggle test output panel' }
    )
  end,
}
