return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-lua/plenary.nvim",
    "antoinemadec/FixCursorHold.nvim",
    "nvim-treesitter/nvim-treesitter",
    "nvim-neotest/neotest-jest",
    "marilari88/neotest-vitest",
  },
  event = "VeryLazy",
  config = function()
    --- @diagnostic disable-next-line: missing-fields
    require("neotest").setup({
      status = {
        enabled = true,
        virtual_text = false,
        signs = true,
      },
      output = {
        enabled = true,
        open_on_run = true,
      },
      running = {
        concurrent = true,
      },
      discovery = {
        enabled = false,
        concurrent = 16,
      },
      config = function(_, opts)
        local neotest = require("neotest")

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
        require("neotest-jest")({
          jestCommand = "npm test --",
          jestConfigFile = "jest.config.ts",
          env = { CI = true },
          cwd = function()
            return vim.fn.getcwd()
          end,
        }),
        require("neotest-vitest")({
          cwd = function(testFilePath) return vim.fs.root(testFilePath, "node_modules") end,
          filter_dir = function(name, rel_path, root) return name ~= "node_modules" end,
        }),
      },
      consumers = {
        overseer = require("neotest.consumers.overseer"),
      },
      overseer = {
        enabled = true,
        force_default = true,
        task_list = {
          name = "Tests",
          filter = function(task)
            return task:has_tags("test")
          end,
        },
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
      '<leader>td',
      '<cmd>lua require("neotest").run.run({ strategy = "dap" })<cr>',
      { desc = 'Debug nearest test' }
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
