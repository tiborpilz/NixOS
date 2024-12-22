local neotest = require("neotest")
local neotest_jest = require("neotest-jest")

neotest.setup({
  adapters = {
    neotest_jest({}),
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
  '<leader>tT',
  '<cmd>lua require("neotest").run.run(vim.fn.expand("%"))<cr>',
  { desc = 'Run all tests in file' }
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
