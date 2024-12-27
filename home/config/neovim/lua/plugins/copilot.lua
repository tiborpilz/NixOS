vim.api.nvim_create_user_command(
  'CopilotToggle',
  function () vim.g.copilot_enabled = not vim.g.copilot_enabled
    if vim.g.copilot_enabled then
      vim.cmd('Copilot disable')
      print("Copilot OFF")
    else
      vim.cmd('Copilot enable')
      print("Copilot ON")
    end
  end,
  {nargs = 0}
)

vim.keymap.set('i', '<C-Space>', 'copilot#Accept("\\<CR>")', { silent = true, expr = true, script = true })

return {
  {
    "github/copilot.vim",
    lazy = false,
    keys = {
      { "<leader>oc", "<cmd>CopilotToggle<cr>", desc = "Toggle Copilot" },
    }
  },
}
