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

-- Accept copilot suggestion with <C-Space>
-- `replace_keycodes` is important to not insert the key code in the buffer
vim.keymap.set(
  'i',
  '<C-Space>', 'copilot#Accept("<CR>")',
  { noremap = true, silent = true, expr = true, replace_keycodes = false, desc = "Copilot accept" }
)


return {
  {
    "github/copilot.vim",
    lazy = false,
    keys = {
      { "<leader>oc", "<cmd>CopilotToggle<cr>", desc = "Toggle Copilot" },
    }
  },
}
