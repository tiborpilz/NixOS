require('lsp_signature').setup({
  bind = true,
  floating_window = true,
  doc_lines = 2,
  use_lspsaga = true,
  padding = ' ',
  shadow_blend = 36,
  shadow_guibg = "green",
  floating_window_above_cur_line = false,
  handler_opts = {
    border = "single"
  }
})
