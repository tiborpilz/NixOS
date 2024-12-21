require('lsp_signature').setup({
  bind = true,
  floating_window = false,
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

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local bufnr = args.buf
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if vim.tbl_contains({ 'null-ls' }, client.name) then  -- blacklist lsp
      return
    end
    require("lsp_signature").on_attach({
      -- ... setup options here ...
    }, bufnr)
  end,
})
