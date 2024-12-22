-- I'm used to opening and closing directories with the `<Tab>` key.
vim.cmd("autocmd FileType nerdtree map <buffer> <Tab> o")

return {
  {
    "preservim/nerdtree",
    keys = {
      {
        "<leader>op",
        function()
          vim.cmd(":NERDTreeToggle")
        end,
        desc = "Toggle NERDTree",
      },
    },
  },
}
