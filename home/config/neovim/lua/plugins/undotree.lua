return {
  "jiaoshijie/undotree",
  opts = {
    parser = "compact",
  },
  keys = {
    { "<leader>su", "<cmd>lua require('undotree').toggle()<cr>", desc = "Toggle Undo Tree" },
  },
}
