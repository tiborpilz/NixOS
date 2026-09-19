return vim.list_extend(
  vim.list_extend(require("plugins.vcs.git"), require("plugins.vcs.jj")),
  require("plugins.vcs.review")
)
