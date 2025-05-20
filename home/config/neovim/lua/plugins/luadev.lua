return {
  "bfredl/nvim-luadev",
  lazy = true,
  config = function()
    require("luadev").setup()
  end,
}
