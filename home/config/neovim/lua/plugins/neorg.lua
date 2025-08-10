return {
  {
    "vhyrro/luarocks.nvim",
    priority = 1000, -- Very high priority is required, luarocks.nvim should run as the first plugin in your config.
    config = true,
  },
  {
    "nvim-neorg/neorg",
    lazy = false,
    version = "v9.3.0", -- Pin Neorg to the latest stable release
    config = true,
  },
}
