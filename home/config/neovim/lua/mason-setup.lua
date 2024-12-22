local mason = require("mason")
local mason_nvim_dap = require("mason-nvim-dap")

mason_nvim_dap.setup({
  ensure_installed = {
    "node2",
    "firefox",
  },
  automatic_installation = true,
  handlers = {
    function(config)
      mason_nvim_dap.default_setup(config)
    end,
  },
})

require("nvim-dap-virtual-text").setup({
  commented = true,
})
