require('neorg').setup {
  load = {
    ["core.defaults"] = {}, -- loads default behavior
    ["core.concealer"] = {}, -- allows for use of pretty icons
    ["core.dirman"] = { -- manages Neorg workspaces
      config = {
        workspace = {
          notes = "~/neorg",
        },
      },
    },
  },
}
