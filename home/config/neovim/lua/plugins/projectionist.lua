return {
  "tpope/vim-projectionist",
  config = function()
    vim.g.projectionist_heuristics = {
      ["test/*.test.ts"] = {
        type = "test"
      },
      ["*"] = {
        ["src/*.ts"] = {
          type      = "source",
          alternate = {
            "test/{}.test.ts",
            "test/**/{}.test.ts",
          },
        },
        ["src/*.tsx"] = {
          type      = "source",
          alternate = {
            "test/{}.test.ts",
            "test/**/{}.test.ts",
          },
        },
        ["src/*.vue"] = {
          type      = "source",
          alternate = {
            "test/{}.test.ts",
            "test/**/{}.test.ts",
          },
        },
        ["test/*.test.ts"] = {
          type      = "test",
          alternate = {
            "src/{}.vue",
            "src/{}.ts",
            "src/{}.tsx",
            "src/**/{}.vue",
            "src/**/{}.ts",
            "src/**/{}.tsx",
          },
        },
      },
    }

    vim.keymap.set(
      "n",
      "<leader>tf",
      "<cmd>A<CR>",
      { desc = "Switch Test File" }
    )
  end,
}
