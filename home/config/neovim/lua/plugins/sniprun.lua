return {
  {
    "michaelb/sniprun",
    branch = "master",

    build = "sh install.sh",
    -- do 'sh install.sh 1' if you want to force compile locally
    -- (instead of fetching a binary from the github release). Requires Rust >= 1.65

    config = function()
      require("sniprun").setup({
        display = {
          "Classic",       --# display results in the command-line  area
          -- "VirtualText",             --# display results as virtual text
          -- "VirtualLine",             --# display results as virtual lines
          "TempFloatingWindow",      --# display results in a floating window
          -- "LongTempFloatingWindow",  --# same as above, but only long results. To use with VirtualText[Ok/Err]
          "Terminal",                --# display results in a vertical split
          "TerminalWithCode",        --# display results and code history in a vertical split
          -- "NvimNotify",              --# display with the nvim-notify plugin
          -- "Api"                      --# return output to a programming interface
        },
        repl_enable = {
          "Python3_fifo", --# Enable REPL for Python3_fifo
          "JS_TS_deno",    --# Enable REPL for JS_TS_bun
        },
        live_display = { "VirtualText", "TerminalOk" },
        live_mode_toggle = "enable",
        inline_messages = false,
        selected_interpreters = {
          "JS_TS_deno",
          "Python3_fifo",
        },
      })
    end,
    keys = {
      { "<leader>sr", "<cmd>:SnipRun<cr>",      mode = { "n" },    desc = "SnipRun" },
      { "<leader>sr", "<cmd>:'<,'>SnipRun<cr>", mode = { "v" },    desc = "SnipRun" },
      { "<leader>sc", "<Plug>SnipClose",        desc = "SnipClose" },
    },
  }
}
