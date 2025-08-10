return {
  'jubnzv/mdeval.nvim',
  event = 'VeryLazy',
  opts = {
    -- Don't ask before executing code blocks
    require_confirmation = false,
    -- Change code blocks evaluation options.
    eval_options = {
      -- Set custom configuration for C++
      cpp = {
        command = { "clang++", "-std=c++20", "-O0" },
        default_header = [[
    #include <iostream>
    #include <vector>
    using namespace std;
      ]]
      },
      -- Add new configuration for Racket
      racket = {
        command = { "racket" },  -- Command to run interpreter
        language_code = "racket", -- Markdown language code
        exec_type = "interpreted", -- compiled or interpreted
        extension = "rkt",       -- File extension for temporary files
      },
      -- Add new configuration for Python
      python = {
        command = { "python3" },  -- Command to run interpreter
        language_code = "python", -- Markdown language code
        exec_type = "interpreted", -- compiled or interpreted
        extension = "py",         -- File extension for temporary files
      },
      -- Add new configuration for JavaScript
      javascript = {
        command = { "node" },     -- Command to run interpreter
        language_code = "javascript", -- Markdown language code
        exec_type = "interpreted", -- compiled or interpreted
        extension = "js",         -- File extension for temporary files
      },
      -- Add new configuration for TypeScript
      typescript = {
        command = { "bun" },  -- Command to run interpreter
        language_code = "typescript", -- Markdown language code
        exec_type = "interpreted", -- compiled or interpreted
        extension = "ts",         -- File extension for temporary files
      },
    },
  },
}
