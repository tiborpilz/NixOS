---@diagnostic disable: missing-fields
return {
  {
    "mfussenegger/nvim-dap",
    lazy = true,
    dependencies = {
      "rcarriga/nvim-dap-ui",
    },
    config = function()
      local dap = require("dap")

      dap.adapters["pwa-node"] = {
        type = "server",
        host = "localhost",
        port = "${port}",
        executable = {
          command = "js-debug-adapter",
          args = { "${port}" },
        },
      }

      dap.adapters["pwa-firefox"] = {
        type = "server",
        host = "localhost",
        port = "${port}",
        executable = {
          command = "node",
          args = { vim.fn.stdpath('data') .. "/mason/packages/js-debug-adapter/dist/src/firefoxDebug.js", "${port}" },
        },
      }

      for _, language in ipairs({ "typescript", "javascript" }) do
        dap.configurations[language] = {
          {
            type = 'pwa-node',
            request = 'launch',
            name = 'Launch Current File (pwa-node)',
            cwd = "${workspaceFolder}", -- vim.fn.getcwd(),
            args = { '${file}' },
            sourceMaps = true,
            protocol = 'inspector',
          },
          {
            type = 'pwa-node',
            request = 'launch',
            name = 'Launch Current File (Typescript)',
            cwd = "${workspaceFolder}",
            runtimeArgs = { '--loader=ts-node/esm' },
            program = "${file}",
            runtimeExecutable = 'node',
            -- args = { '${file}' },
            sourceMaps = true,
            protocol = 'inspector',
            outFiles = { "${workspaceFolder}/**/**/*", "!**/node_modules/**" },
            skipFiles = { '<node_internals>/**', 'node_modules/**' },
            resolveSourceMapLocations = {
              "${workspaceFolder}/**",
              "!**/node_modules/**",
            },
          },
          {
            type = 'pwa-node',
            request = 'launch',
            name = 'Debug NestJS Application',
            cwd = "${workspaceFolder}",
            runtimeExecutable = 'npm',
            runtimeArgs = { 'run', 'start:dev', '--', '--debug', '--watch', '--inspect-brk' },
            autoAttachChildProcesses = true,
            restart = true,
            sourceMaps = true,
            stopOnEntry = false,
            console = 'integratedTerminal',
          },
          {
            type = 'pwa-firefox',
            request = 'launch',
            name = 'Launch Firefox',
          }
        }
      end

      require("dapui").setup({
        controls = {
          element = "repl",
          enabled = true,
          icons = {
            disconnect = "",
            pause = "",
            play = "",
            run_last = "",
            step_back = "",
            step_into = "",
            step_out = "",
            step_over = "",
            terminate = ""
          }
        },
        expand_lines = true,
        floating = {
          border = "single",
          mappings = {
            close = { "q", "<Esc>" }
          }
        },
        layouts = {
          {
            elements = {
              { title = "Stacks",      id = "stacks",      size = 0.25 },
              { title = "Watches",     id = "watches",     size = 0.25 },
              { title = "Breakpoints", id = "Breakpoints", size = 0.25 },
            },
            position = "right",
            size = 0.25,
          },
          {
            elements = {
              { title = "repl",   id = "repl",   size = 0.4 },
              { title = "Scopes", id = "scopes", size = 0.4 },
            },
            position = "bottom",
            size = 0.25,
          }
        },
        mappings = {
          expand = { "<CR>", "<2-LeftMouse>" },
          open = "o",
          remove = "d",
          edit = "e",
          repl = "r",
          toggle = "t",
        },
        render = {
          max_value_lines = 100,
          max_string_length = 100,
          max_num_lines = 10,
          indent = 2,
        },
      })

      local dapui = require("dapui")
      dap.listeners.before.attach["dapui_config"] = function()
        dapui.open()
      end
      dap.listeners.before.launch["dap-view-config"] = function()
        dapui.open()
      end
      dap.listeners.before.event_terminated["dapui_config"] = function()
        dapui.close()
      end
      dap.listeners.before.event_exited["dapui_config"] = function()
        dapui.close()
      end
    end,

    keys = {
      {
        "<leader>db",
        function()
          require("dap").toggle_breakpoint()
        end,
        desc = "Toggle Breakpoint",
      },
      {
        "<leader>dc",
        function()
          require("dap").continue()
        end,
        desc = "Continue",
      },
      {
        "<leader>di",
        function()
          require("dap").step_into()
        end,
        desc = "Step Into",
      },
      {
        "<leader>do",
        function()
          require("dap").step_out()
        end,
        desc = "Step Out",
      },
      {
        "<leader>dr",
        function()
          require("dap").repl.open()
        end,
        desc = "Open REPL",
      },
      {
        -- toggle the UI
        "<leader>dt",
        function()
          require("dapui").toggle()
        end,
      },
      {
        -- glance at value
        "<leader>dg",
        function()
          require("dapui").eval()
        end,
      },
      {
        -- Watch a variable
        "<leader>dw",
        function()
          require('dapui').elements.watches.add(vim.fn.expand('<cword>'))
        end,
      },
      {
        -- Overlay STack Trace
        "<leader>ds",
        function()
          require("dapui").float_element("stacks", { title = "Stack Trace", enter = true, max_height = 0.5 })
        end,
      }
    },
  },
  -- TODO: Decide between this and dap ui
  {
    "igorlfs/nvim-dap-view",
    ---@module 'dap-view'
    ---@type dapview.Config
    opts = {},
    keys = {
      {
        "<leader>dv",
        function()
          require("dap-view").toggle()
        end,
        desc = "Toggle DAP View",
      },
    },
  },

  -- Performance Analysis (TODO: Do these tools work with JS, etc.?)
  {
    "t-troebst/perfanno.nvim",
    event = "VeryLazy",
    config = function()
      local keymap = vim.api.nvim_set_keymap
      local opts = { noremap = true, silent = true }

      keymap("n", "<LEADER>dplf", ":PerfLoadFlat<CR>", opts)
      keymap("n", "<LEADER>dplg", ":PerfLoadCallGraph<CR>", opts)
      keymap("n", "<LEADER>dplo", ":PerfLoadFlameGraph<CR>", opts)

      keymap("n", "<LEADER>dpe", ":PerfPickEvent<CR>", opts)

      keymap("n", "<LEADER>dpa", ":PerfAnnotate<CR>", opts)
      keymap("n", "<LEADER>dpf", ":PerfAnnotateFunction<CR>", opts)
      keymap("v", "<LEADER>dpa", ":PerfAnnotateSelection<CR>", opts)

      keymap("n", "<LEADER>dpt", ":PerfToggleAnnotations<CR>", opts)

      keymap("n", "<LEADER>dph", ":PerfHottestLines<CR>", opts)
      keymap("n", "<LEADER>dps", ":PerfHottestSymbols<CR>", opts)
      keymap("n", "<LEADER>dpc", ":PerfHottestCallersFunction<CR>", opts)
      keymap("v", "<LEADER>dpc", ":PerfHottestCallersSelection<CR>", opts)
    end
  },
}
