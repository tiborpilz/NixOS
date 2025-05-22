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


      require("dapui").setup()

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
    },
  }
}
