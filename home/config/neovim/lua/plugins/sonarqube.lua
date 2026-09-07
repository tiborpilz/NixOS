-- SonarQube / SonarLint real-time static analysis.
-- Wraps the `sonarlint-language-server` (installed by mason-tool-installer in
-- languages.lua) and surfaces issues as Neovim diagnostics + code actions.
-- Repo: https://github.com/iamkarasik/sonarqube.nvim
return {
  {
    "iamkarasik/sonarqube.nvim",
    event = "VeryLazy",
    config = function()
      -- Path inside the mason-managed sonarlint-language-server package.
      local extension_path =
        vim.fn.stdpath("data") .. "/mason/packages/sonarlint-language-server/extension"
      local analyzers = extension_path .. "/analyzers"

      require("sonarqube").setup({
        lsp = {
          cmd = {
            vim.fn.exepath("java"),
            "-jar",
            extension_path .. "/server/sonarlint-ls.jar",
            "-stdio",
            "-analyzers",
            analyzers .. "/sonargo.jar",
            analyzers .. "/sonarhtml.jar",
            analyzers .. "/sonariac.jar",
            analyzers .. "/sonarjava.jar",
            analyzers .. "/sonarjavasymbolicexecution.jar",
            analyzers .. "/sonarjs.jar",
            analyzers .. "/sonarphp.jar",
            analyzers .. "/sonarpython.jar",
            analyzers .. "/sonartext.jar",
            analyzers .. "/sonarxml.jar",
          },
          handlers = {
            -- Open the rule description in the browser instead of the raw
            -- HTML payload the server sends by default.
            ["sonarlint/showRuleDescription"] = function(_, res)
              local uri = "https://rules.sonarsource.com/%s/RSPEC-%s"
              local spec = string.match(res.key, "S(%d+)")
              vim.ui.open(string.format(uri, res.languageKey, spec))
            end,
          },
        },
        rules = { enabled = true },
        go = { enabled = true },
        html = { enabled = true },
        iac = { enabled = true }, -- Docker / CloudFormation / Terraform / YAML
        javascript = { enabled = true, clientNodePath = vim.fn.exepath("node") },
        php = { enabled = true },
        python = { enabled = true },
        text = { enabled = true },
        xml = { enabled = true },
        -- java/csharp need an extra JDK / omnisharp on the PATH; enable once
        -- those are wired up if/when needed.
      })
    end,
  },
}
