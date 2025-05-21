return {
  {
    'https://gitlab.com/gitlab-org/editor-extensions/gitlab.vim.git',
    event = { 'BufReadPre', 'BufNewFile' },
    ft = { 'go', 'javascript', 'typescript', 'html', 'css', 'scss', 'less', 'json', 'yaml', 'markdown' },
    opts = {
      statusline = {
        enabled = true,
      },
    },
  },
}
