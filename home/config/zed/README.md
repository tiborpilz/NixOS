# Zed keybinding translation

`keymap.json` translates the active Neovim mappings directly, retaining Space as
the leader and the existing functional prefixes (`b` buffers, `c` code, `d`
debug, `g` Git, `o` open, `p` project, `s` search, `t` test, and `w` window).
Doom Emacs was used only to confirm shared intent such as `SPC c g` for hover and
the debugger grouping.

## Material approximations

- Neovim buffers become Zed tabs/items. `SPC b b/p/n/k` therefore use the tab
  switcher and pane item actions.
- Zen Mode (`SPC T z` and `SPC z z`) becomes Zed's centered layout.
- Harpoon add/list (`SPC p h a/h`) becomes Zed bookmarks. Bookmarks are not an
  ordered quick-jump list.
- Overseer and Neotest use Zed tasks: `SPC p t` and `SPC p r` open the task
  picker, `SPC t r` runs the nearest runnable, `SPC t R` opens the task picker,
  and `SPC t l` reruns the last task. Test output maps to the terminal panel.
- `SPC t d` opens Zed's debugger because Zed has no generic "debug nearest
  test" action. `SPC t f` uses the alternate-file action rather than
  Projectionist's source/test heuristics.
- Neogit's log and Tardis time machine map to Zed file history. Neogit itself
  maps to the Git panel; branch diff/review and inline blame use native actions.
- Postilla's local review start/note bindings become view/add-labelled-bookmark.
  These share Zed's bookmark store with the Harpoon approximation and do not
  provide Postilla's session or export lifecycle.
- Claude Code and Sidekick panel/context actions map to Zed's Agent panel.
  Resume and continue open thread history, while add-buffer or add-file opens
  Zed's context picker. Diff accept/deny uses Agent keep/reject.
- Copilot toggle/accept maps to Zed edit predictions.

## Intentionally omitted

- Org-mode/org-roam agenda, capture, refile, note links, and TODO search.
- Neotest watch, attach, stop, summary, and its separate output views; these
  require project-specific Zed task definitions to reproduce accurately.
- Perfanno profiling, pipeline.nvim, UndoTree, Aider modified-file collection,
  and Sidekick session lifecycle/prompt routing. SnipRun maps to the nearest
  runnable and its close binding toggles the terminal panel.
- Review.nvim's typed note/suggestion/issue/praise variants, PR checkout flow,
  preview/export/clear operations, Postilla's remaining session/export workflow,
  and CodeReview PR/MR UI.
  Zed's native diff review comment submit/edit/delete and send-to-agent actions
  are retained under `SPC g c` where close counterparts exist.
- NeoTree's alternate sources, Trouble quickfix/location lists, Glance's popup
  presentation, and Harpoon's numbered list. Their underlying portable actions
  are covered by project panel, diagnostics, LSP navigation, and bookmarks.
