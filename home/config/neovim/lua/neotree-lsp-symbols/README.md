# neotree-lsp-symbols

Inline LSP symbol expansion under file nodes in neo-tree, like treemacs in emacs.

Tab on a file expands its symbols as children. Tab on a symbol toggles its own children if it has any. Enter on a symbol jumps to its declaration. Switching buffers swaps the expanded file to match the active buffer.

`init.lua` holds the cache, the async LSP fetch, the `BEFORE_RENDER` hook that re-injects symbols after refresh, and the `toggle_symbols` / `open_symbol` / `follow` commands. The `KINDS` table at the top maps LSP SymbolKind codes to icons and highlight groups. Replace glyphs there if your nerd font is missing any.

Wired in from `lua/plugins/neotree.lua`.

## Notes

LSP only, no treesitter fallback. Files whose server doesn't implement `documentSymbol` get a notification instead of expanded symbols.

Errors from the render or fetch path get appended to `/tmp/neotree-lsp-symbols.log` and surfaced via `vim.notify`. Useful since noice can swallow plain error messages.
