-- Hovercraft provider for progressively expandable TypeScript types.
-- Talks to vtsls' quickinfo command with a verbosityLevel (TypeScript 5.9+),
-- so type expansion sits behind <leader>cg next to LSP, Man and Diagnostics
-- instead of owning a float of its own.

local M = {}

M.id = 'TS Type'

local state = {
  verbosity = 0,
  can_expand = false,
  bufnr = nil,
  pos = nil,
}

local function vtsls_client(bufnr)
  return vim.lsp.get_clients({ bufnr = bufnr, name = 'vtsls' })[1]
end

-- quickinfo returns documentation and tag text as SymbolDisplayPart[] or a plain string
local function flatten(val)
  if type(val) == 'string' then return val end
  if type(val) ~= 'table' then return nil end

  local out = {}
  for _, part in ipairs(val) do
    if part.text then out[#out + 1] = part.text end
  end
  return table.concat(out)
end

local function build_lines(body)
  if not body or not body.displayString then
    return { '-- no type information --' }
  end

  local lines = { '```typescript' }
  vim.list_extend(lines, vim.split(body.displayString, '\n', { plain = true }))
  lines[#lines + 1] = '```'

  local doc = flatten(body.documentation)
  if doc and doc ~= '' then
    lines[#lines + 1] = ''
    vim.list_extend(lines, vim.split(doc, '\n', { plain = true }))
  end

  for _, tag in ipairs(body.tags or {}) do
    local text = vim.split(flatten(tag.text) or '', '\n', { plain = true })
    lines[#lines + 1] = ''
    lines[#lines + 1] = string.format('**@%s** %s', tag.name, text[1] or '')
    for i = 2, #text do
      lines[#lines + 1] = text[i]
    end
  end

  -- hovercraft owns the window, so the depth hint has to live in the content
  local hints = {}
  if body.canIncreaseVerbosityLevel then hints[#hints + 1] = '`+` expand' end
  if state.verbosity > 0 then hints[#hints + 1] = '`-` collapse' end
  if #hints == 0 then hints[#hints + 1] = '`max`' end

  lines[#lines + 1] = ''
  lines[#lines + 1] = string.format('*depth %d*  %s', state.verbosity, table.concat(hints, '  '))

  return lines
end

local Provider = {}
Provider.__index = Provider

function Provider:is_enabled(opts)
  return vtsls_client(opts.bufnr) ~= nil
end

function Provider:execute(opts, done)
  local client = vtsls_client(opts.bufnr)
  if not client then
    done({ lines = { '-- vtsls is not attached --' }, filetype = 'markdown' })
    return
  end

  -- Hovering somewhere new starts over at depth 0. expand/collapse re-enter
  -- here with the cursor unmoved, which is what carries the depth across.
  local pos = opts.pos
  if state.bufnr ~= opts.bufnr
      or not state.pos
      or state.pos[1] ~= pos[1]
      or state.pos[2] ~= pos[2] then
    state.verbosity = 0
  end
  state.bufnr = opts.bufnr
  state.pos = { pos[1], pos[2] }

  local params = {
    command = 'typescript.tsserverRequest',
    arguments = {
      'quickinfo',
      {
        file = vim.api.nvim_buf_get_name(opts.bufnr),
        line = pos[1],       -- hovercraft row is already 1-indexed
        offset = pos[2] + 1, -- col is 0-indexed, tsserver offset is 1-indexed
        verbosityLevel = state.verbosity,
      },
    },
  }

  -- Every path below has to call done() or hovercraft's async provider wrapper
  -- waits forever and the popup never opens.
  client:request('workspace/executeCommand', params, function(err, result)
    local body = not err and result and result.body or nil
    state.can_expand = (body and body.canIncreaseVerbosityLevel) or false

    if not body then
      -- most likely TypeScript < 5.9, which has no verbosityLevel
      state.verbosity = 0
      done({ lines = { '-- quickinfo unavailable (TypeScript 5.9+ required) --' }, filetype = 'markdown' })
      return
    end

    done({ lines = build_lines(body), filetype = 'markdown' })
  end, opts.bufnr)
end

function M.new()
  return setmetatable({}, Provider)
end

local function is_active()
  local ok, hovercraft = pcall(require, 'hovercraft')
  if not ok then return false end

  local ui = hovercraft.ui
  return ui ~= nil
      and ui.window_config ~= nil
      and ui.window_config.active_provider == M.id
end

-- + and - stay armed on the source buffer for as long as any hovercraft popup is
-- open, so when another provider is on screen they have to fall back to their
-- normal motions rather than swallow the key.
local function rehover(delta, fallback)
  if not is_active() then
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(fallback, true, false, true), 'n', false)
    return
  end

  local next_verbosity = state.verbosity + delta
  if next_verbosity < 0 then return end
  if delta > 0 and not state.can_expand then return end

  state.verbosity = next_verbosity
  require('hovercraft').hover({ current_provider = M.id })
end

function M.expand()
  rehover(1, '+')
end

function M.collapse()
  rehover(-1, '-')
end

return M
