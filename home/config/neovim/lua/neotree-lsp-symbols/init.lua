local NuiTree = require("nui.tree")

local M = {}

M.expanded_files = {}
M.expanded_symbols = {}
M.symbol_cache = {}

local LOG_PATH = "/tmp/neotree-lsp-symbols.log"

local function log(msg)
  local f = io.open(LOG_PATH, "a")
  if f then
    f:write(os.date("%H:%M:%S") .. " " .. tostring(msg) .. "\n")
    f:close()
  end
end

local function safe(fn, label)
  return function(...)
    local args = { ... }
    local ok, err = xpcall(function()
      return fn(unpack(args))
    end, debug.traceback)
    if not ok then
      log("ERROR in " .. label .. ": " .. tostring(err))
      vim.schedule(function()
        vim.notify("[neotree-lsp-symbols] " .. label .. " failed (see " .. LOG_PATH .. ")", vim.log.levels.ERROR)
      end)
    end
  end
end

local KINDS = {
  [1]  = { icon = "󰈙", name = "File",          hl = "Tag" },
  [2]  = { icon = "󰏗", name = "Module",        hl = "Exception" },
  [3]  = { icon = "󰌗", name = "Namespace",     hl = "Include" },
  [4]  = { icon = "󰏖", name = "Package",       hl = "Label" },
  [5]  = { icon = "󰠱", name = "Class",         hl = "Include" },
  [6]  = { icon = "󰊕", name = "Method",        hl = "Function" },
  [7]  = { icon = "󰜢", name = "Property",      hl = "@property" },
  [8]  = { icon = "󰜢", name = "Field",         hl = "@field" },
  [9]  = { icon = "󰒓", name = "Constructor",   hl = "@constructor" },
  [10] = { icon = "󰒻", name = "Enum",          hl = "@number" },
  [11] = { icon = "󰜰", name = "Interface",     hl = "Type" },
  [12] = { icon = "󰊕", name = "Function",      hl = "Function" },
  [13] = { icon = "󰀫", name = "Variable",      hl = "@variable" },
  [14] = { icon = "󰏿", name = "Constant",      hl = "Constant" },
  [15] = { icon = "󰀬", name = "String",        hl = "String" },
  [16] = { icon = "󰎠", name = "Number",        hl = "Number" },
  [17] = { icon = "󰨙", name = "Boolean",       hl = "Boolean" },
  [18] = { icon = "󰅪", name = "Array",         hl = "Type" },
  [19] = { icon = "󰅩", name = "Object",        hl = "Type" },
  [20] = { icon = "󰌋", name = "Key",           hl = "" },
  [21] = { icon = "󰟢", name = "Null",          hl = "Constant" },
  [22] = { icon = "󰉹", name = "EnumMember",    hl = "Number" },
  [23] = { icon = "󰙅", name = "Struct",        hl = "Type" },
  [24] = { icon = "󱐋", name = "Event",         hl = "Constant" },
  [25] = { icon = "󰆕", name = "Operator",      hl = "Operator" },
  [26] = { icon = "󰊄", name = "TypeParameter", hl = "Type" },
}

local function kind_info(k)
  return KINDS[k] or { icon = "? ", name = "Unknown" }
end

local KIND_ID_BY_NAME = {}
for id, info in pairs(KINDS) do
  KIND_ID_BY_NAME[info.name] = id
end

-- Treesitter node types we treat as symbols, with the kind to use and which
-- field on the node holds the symbol's name.
local TS_SYMBOL_NODES = {
  function_declaration  = { kind = "Function", name_field = "name" },
  function_definition   = { kind = "Function", name_field = "name" },
  function_expression   = { kind = "Function", name_field = "name" },
  function_item         = { kind = "Function", name_field = "name" },
  method_declaration    = { kind = "Method",   name_field = "name" },
  method_definition     = { kind = "Method",   name_field = "name" },
  class_declaration     = { kind = "Class",    name_field = "name" },
  class_definition      = { kind = "Class",    name_field = "name" },
  interface_declaration = { kind = "Interface",name_field = "name" },
  struct_declaration    = { kind = "Struct",   name_field = "name" },
  struct_item           = { kind = "Struct",   name_field = "name" },
  enum_declaration      = { kind = "Enum",     name_field = "name" },
  enum_item             = { kind = "Enum",     name_field = "name" },
  trait_item            = { kind = "Interface",name_field = "name" },
  type_alias_declaration = { kind = "TypeParameter", name_field = "name" },
  binding               = { kind = "Variable", name_field = "attrpath" },
}

local function ts_to_lsp_symbols(bufnr)
  local ok, parser = pcall(vim.treesitter.get_parser, bufnr)
  if not ok or not parser then return {} end
  local tree = (parser:parse() or {})[1]
  if not tree then return {} end
  local root = tree:root()

  local function walk(node)
    local results = {}
    for child in node:iter_children() do
      local info = TS_SYMBOL_NODES[child:type()]
      local entry
      if info then
        local name_nodes = child:field(info.name_field)
        local name_node = name_nodes and name_nodes[1]
        if name_node then
          local ok2, name = pcall(vim.treesitter.get_node_text, name_node, bufnr)
          if ok2 and name and name ~= "" then
            local sr, sc, er, ec = child:range()
            local nsr, nsc, ner, nec = name_node:range()
            entry = {
              name = name,
              kind = KIND_ID_BY_NAME[info.kind] or 13,
              range = { start = { line = sr, character = sc }, ["end"] = { line = er, character = ec } },
              selectionRange = { start = { line = nsr, character = nsc }, ["end"] = { line = ner, character = nec } },
              children = {},
            }
          end
        end
      end
      local sub = walk(child)
      if entry then
        entry.children = sub
        table.insert(results, entry)
      else
        for _, s in ipairs(sub) do
          table.insert(results, s)
        end
      end
    end
    return results
  end

  return walk(root)
end

local function to_pos(p)
  return p and { p.line, p.character } or nil
end

local function start_pos(sym)
  local r = sym.range or (sym.location and sym.location.range)
  if not r then return { math.huge, math.huge } end
  return { r.start.line, r.start.character }
end

local function sorted_by_pos(list)
  local out = {}
  for i, v in ipairs(list or {}) do out[i] = v end
  table.sort(out, function(a, b)
    local pa, pb = start_pos(a), start_pos(b)
    if pa[1] ~= pb[1] then return pa[1] < pb[1] end
    return pa[2] < pb[2]
  end)
  return out
end

local function to_data(sym, file_path, parent_id, idx)
  local id = parent_id .. "::" .. tostring(idx)
  local children = {}
  for i, child in ipairs(sorted_by_pos(sym.children)) do
    table.insert(children, to_data(child, file_path, id, i))
  end
  local range = sym.range or (sym.location and sym.location.range)
  local sel = sym.selectionRange or range
  local k = kind_info(sym.kind)
  return {
    id = id,
    name = sym.name,
    type = "lsp_symbol",
    path = file_path,
    children = children,
    extra = {
      kind = sym.kind,
      kind_name = k.name,
      kind_icon = k.icon,
      kind_hl = k.hl,
      range = range,
      selection_range = sel,
      position = range and to_pos(range.start) or nil,
      end_position = range and to_pos(range["end"]) or nil,
      file_path = file_path,
    },
  }
end

local function loc_le(a, b)
  if a[1] < b[1] then return true end
  if a[1] > b[1] then return false end
  return a[2] <= b[2]
end

local function loc_in(loc, p_start, p_end)
  return p_start and p_end and loc_le(p_start, loc) and loc_le(loc, p_end) or false
end

local function find_symbol_at(tree, parent_id, loc)
  local children = tree:get_nodes(parent_id)
  for _, child in ipairs(children) do
    local extra = child.extra or {}
    if extra.position and extra.end_position and loc_in(loc, extra.position, extra.end_position) then
      if child:is_expanded() then
        local deeper = find_symbol_at(tree, child:get_id(), loc)
        if deeper then return deeper end
      end
      return child:get_id()
    end
  end
  return nil
end

local function find_file_node(tree, path)
  local function walk(parent_id)
    for _, node in ipairs(tree:get_nodes(parent_id)) do
      if node.type == "file" and node.path == path then
        return node
      end
      if node.type == "directory" and node:is_expanded() then
        local found = walk(node:get_id())
        if found then return found end
      end
    end
    return nil
  end
  return walk()
end

local function wrap_nodes(data_list, level, expanded)
  expanded = expanded or {}
  local nodes = {}
  for i, data in ipairs(data_list) do
    local is_last = i == #data_list
    local children = data.children or {}
    local wrapped_children = #children > 0 and wrap_nodes(children, level + 1, expanded) or nil
    local node_data = {
      id = data.id,
      name = data.name,
      type = data.type,
      path = data.path,
      extra = data.extra,
      level = level,
      is_last_child = is_last,
    }
    local node = NuiTree.Node(node_data, wrapped_children)
    if expanded[data.id] then
      node:expand()
    end
    table.insert(nodes, node)
  end
  return nodes
end

local function load_buffer(path)
  local bufnr = vim.fn.bufnr(path)
  if bufnr == -1 or not vim.api.nvim_buf_is_loaded(bufnr) then
    bufnr = vim.fn.bufadd(path)
    vim.fn.bufload(bufnr)
    vim.api.nvim_buf_call(bufnr, function()
      if vim.bo.filetype == "" then
        vim.cmd("filetype detect")
      end
    end)
  end
  return bufnr
end

local function has_symbol_provider(bufnr)
  local clients = vim.lsp.get_clients and vim.lsp.get_clients({ bufnr = bufnr })
    or vim.lsp.get_active_clients({ bufnr = bufnr })
  for _, c in ipairs(clients) do
    if c.server_capabilities and c.server_capabilities.documentSymbolProvider then
      return true
    end
  end
  return false
end

local function parse_responses(responses, path)
  responses = responses or {}
  local keys = {}
  for k in pairs(responses) do
    table.insert(keys, k)
  end
  table.sort(keys)
  local first
  for _, k in ipairs(keys) do
    local resp = responses[k]
    if resp and resp.result and #resp.result > 0 then
      first = resp.result
      break
    end
  end
  local data = {}
  if first then
    for i, s in ipairs(sorted_by_pos(first)) do
      table.insert(data, to_data(s, path, path, i))
    end
  end
  return data
end

function M.fetch(path, cb)
  if M.symbol_cache[path] then
    cb(M.symbol_cache[path])
    return
  end
  local bufnr = load_buffer(path)

  local function fallback_treesitter()
    local pseudo = ts_to_lsp_symbols(bufnr)
    local data = {}
    for i, s in ipairs(sorted_by_pos(pseudo)) do
      table.insert(data, to_data(s, path, path, i))
    end
    M.symbol_cache[path] = data
    cb(data)
  end

  local function do_request()
    vim.lsp.buf_request_all(
      bufnr,
      "textDocument/documentSymbol",
      { textDocument = vim.lsp.util.make_text_document_params(bufnr) },
      function(responses)
        local data = parse_responses(responses, path)
        if #data > 0 then
          M.symbol_cache[path] = data
          cb(data)
        else
          fallback_treesitter()
        end
      end
    )
  end

  if has_symbol_provider(bufnr) then
    do_request()
    return
  end

  local attempts = 0
  local timer = vim.loop.new_timer()
  timer:start(100, 100, vim.schedule_wrap(function()
    attempts = attempts + 1
    if has_symbol_provider(bufnr) then
      timer:stop(); timer:close()
      do_request()
    elseif attempts >= 15 then
      timer:stop(); timer:close()
      fallback_treesitter()
    end
  end))
end

local function inject_children(tree, file_node, symbol_data)
  local base_level = (file_node.level or 0) + 1
  local wrapped = wrap_nodes(symbol_data, base_level, M.expanded_symbols)
  for _, child in ipairs(wrapped) do
    tree:add_node(child, file_node:get_id())
  end
  file_node:expand()
end

-- Hook AFTER_RENDER, not BEFORE_RENDER. BEFORE fires on the soon-to-be-replaced
-- old tree, so any injection there gets thrown away when show_nodes builds the
-- new tree from sourceItems (which doesn't contain our symbols).
M.after_render = safe(function(state)
  if not state.tree then return end
  local need_redraw = false
  local function walk(parent_id)
    local nodes = state.tree:get_nodes(parent_id)
    for _, node in ipairs(nodes) do
      if node.type == "directory" and node:is_expanded() then
        walk(node:get_id())
      elseif node.type == "file" and M.expanded_files[node.path] then
        if #state.tree:get_nodes(node:get_id()) == 0 then
          local symbols = M.symbol_cache[node.path]
          if symbols and #symbols > 0 then
            inject_children(state.tree, node, symbols)
            need_redraw = true
          end
        elseif not node:is_expanded() then
          node:expand()
          need_redraw = true
        end
      end
    end
  end
  walk()
  if need_redraw then
    require("neo-tree.ui.renderer").redraw(state)
  end
end, "after_render")

M.toggle_symbols = safe(function(state)
  local node = state.tree:get_node()
  if not node then return end

  if node.type == "directory" then
    require("neo-tree.sources.filesystem").toggle_directory(state, node)
    return
  end

  if node.type == "lsp_symbol" then
    if node:has_children() then
      if node:is_expanded() then
        node:collapse()
        M.expanded_symbols[node:get_id()] = nil
      else
        node:expand()
        M.expanded_symbols[node:get_id()] = true
      end
      require("neo-tree.ui.renderer").redraw(state)
    end
    return
  end

  if node.type == "file" then
    local path = node.path
    local renderer = require("neo-tree.ui.renderer")

    if M.expanded_files[path] then
      M.expanded_files[path] = nil
      node:collapse()
      renderer.redraw(state)
      return
    end

    M.expanded_files[path] = true
    M.fetch(path, safe(function(symbols)
      vim.schedule(function()
        if #symbols == 0 then
          M.expanded_files[path] = nil
          vim.notify("[neo-tree] no LSP symbols for " .. vim.fn.fnamemodify(path, ":t"), vim.log.levels.INFO)
          return
        end
        local cur = state.tree:get_node(node:get_id())
        if not cur then
          renderer.redraw(state)
          return
        end
        if #state.tree:get_nodes(cur:get_id()) == 0 then
          inject_children(state.tree, cur, symbols)
        else
          cur:expand()
        end
        renderer.redraw(state)
      end)
    end, "fetch_callback"))
  end
end, "toggle_symbols")

M.open_symbol = safe(function(state)
  local node = state.tree:get_node()
  if not node then return end
  if node.type ~= "lsp_symbol" then
    require("neo-tree.sources.common.commands").open(state, function() end)
    return
  end
  local extra = node.extra or {}
  local file = extra.file_path or node.path
  local utils = require("neo-tree.utils")
  local winid = utils.get_appropriate_window(state)
  vim.api.nvim_set_current_win(winid)
  vim.cmd.edit(vim.fn.fnameescape(file))
  if extra.selection_range and extra.selection_range.start then
    local pos = extra.selection_range.start
    pcall(vim.api.nvim_win_set_cursor, winid, {
      (pos.line or 0) + 1,
      pos.character or 0,
    })
  end
end, "open_symbol")

M.follow_cursor = safe(function()
  local bufnr = vim.api.nvim_get_current_buf()
  if vim.bo[bufnr].buftype ~= "" then return end
  if vim.bo[bufnr].filetype == "neo-tree" then return end
  local path = vim.api.nvim_buf_get_name(bufnr)
  if path == "" or not M.expanded_files[path] then return end
  local cursor = vim.api.nvim_win_get_cursor(0)
  local loc = { cursor[1] - 1, cursor[2] }
  local manager = require("neo-tree.sources.manager")
  local state = manager.get_state("filesystem")
  if not state or not state.tree then return end
  local file_node = find_file_node(state.tree, path)
  if not file_node then return end
  local node_id = find_symbol_at(state.tree, file_node:get_id(), loc)
  if node_id then
    require("neo-tree.ui.renderer").focus_node(state, node_id, true)
  end
end, "follow_cursor")

local follow_cursor_timer
function M.follow_cursor_debounced()
  if follow_cursor_timer then
    follow_cursor_timer:stop()
    pcall(follow_cursor_timer.close, follow_cursor_timer)
  end
  follow_cursor_timer = vim.uv.new_timer()
  follow_cursor_timer:start(80, 0, vim.schedule_wrap(function()
    M.follow_cursor()
    pcall(follow_cursor_timer.close, follow_cursor_timer)
    follow_cursor_timer = nil
  end))
end

function M.kind_icon_component(_, node, _)
  local extra = node.extra or {}
  local hl = extra.kind_hl
  if hl == nil or hl == "" then hl = "NeoTreeFileIcon" end
  return {
    text = (extra.kind_icon or "?") .. " ",
    highlight = hl,
  }
end

M._internal = {
  KINDS = KINDS,
  KIND_ID_BY_NAME = KIND_ID_BY_NAME,
  kind_info = kind_info,
  to_data = to_data,
  wrap_nodes = wrap_nodes,
  parse_responses = parse_responses,
  find_symbol_at = find_symbol_at,
  loc_in = loc_in,
  ts_to_lsp_symbols = ts_to_lsp_symbols,
  TS_SYMBOL_NODES = TS_SYMBOL_NODES,
}

return M
