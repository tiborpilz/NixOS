local mod = require("neotree-lsp-symbols")
local I = mod._internal

describe("kind_info", function()
  it("returns icon, name, hl for known kinds", function()
    local k = I.kind_info(12)
    assert.equals("Function", k.name)
    assert.is_string(k.icon)
    assert.is_string(k.hl)
  end)

  it("falls back for unknown kinds", function()
    local k = I.kind_info(999)
    assert.equals("Unknown", k.name)
  end)
end)

describe("to_data", function()
  local sym = {
    name = "myFunc",
    kind = 12,
    range = { start = { line = 5, character = 0 }, ["end"] = { line = 10, character = 1 } },
    selectionRange = { start = { line = 5, character = 4 }, ["end"] = { line = 5, character = 10 } },
    children = {
      { name = "inner", kind = 13, range = { start = { line = 6, character = 2 }, ["end"] = { line = 6, character = 8 } } },
    },
  }

  it("produces lsp_symbol type with stable id", function()
    local d = I.to_data(sym, "/tmp/foo.lua", "/tmp/foo.lua", 1)
    assert.equals("lsp_symbol", d.type)
    assert.equals("/tmp/foo.lua", d.path)
    assert.equals("/tmp/foo.lua::1", d.id)
    assert.equals("myFunc", d.name)
  end)

  it("populates extra with kind metadata and ranges", function()
    local d = I.to_data(sym, "/tmp/foo.lua", "/tmp/foo.lua", 1)
    assert.equals(12, d.extra.kind)
    assert.equals("Function", d.extra.kind_name)
    assert.is_string(d.extra.kind_icon)
    assert.equals("/tmp/foo.lua", d.extra.file_path)
    assert.equals(5, d.extra.selection_range.start.line)
    assert.equals(4, d.extra.selection_range.start.character)
  end)

  it("recurses into children with derived ids", function()
    local d = I.to_data(sym, "/tmp/foo.lua", "/tmp/foo.lua", 1)
    assert.equals(1, #d.children)
    assert.equals("/tmp/foo.lua::1::1", d.children[1].id)
    assert.equals("inner", d.children[1].name)
  end)

  it("handles SymbolInformation shape (location instead of range)", function()
    local info_sym = {
      name = "X",
      kind = 5,
      location = {
        range = { start = { line = 1, character = 0 }, ["end"] = { line = 2, character = 0 } },
      },
    }
    local d = I.to_data(info_sym, "/tmp/x.lua", "/tmp/x.lua", 1)
    assert.equals(1, d.extra.range.start.line)
  end)
end)

describe("wrap_nodes", function()
  local sample_data = {
    {
      id = "a",
      name = "A",
      type = "lsp_symbol",
      path = "/tmp/foo.lua",
      extra = { kind = 5 },
      children = {
        { id = "a.1", name = "A.1", type = "lsp_symbol", path = "/tmp/foo.lua", extra = {}, children = {} },
        { id = "a.2", name = "A.2", type = "lsp_symbol", path = "/tmp/foo.lua", extra = {}, children = {} },
      },
    },
    {
      id = "b",
      name = "B",
      type = "lsp_symbol",
      path = "/tmp/foo.lua",
      extra = {},
      children = {},
    },
  }

  it("sets level on each node based on depth", function()
    local nodes = I.wrap_nodes(sample_data, 3)
    assert.equals(3, nodes[1].level)
    assert.equals(3, nodes[2].level)
  end)

  it("increments level for nested children", function()
    local nodes = I.wrap_nodes(sample_data, 2)
    -- NuiTree.Node stores children for later expansion via __children
    local first_child = nodes[1].__children[1]
    assert.equals(3, first_child.level)
  end)

  it("sets is_last_child correctly", function()
    local nodes = I.wrap_nodes(sample_data, 1)
    assert.is_false(nodes[1].is_last_child)
    assert.is_true(nodes[2].is_last_child)
  end)

  it("preserves extra and node fields on the wrapper", function()
    local nodes = I.wrap_nodes(sample_data, 1)
    assert.equals("A", nodes[1].name)
    assert.equals("lsp_symbol", nodes[1].type)
    assert.equals(5, nodes[1].extra.kind)
  end)

  it("produces NuiTree.Node objects with get_id", function()
    local nodes = I.wrap_nodes(sample_data, 1)
    assert.is_function(nodes[1].get_id)
  end)

  it("restores expanded state from the expanded set", function()
    local nodes = I.wrap_nodes(sample_data, 1, { a = true })
    assert.is_true(nodes[1]:is_expanded())
    assert.is_false(nodes[2]:is_expanded() or false)
  end)

  it("restores expanded state for nested nodes", function()
    local deeper = {
      {
        id = "x",
        name = "X",
        type = "lsp_symbol",
        path = "/tmp/foo.lua",
        extra = {},
        children = {
          {
            id = "x.1",
            name = "X.1",
            type = "lsp_symbol",
            path = "/tmp/foo.lua",
            extra = {},
            children = {
              { id = "x.1.1", name = "deep", type = "lsp_symbol", path = "/tmp/foo.lua", extra = {}, children = {} },
            },
          },
        },
      },
    }
    local nodes = I.wrap_nodes(deeper, 1, { ["x.1"] = true })
    local mid = nodes[1].__children[1]
    assert.is_true(mid:is_expanded())
  end)
end)

describe("parse_responses (LSP dedupe)", function()
  local function lsp_sym(name, kind)
    return {
      name = name,
      kind = kind,
      range = { start = { line = 0, character = 0 }, ["end"] = { line = 1, character = 0 } },
    }
  end

  it("returns empty list when no responses", function()
    assert.equals(0, #I.parse_responses(nil, "/tmp/x.lua"))
    assert.equals(0, #I.parse_responses({}, "/tmp/x.lua"))
  end)

  it("returns empty list when all results are empty", function()
    local responses = {
      [1] = { result = {} },
      [2] = { result = nil },
    }
    assert.equals(0, #I.parse_responses(responses, "/tmp/x.lua"))
  end)

  it("uses first non-empty response only — does not merge multiple clients", function()
    local responses = {
      [1] = { result = { lsp_sym("fromA", 12), lsp_sym("alsoA", 13) } },
      [2] = { result = { lsp_sym("fromB", 12) } },
    }
    local data = I.parse_responses(responses, "/tmp/x.lua")
    -- 2 symbols from first client, NOT 3 merged
    assert.equals(2, #data)
    -- both should be from the first client
    local names = { data[1].name, data[2].name }
    table.sort(names)
    assert.same({ "alsoA", "fromA" }, names)
  end)

  it("skips empty client and uses next non-empty", function()
    local responses = {
      [1] = { result = {} },
      [2] = { result = { lsp_sym("real", 12) } },
    }
    local data = I.parse_responses(responses, "/tmp/x.lua")
    assert.equals(1, #data)
    assert.equals("real", data[1].name)
  end)

  it("sorts symbols by source position regardless of LSP response order", function()
    local function s_at(name, line, char)
      return {
        name = name, kind = 12,
        range = { start = { line = line, character = char }, ["end"] = { line = line + 1, character = 0 } },
      }
    end
    local responses = {
      [1] = { result = { s_at("third", 50, 0), s_at("first", 1, 0), s_at("second", 10, 5) } },
    }
    local data = I.parse_responses(responses, "/tmp/x.lua")
    assert.equals("first", data[1].name)
    assert.equals("second", data[2].name)
    assert.equals("third", data[3].name)
  end)

  it("sorts nested children by position too", function()
    local function s_at(name, line, char, children)
      return {
        name = name, kind = 12,
        range = { start = { line = line, character = char }, ["end"] = { line = line + 1, character = 0 } },
        children = children,
      }
    end
    local responses = {
      [1] = { result = { s_at("parent", 0, 0, { s_at("c2", 5, 0), s_at("c1", 2, 0) }) } },
    }
    local data = I.parse_responses(responses, "/tmp/x.lua")
    assert.equals(2, #data[1].children)
    assert.equals("c1", data[1].children[1].name)
    assert.equals("c2", data[1].children[2].name)
  end)
end)

describe("kind_icon_component", function()
  it("returns text with icon and trailing space", function()
    local node = { extra = { kind_icon = "X", kind_name = "Function", kind_hl = "Function" } }
    local result = mod.kind_icon_component(nil, node, nil)
    assert.equals("X ", result.text)
    assert.equals("Function", result.highlight)
  end)

  it("falls back when extra is missing", function()
    local node = {}
    local result = mod.kind_icon_component(nil, node, nil)
    assert.equals("? ", result.text)
    assert.equals("NeoTreeFileIcon", result.highlight)
  end)

  it("uses default highlight when kind_hl is empty", function()
    local node = { extra = { kind_icon = "X", kind_hl = "" } }
    local result = mod.kind_icon_component(nil, node, nil)
    assert.equals("NeoTreeFileIcon", result.highlight)
  end)
end)

describe("loc_in", function()
  it("returns true for cursor inside the range", function()
    assert.is_true(I.loc_in({ 5, 10 }, { 5, 0 }, { 5, 20 }))
    assert.is_true(I.loc_in({ 6, 0 }, { 5, 0 }, { 7, 0 }))
  end)

  it("returns true on range boundaries", function()
    assert.is_true(I.loc_in({ 5, 0 }, { 5, 0 }, { 5, 20 }))
    assert.is_true(I.loc_in({ 5, 20 }, { 5, 0 }, { 5, 20 }))
  end)

  it("returns false outside range", function()
    assert.is_false(I.loc_in({ 4, 99 }, { 5, 0 }, { 7, 0 }))
    assert.is_false(I.loc_in({ 7, 1 }, { 5, 0 }, { 7, 0 }))
  end)

  it("returns false when range is missing", function()
    assert.is_false(I.loc_in({ 1, 0 }, nil, { 1, 0 }))
    assert.is_false(I.loc_in({ 1, 0 }, { 1, 0 }, nil))
  end)
end)

describe("find_symbol_at", function()
  -- Build a small NuiTree manually with file -> symbol -> nested-symbol
  local NuiTree = require("nui.tree")
  local mk = function(id, pos, end_pos, children)
    return NuiTree.Node({
      id = id, name = id, type = "lsp_symbol",
      extra = { position = pos, end_position = end_pos },
    }, children)
  end

  local function setup_tree()
    local bufnr = vim.api.nvim_create_buf(false, true)
    local tree = NuiTree({
      bufnr = bufnr,
      get_node_id = function(node) return node.id end,
      nodes = {
        NuiTree.Node({ id = "/foo.lua", name = "foo.lua", type = "file", path = "/foo.lua" }, {
          mk("s1", { 0, 0 }, { 10, 0 }, {
            mk("s1.1", { 2, 0 }, { 5, 0 }),
            mk("s1.2", { 6, 0 }, { 9, 0 }),
          }),
          mk("s2", { 11, 0 }, { 20, 0 }),
        }),
      },
    })
    -- expand the file node so children are visible to get_nodes
    local file_node = tree:get_node("/foo.lua")
    file_node:expand()
    local s1 = tree:get_node("s1")
    s1:expand()
    return tree, file_node
  end

  it("returns deepest matching symbol", function()
    local tree, file_node = setup_tree()
    local id = I.find_symbol_at(tree, file_node:get_id(), { 3, 5 })
    assert.equals("s1.1", id)
  end)

  it("returns mid-level symbol when no nested match", function()
    local tree, file_node = setup_tree()
    local id = I.find_symbol_at(tree, file_node:get_id(), { 10, 0 })
    -- s1 ends at 10,0 inclusive; nested children don't cover it
    assert.equals("s1", id)
  end)

  it("returns sibling symbol", function()
    local tree, file_node = setup_tree()
    local id = I.find_symbol_at(tree, file_node:get_id(), { 15, 0 })
    assert.equals("s2", id)
  end)

  it("returns nil when cursor is outside any symbol", function()
    local tree, file_node = setup_tree()
    local id = I.find_symbol_at(tree, file_node:get_id(), { 100, 0 })
    assert.is_nil(id)
  end)

  it("does not descend into collapsed symbols", function()
    -- s1 has children but is NOT expanded; cursor is inside s1.1's range
    local bufnr = vim.api.nvim_create_buf(false, true)
    local tree = NuiTree({
      bufnr = bufnr,
      get_node_id = function(node) return node.id end,
      nodes = {
        NuiTree.Node({ id = "/foo.lua", name = "foo.lua", type = "file", path = "/foo.lua" }, {
          mk("s1", { 0, 0 }, { 10, 0 }, {
            mk("s1.1", { 2, 0 }, { 5, 0 }),
          }),
        }),
      },
    })
    tree:get_node("/foo.lua"):expand()
    -- intentionally leave s1 collapsed
    local id = I.find_symbol_at(tree, "/foo.lua", { 3, 0 })
    assert.equals("s1", id)
  end)
end)

describe("KIND_ID_BY_NAME", function()
  it("maps kind names back to numeric ids", function()
    assert.equals(12, I.KIND_ID_BY_NAME.Function)
    assert.equals(5, I.KIND_ID_BY_NAME.Class)
  end)
end)

describe("TS_SYMBOL_NODES", function()
  it("uses kinds that exist in KIND_ID_BY_NAME", function()
    for node_type, info in pairs(I.TS_SYMBOL_NODES) do
      assert.is_number(I.KIND_ID_BY_NAME[info.kind],
        "kind '" .. info.kind .. "' for node '" .. node_type .. "' has no entry in KIND_ID_BY_NAME")
    end
  end)
end)

describe("KINDS table", function()
  it("covers all 26 LSP SymbolKind codes", function()
    for i = 1, 26 do
      assert.is_table(I.KINDS[i], "missing kind " .. i)
      assert.is_string(I.KINDS[i].icon)
      assert.is_string(I.KINDS[i].name)
    end
  end)
end)
