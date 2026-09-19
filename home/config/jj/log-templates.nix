{
  # jj log      -> two-line "pretty" (compact header + description)
  # jj lo       -> one line per commit
  # jj ll       -> header, description, per-file diffstat
  templates.log = "pretty";
  templates.log_node = "pretty_node";

  template-aliases = {
    # "17 minutes ago" -> "17m"
    "ago(ts)" = ''
      stringify(ts.ago())
        .replace(regex:"^less than a second ago$", "now")
        .replace(regex:" seconds? ago$", "s")
        .replace(regex:" minutes? ago$", "m")
        .replace(regex:" hours? ago$", "h")
        .replace(regex:" days? ago$", "d")
        .replace(regex:" weeks? ago$", "w")
        .replace(regex:" months? ago$", "mo")
        .replace(regex:" years? ago$", "y")
    '';

    # Colour the conventional-commit type and scope, e.g. "feat(nvim)!:".
    "conv(s)" = ''
      replace(regex:"^([a-z]+)(\\([^)]*\\))?(!)?:", s, |c|
        label("conv " ++ c.get(1), c.get(1))
        ++ label("conv_scope", c.get(2))
        ++ label("conv_breaking", c.get(3))
        ++ label("conv " ++ c.get(1), ":")
      )
    '';

    # Links commit ids to the origin remote's web page (OSC 8). Only immutable commits:
    # mutable ones are mostly unpushed and would 404.
    "commit_link(c)" = ''
      if(git_web_url() && c.immutable(),
        hyperlink(git_web_url() ++ "/commit/" ++ c.commit_id(), c.commit_id().shortest(7)),
        c.commit_id().shortest(7))
    '';

    "id(c)" = ''
      label(
        separate(" ", if(c.divergent(), "divergent"), if(c.hidden(), "hidden")),
        c.change_id().shortest(4),
      )
    '';

    "refs(c)" = ''
      separate(" ",
        c.working_copies(),
        c.bookmarks().map(|b| label("bookmarks", "󰘬 " ++ b)),
        c.tags().map(|t| label("tags", "󰓹 " ++ t)),
      )
    '';

    # Author only when it is somebody else.
    "who(c)" = ''if(!c.mine(), label("author", c.author().email().local()))'';

    # One diff pass: parse the "N files changed, X insertions(+), Y deletions(-)" summary.
    "stat(c)" = ''
      if(!c.empty(), label("diffstat", replace(
        regex:"^(\\d+ files?) changed, (\\d+) insertions?\\(\\+\\), (\\d+) deletions?\\(-\\)$",
        stringify(c.diff().stat()).lines().last(),
        |m| label("added", "+" ++ m.get(2)) ++ " " ++ label("removed", "-" ++ m.get(3)) ++ " " ++ label("files", m.get(1)),
      )))
    '';

    # Verifying runs ssh-keygen per commit (~15ms each), so only when
    # ui.show-cryptographic-signatures is on; otherwise just mark signed commits.
    "sig(c)" = ''
      if(c.signature(),
        if(config("ui.show-cryptographic-signatures").as_boolean(),
          label("signature status " ++ c.signature().status(), coalesce(
            if(c.signature().status() == "good", "󰄬"),
            if(c.signature().status() == "unknown", "󰌆"),
            "󰅖",
          )),
          label("signature", "󰌆"),
        ))
    '';

    "flags(c)" = ''
      separate(" ",
        if(c.conflict(), label("conflict", "󰀦 conflict")),
        if(c.divergent(), label("divergent", "󰙁 divergent")),
        if(c.hidden(), label("hidden", "󰈉 hidden")),
      )
    '';

    "subject(c)" = ''
      separate(" ",
        if(c.empty(), label("empty", "∅")),
        if(c.description(),
          conv(c.description().first_line()),
          label(if(c.empty(), "empty"), description_placeholder),
        ),
      )
    '';

    "state(c)" = ''
      separate(" ",
        if(c.current_working_copy(), "working_copy"),
        if(c.immutable(), "immutable", "mutable"),
        if(c.conflict(), "conflicted"),
      )
    '';

    pretty = ''
      if(root, format_root_commit(self),
        label(state(self), concat(
          separate("  ",
            separate(" ", id(self), refs(self), flags(self)),
            label("meta", separate(" · ",
              who(self), label("timestamp", ago(committer.timestamp())),
              label("commit_id", commit_link(self)),
            )),
            sig(self),
          ) ++ "\n",
          separate("  ", subject(self), stat(self)) ++ "\n",
        ))
      )
    '';

    oneline = ''
      if(root, format_root_commit(self),
        label(state(self), separate(" ",
          id(self),
          refs(self),
          flags(self),
          subject(self),
          label("meta", ago(committer.timestamp())),
        ) ++ "\n")
      )
    '';

    detailed = ''
      if(root, format_root_commit(self),
        label(state(self), concat(
          separate("  ",
            separate(" ", id(self), refs(self), flags(self)),
            label("meta", separate(" · ",
              label("author", author.name()),
              label("timestamp", ago(committer.timestamp()) ++ " (" ++ committer.timestamp().local().format("%a %d %b %H:%M") ++ ")"),
              label("commit_id", commit_link(self)),
            )),
            sig(self),
          ) ++ "\n",
          subject(self) ++ "\n",
          if(description.lines().len() > 1,
            label("description body", indent("  ", description.lines().skip(1).join("\n")) ++ "\n")),
          if(!empty, label("meta", self.diff().stat().files().take(8).map(|f|
            "  " ++ label("diff " ++ f.status(), f.status_char()) ++ " "
            ++ f.display_diff_path() ++ " "
            ++ label("diffstat added", "+" ++ f.lines_added()) ++ " "
            ++ label("diffstat removed", "-" ++ f.lines_removed())
          ).join("\n") ++ "\n"
            ++ if(self.diff().files().len() > 8, "  … " ++ (self.diff().files().len() - 8) ++ " more\n"))),
        ))
      )
    '';

    pretty_node = ''
      coalesce(
        if(!self, label("elided", "󰇙")),
        label(
          separate(" ",
            if(current_working_copy, "working_copy"),
            if(immutable, "immutable", "mutable"),
            if(conflict, "conflicted"),
            if(divergent, "divergent"),
            if(hidden, "hidden"),
            if(empty && !current_working_copy, "empty"),
          ),
          coalesce(
            if(hidden, "󰈉"),
            if(conflict, "󰀦"),
            if(divergent, "󰙁"),
            if(current_working_copy, "󰐾"),
            if(immutable, "󰌾"),
            if(empty, "󰝦"),
            "󰝥",
          )
        )
      )
    '';

    # Same semantics without Nerd Font glyphs.
    plain_node = ''
      coalesce(
        if(!self, label("elided", "┊")),
        label(
          separate(" ",
            if(current_working_copy, "working_copy"),
            if(immutable, "immutable", "mutable"),
            if(conflict, "conflicted"),
            if(divergent, "divergent"),
            if(hidden, "hidden"),
            if(empty && !current_working_copy, "empty"),
          ),
          coalesce(
            if(hidden, "⊘"),
            if(conflict, "✕"),
            if(divergent, "◈"),
            if(current_working_copy, "@"),
            if(immutable, "◆"),
            if(empty, "◌"),
            "○",
          )
        )
      )
    '';
  };

  aliases = {
    lo = [ "log" "-T" "oneline" ];
    ll = [ "log" "-T" "detailed" ];
  };

  # ANSI names (not hex) so the palette follows the terminal's light/dark theme.
  colors = {
    change_id = { fg = "magenta"; bold = true; };
    "working_copy change_id" = { fg = "bright magenta"; bold = true; underline = true; };
    "meta" = "bright black";
    "meta author" = "yellow";
    "meta timestamp" = "cyan";
    "meta commit_id" = "bright black";
    "working_copy meta commit_id" = "blue";
    bookmarks = { fg = "green"; bold = true; };
    tags = { fg = "yellow"; bold = true; };
    working_copies = { fg = "cyan"; italic = true; };
    "description placeholder" = { fg = "yellow"; italic = true; };
    "empty description placeholder" = { fg = "bright black"; italic = true; };
    empty = "bright black";
    "immutable description" = "bright black";
    "description body" = { fg = "default"; italic = true; };
    elided = { fg = "bright black"; italic = true; };

    "conv feat" = { fg = "green"; bold = true; };
    "conv fix" = { fg = "red"; bold = true; };
    "conv chore" = "bright black";
    "conv docs" = "blue";
    "conv refactor" = "magenta";
    "conv perf" = "yellow";
    "conv test" = "cyan";
    "conv ci" = "cyan";
    "conv build" = "cyan";
    "conv style" = "blue";
    "conv wip" = { fg = "yellow"; italic = true; };
    conv_scope = { fg = "default"; italic = true; };
    conv_breaking = { fg = "red"; bold = true; };

    "diffstat added" = "green";
    "diffstat removed" = "red";
    "diffstat files" = "bright black";
    "signature" = "bright black";

    "node working_copy mutable" = { fg = "green"; bold = true; };
    "node mutable" = "blue";
    "node mutable empty" = "bright black";
    "node immutable" = "bright black";
    "node conflicted" = { fg = "red"; bold = true; };
    "node divergent" = { fg = "yellow"; bold = true; };
    "node hidden" = "bright black";
    "node elided" = "bright black";
  };
}
