/* Page-side helpers for the hush theme.
 *
 * These share one file because tridactyl keys autocmds by event *and*
 * pattern: a second `autocmd DocStart .* ...` replaces the first rather than
 * adding to it, so two scripts on the same event would silently disable one
 * another. Run via `js -r`, which re-reads the file on every call.
 */

/* ── COMMAND LINE COLOUR SCHEME ─────────────────────────────────────── */
/*
 * The iframe's canvas is transparent only while its used color-scheme matches
 * the embedding page's; mismatched, Firefox paints it opaque and the pill's
 * backdrop-filter has nothing left to sample. Nothing inside the iframe can
 * read the page's scheme, so it is marked from here and hush.css keys off it.
 */
(function () {
  "use strict";

  /* The CSS property reads `normal` on pages that only declare a scheme in a
     meta tag — google.com is one — so fall back to the tag. */
  function pageIsDark() {
    var declared = getComputedStyle(document.documentElement).colorScheme;
    if (!declared || declared === "normal") {
      var meta = document.querySelector('meta[name="color-scheme"]');
      if (meta && meta.content) declared = meta.content;
    }
    if (!/dark/.test(declared)) return false;
    if (/only\s+dark/.test(declared)) return true;
    return matchMedia("(prefers-color-scheme: dark)").matches;
  }

  function mark(root, scheme) {
    if (root && root.dataset.hushScheme !== scheme) {
      root.dataset.hushScheme = scheme;
    }
  }

  function apply() {
    var scheme = pageIsDark() ? "dark" : "light";
    /* The page root carries it too: the mode indicator is a page-side element
       and shares --hush-blur, so it has to dim on light pages as well. */
    mark(document.documentElement, scheme);
    var frame = document.getElementById("cmdline_iframe");
    if (!frame) return;
    var doc = frame.contentDocument;
    if (doc) mark(doc.documentElement, scheme);
    if (frame.hushScheme) return;
    frame.hushScheme = true;
    frame.addEventListener("load", apply);
  }

  /* The iframe is created on first use and torn down on navigation. */
  new MutationObserver(apply).observe(document.documentElement, {
    childList: true,
  });
  apply();
})();

/* ── FAVICONS ───────────────────────────────────────────────────────── */
/*
 * Tridactyl only has favicons for open tabs, so `:tab` rows get one and
 * `:open` rows carry no icon cell at all. The page-icon: protocol, which
 * would resolve one out of places, is blocked inside the extension's own
 * page. So icons are cached here as data URLs keyed by hostname — seeded
 * from topSites by favicons-seed.js, and grown one host per visit. Hostname
 * rather than origin because history holds both schemes for the same site.
 */
(function () {
  "use strict";

  var KEY = "hushIcons";
  var MAX_ENTRIES = 400;
  var MAX_BYTES = 32768;

  function hostOf(url) {
    try {
      return new URL(url).hostname;
    } catch (e) {
      return null;
    }
  }

  /* Sites are routinely reached under more than one host — gitlab.com sends
     you to about.gitlab.com, www. comes and goes — so an exact hostname miss
     falls back to the registrable domain. The <=3 char test keeps compound
     suffixes like co.uk from collapsing every site under them into one. */
  function baseOf(host) {
    var parts = host.split(".");
    if (parts.length <= 2) return host;
    var tail = parts.slice(-2);
    if (tail[0].length <= 3 && tail[1].length <= 3) return parts.slice(-3).join(".");
    return tail.join(".");
  }

  function indexByBase(cache) {
    var byBase = {};
    Object.keys(cache).forEach(function (host) {
      var base = baseOf(host);
      if (!byBase[base]) byBase[base] = cache[host];
    });
    return byBase;
  }

  function lookup(cache, byBase, host) {
    if (!host) return null;
    return cache[host] || byBase[baseOf(host)] || null;
  }

  function save(cache) {
    var keys = Object.keys(cache);
    while (keys.length > MAX_ENTRIES) {
      delete cache[keys.shift()];
    }
    var entry = {};
    entry[KEY] = cache;
    return browser.storage.local.set(entry);
  }

  /* One fetch per origin, and only for origins not already known. The icon is
     normally still in the HTTP cache from the page that just displayed it. */
  function capture(cache) {
    var host = location.hostname;
    if (location.protocol !== "http:" && location.protocol !== "https:") {
      return Promise.resolve(false);
    }
    if (cache[host]) return Promise.resolve(false);

    var link = document.querySelector('link[rel~="icon"]');
    var href = link && link.href ? link.href : location.origin + "/favicon.ico";

    return fetch(href, { cache: "force-cache" })
      .then(function (res) {
        return res.ok ? res.blob() : Promise.reject(res.status);
      })
      .then(function (blob) {
        if (blob.size > MAX_BYTES || blob.type.indexOf("image/") !== 0) {
          return false;
        }
        return new Promise(function (resolve) {
          var reader = new FileReader();
          reader.onload = function () {
            cache[host] = reader.result;
            resolve(true);
          };
          reader.onerror = function () {
            resolve(false);
          };
          reader.readAsDataURL(blob);
        });
      })
      .catch(function () {
        return false;
      });
  }

  function decorate(doc, cache, byBase) {
    var rows = doc.querySelectorAll(
      ".HistoryCompletionSource tr.option, .BmarkCompletionSource tr.option"
    );
    for (var i = 0; i < rows.length; i++) {
      var row = rows[i];
      if (row.dataset.hushIcon) continue;

      var anchor = row.querySelector("a.url");
      var title = row.querySelector("td.title");
      var src = anchor && lookup(cache, byBase, hostOf(anchor.href));
      if (!src || !title) continue;

      var img = doc.createElement("img");
      img.className = "hush-favicon";
      img.src = src;
      title.insertBefore(img, title.firstChild);
      row.dataset.hushIcon = "1";
    }
  }

  function watchCompletions(frame, cache, byBase) {
    var doc = frame.contentDocument;
    if (!doc) return;
    var completions = doc.getElementById("completions");
    if (!completions || completions.hushWatched) return;
    completions.hushWatched = true;

    new MutationObserver(function () {
      decorate(doc, cache, byBase);
    }).observe(completions, { childList: true, subtree: true });
    decorate(doc, cache, byBase);
  }

  /* The command line iframe is created on first use and torn down on
     navigation, so catch it appearing rather than looking once. */
  function watchFrame(cache, byBase) {
    var attach = function () {
      var frame = document.getElementById("cmdline_iframe");
      if (!frame) return;
      watchCompletions(frame, cache, byBase);
      if (frame.hushBound) return;
      frame.hushBound = true;
      frame.addEventListener("load", function () {
        watchCompletions(frame, cache, byBase);
      });
    };

    new MutationObserver(attach).observe(document.documentElement, {
      childList: true,
    });
    attach();
  }

  browser.storage.local.get(KEY).then(function (stored) {
    var cache = stored[KEY] || {};
    watchFrame(cache, indexByBase(cache));
    capture(cache).then(function (added) {
      if (added) save(cache);
    });
  });
})();
