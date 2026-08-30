/* Seeds the cache read by favicons.js. topSites is the only local source of
 * favicons for URLs that are not open tabs, and it is not exposed to content
 * scripts — hence the background context. It returns roughly a dozen hosts,
 * which is the cold start; favicons.js grows the rest as pages are visited. */
browser.topSites
  .get({ includeFavicon: true, limit: 100 })
  .then(function (sites) {
    return browser.storage.local.get("hushIcons").then(function (stored) {
      var cache = stored.hushIcons || {};
      sites.forEach(function (site) {
        if (!site.favicon) return;
        var host;
        try {
          host = new URL(site.url).hostname;
        } catch (e) {
          return;
        }
        if (!cache[host]) cache[host] = site.favicon;
      });
      return browser.storage.local.set({ hushIcons: cache });
    });
  });
