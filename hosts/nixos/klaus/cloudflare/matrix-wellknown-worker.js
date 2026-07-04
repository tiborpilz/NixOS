// Matrix federation/client delegation for the apex domain (tiborpilz.xyz).
//
// server_name is `tiborpilz.xyz` but the homeserver actually lives at
// `matrix.tiborpilz.xyz`. Federating servers and clients discover that by
// fetching these two files from the *apex*. The apex isn't served by the
// NixOS Caddy (the Cloudflare Tunnel only routes `*.tiborpilz.xyz`), so we
// answer them at the edge with a Worker instead of touching the apex origin.
//
// Deploy: bind this Worker to the route  tiborpilz.xyz/.well-known/matrix/*
// (see README.md). Only those paths are intercepted; the rest of the apex is
// left untouched.

const SERVER = { "m.server": "matrix.tiborpilz.xyz:443" };
const CLIENT = {
  "m.homeserver": { "base_url": "https://matrix.tiborpilz.xyz" },
};

function json(body) {
  return new Response(JSON.stringify(body), {
    headers: {
      "Content-Type": "application/json",
      // Clients fetch /client cross-origin during autodiscovery.
      "Access-Control-Allow-Origin": "*",
    },
  });
}

export default {
  async fetch(request) {
    const { pathname } = new URL(request.url);
    if (pathname === "/.well-known/matrix/server") return json(SERVER);
    if (pathname === "/.well-known/matrix/client") return json(CLIENT);
    return new Response("Not found", { status: 404 });
  },
};
