# Matrix — manual steps outside Nix

The Synapse homeserver itself is fully declared in
`modules/nixos/services/matrix.nix` and enabled in `../default.nix`. Two things
can't live in the Nix config and must be done by hand.

## 1. Secrets (`../secrets/secrets.yaml`)

Declaring `authentik.applications.matrix` auto-creates two sops secrets. Add
values for both — the *same* pair is consumed by Authentik (to configure the
OAuth2 provider) and by Synapse (to authenticate against it):

```
authentik_matrix_client_id: <random id>
authentik_matrix_client_secret: <random secret>
```

Generate them with e.g. `openssl rand -hex 32` (id) and `openssl rand -hex 48`
(secret), then:

```sh
sops hosts/nixos/klaus/secrets/secrets.yaml   # add the two keys above
```

Synapse's own `registration_shared_secret`, `macaroon_secret_key` and
`form_secret` are generated on the host on first boot
(`matrix-synapse-local-secrets.service`) — nothing to add for those.

## 2. Apex `.well-known` delegation (Cloudflare Worker)

`server_name` is `tiborpilz.xyz`, so users are `@you:tiborpilz.xyz`, but the
homeserver runs at `matrix.tiborpilz.xyz`. Federating servers and clients look
up the delegation files at the apex, which the Cloudflare Tunnel (`*.tiborpilz.xyz`
only) does not serve. Publish `matrix-wellknown-worker.js` at the edge:

1. Cloudflare dashboard → **Workers & Pages** → create a Worker, paste the
   contents of `matrix-wellknown-worker.js`, deploy.
2. Add a **Route**: `tiborpilz.xyz/.well-known/matrix/*` → this Worker.
   (Only these paths are intercepted; the apex static site is untouched.)

Verify:

```sh
curl https://tiborpilz.xyz/.well-known/matrix/server
# {"m.server":"matrix.tiborpilz.xyz:443"}
curl https://tiborpilz.xyz/.well-known/matrix/client
# {"m.homeserver":{"base_url":"https://matrix.tiborpilz.xyz"}}
```

Then run the Matrix Federation Tester against `tiborpilz.xyz`:
<https://federationtester.matrix.org/#tiborpilz.xyz>

## 3. Cloudflare WAF note

`matrix.tiborpilz.xyz` is excluded from Cloudflare Access (like Forgejo/Authentik) —
it must be, or federation and client callbacks break. If federation still fails,
add a WAF skip rule for `/_matrix/*` and `/_synapse/*` on `matrix.tiborpilz.xyz`
so Cloudflare's managed rules/bot-fight don't challenge server-to-server traffic.
