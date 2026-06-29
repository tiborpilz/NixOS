# infrastructure/

Kubernetes manifests for the **external cluster** — the one that runs
infrastructure-level workloads (monitoring, etc.), separate from the bare-metal
hosts that the rest of this flake configures.

NixOS does **not** manage that cluster. These files are the desired state,
expressed as [Argo CD](https://argo-cd.readthedocs.io/) `Application`
resources that reference upstream Helm charts with inline values. Argo CD then
reconciles them onto the cluster.

```
infrastructure/
└── monitoring/
    └── kube-prometheus-stack/
        └── application.yaml   # Prometheus Operator, Prometheus, Alertmanager, Grafana
```

> This is cluster/infrastructure monitoring. It is intentionally independent of
> `modules.services.monitoring` (the native NixOS Prometheus + Grafana that runs
> on the `klaus` host) — different target, different scope.

## Deploying

Either point an Argo CD app-of-apps at this directory, or apply an Application
directly:

```sh
kubectl apply -f infrastructure/monitoring/kube-prometheus-stack/application.yaml
```

Argo CD pulls the chart, renders the inline values, and syncs (auto-prune +
self-heal are on). The destination namespace (`monitoring`) is created
automatically.

## Before going to production

The Application ships conservative defaults; a few cluster-specific knobs are
left commented in `application.yaml`:

- **Persistence** — Prometheus/Alertmanager/Grafana are ephemeral until you set
  a `storageClassName` that exists on your cluster. Uncomment the
  `storageSpec` / `storage` / `persistence` blocks.
- **Grafana login** — the chart default is `admin` / `prom-operator`. Create a
  secret and reference it via `grafana.admin.existingSecret` instead of using
  the default.
- **Ingress** — Grafana's ingress is disabled. Set your ingress class and host
  to expose it.
- **Managed clusters** — on EKS/GKE/AKS the control plane isn't scrapeable;
  disable `kubeControllerManager` / `kubeScheduler` / `kubeEtcd` / `kubeProxy`
  to silence `TargetDown` alerts.

## Versioning

The chart version is pinned in `targetRevision`. Renovate's `argocd` manager
(see `renovate.json`) watches these manifests and opens PRs to bump it.
