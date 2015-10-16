Following the examples of https://nixos.org/nixops/manual.

First install NixOps:

```
nix-env -i nixops
```

Create the deployment:

```
nixops create config-nixops.nix -d test-dp
```

To deploy the VMs at first, and to update them everytime the config or the source
is changed:

```
nixops deploy -d test-dp
```
