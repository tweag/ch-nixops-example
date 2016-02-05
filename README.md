## Install NixOps ##

You need the Nix package manager installed (see [instructions
here](http://nixos.org/nix/)).

NixOps is installed with the following command:
```
nix-env -i nixops
```

## Configure Virtualbox ##

For NixOps to work properly, Virtualbox needs to be configured to have
a "host-only" network with a dhcp server. If this step is forgotten,
NixOps will fail with a variety of terrifying error messages.

In Virtualbox go to `File > Preferences > Network > Host-only
Networks`. Click the "Add" button. The "Host-only Networks" need to be
named "vboxnet0" (if you already have a network named "vboxnet0" use
it instead of creating a new one).

Click the "Edit" button, go to the `DHCP Server` tab. There enable the
server. The default configuration works fine, but sometimes it isn't filled automatically. In that case here is said default

- `Server address`: `192.168.56.100` (in which case the `IPv4
   Address` from the `Adapter` tab is `192.168.56.1).
- `Server Mask`: `255.255.255.0`.
- `Lower Address Bound`: `192.168.56.101`
- `Upper Address Bound`: `192.168.56.200`

> remark: there is also a command-line way, to be documented.

## Deploy the cluster ##

Create the deployment with
```
nixops create config-nixops.nix -d test-dp
```
This does not build anything, it only creates a global name for the
configuration file.

Build and deploy the VM cluster with:
```
nixops deploy -d test-dp
```
This command also makes an incremental update if the configuration or
the source have changed.

## Observe the trace ##

Log in to the `helloworlder` machine with
```
nixops ssh helloworlder -d test-dp
```

The traces are logged (with timestamps) in `hwer.log` in the home
directory (of the `helloworlder` machine).

## Turn the cluster off ##

The cluster can be turned off with
```
nixops stop -d test-dp
```
Alternatively, it can be torn down with
```
nixops destroy -d test-dp
```

After using `nixops destroy` the `test-dp` logical deployment still
exists though every associated resource have been cleaned up. To
completely remove it from NixOps memory use `nixops delete -d
test-dp`.

## Advanced ##

To hack around with GHCi:

```
nix-shell  # uses the shell.nix file
ghci
```

## References ##

- Examples of https://nixos.org/nixops/manual and https://nixos.org/wiki/NixOS:extend_NixOS (for services config).
