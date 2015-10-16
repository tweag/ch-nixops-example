{
  network.description = "Haskell + NixOps FTW!";
  helloworlder = { config, pkgs, ... }:
    let hwer = import ./shell.nix {}; in
    { #services.httpd.enable = true;
      #environment.systemPackages = [ hwer ];

      deployment.targetEnv = "virtualbox";
      deployment.virtualbox.headless = true;

      systemd.services.hwer = {
        wantedBy = ["multi-user.target"];
        serviceConfig = {
          Type = "forking";
          User = "root";
          ExecStart = ''${hwer}/bin/${hwer.pname}'';
        };
      };
    };
}
