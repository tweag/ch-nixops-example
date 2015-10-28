{
  network.description = "Haskell + NixOps FTW!";

  helloworlder = { config, pkgs, ... }:
    let hwer = pkgs.haskellPackages.callPackage (import ./.) {};
    in
    { 
      deployment.targetEnv = "virtualbox";
      deployment.virtualbox.headless = true;

      #environment.systemPackages = [ hwer ];

      systemd.services.hwer = {
        wantedBy = ["multi-user.target"];
        serviceConfig = {
          Type = "simple";  # See
                             # http://www.freedesktop.org/software/systemd/man/systemd.service.html#Options for Type
          User = "root";
          ExecStart = ''${hwer}/bin/${hwer.pname}'';
        };
      };
      networking.firewall.enable = false;
    };

  ponger = { config, pkgs, ... }:
    let hwer = pkgs.haskellPackages.callPackage (import ./.) {};
    in
    {
      deployment.targetEnv = "virtualbox";
      deployment.virtualbox.headless = true;

      systemd.services.hwernode = {
        wantedBy = ["multi-user.target"];
        serviceConfig = {
          Type = "simple";
          User = "root";
          ExecStart = ''${hwer}/bin/${hwer.pname} --node'';
        };
      };
      networking.firewall.enable = false;
    };
}
