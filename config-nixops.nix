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
          Type = "forking";
          User = "root";
          ExecStart = ''${hwer}/bin/${hwer.pname}'';
        };
      };
    };
}
