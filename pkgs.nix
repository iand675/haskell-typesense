{
  extras = hackage:
    { packages = { typesense-client = ./typesense-client.nix; }; };
  resolver = "lts-18.2";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }