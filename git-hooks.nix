let
  sources = import ./nix/sources.nix {};
  nix-pre-commit-hooks = import sources.preCommitHooksNix;
in {
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    # If your hooks are intrusive, avoid running on each commit with a default_states like this:
    # default_stages = ["manual" "push"];
    hooks = {
      # elm-format.enable = true;
      # ormolu.enable = true;
      shellcheck.enable = true;
    };
  };
}