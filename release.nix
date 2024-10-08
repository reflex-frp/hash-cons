let hash-cons = (import ./.).hash-cons;
in {
  inherit (hash-cons) checks;
  inherit (hash-cons.components) library;
  shell = import ./shell.nix;
}
