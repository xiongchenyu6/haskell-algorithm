with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
    name = "myEnv";
      buildInputs = [ curl libgnurl ];
  }