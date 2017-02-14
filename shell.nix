let pkgs = import <nixpkgs> {};
in pkgs.stdenv.mkDerivation {
  name = "devEnv";
  buildInputs = with pkgs; let pp = python27Packages; in [
    sbt
    python27
    pp.websocket_client
    pp.sexpdata
  ];

  BROWSER = "firefox";

  SBT_OPTS = "-Xss2M -XX:MaxMetaspaceSize=1024m";
}
