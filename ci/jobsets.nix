{ nixpkgs, declInput }: let pkgs = import nixpkgs {}; in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
        "separated-values": {
            "enabled": 1,
            "hidden": false,
            "description": "separated-values",
            "nixexprinput": "separated-values",
            "nixexprpath": "./ci/ci.nix",
            "checkinterval": 300,
            "schedulingshares": 1,
            "enableemail": false,
            "emailoverride": "",
            "keepnr": 5,
            "inputs": {
                "separated-values": { "type": "git", "value": "https://github.com/qfpl/separated-values.git master", "emailresponsible": false },
                "nixpkgs": { "type": "git", "value": "https://github.com/NixOS/nixpkgs.git release-17.03", "emailresponsible": false }
            }
        }
    }
    EOF
  '';
}
