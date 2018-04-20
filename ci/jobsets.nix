{ nixpkgs, declInput }: let pkgs = import nixpkgs {}; in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
        "sv": {
            "enabled": 1,
            "hidden": false,
            "description": "sv",
            "nixexprinput": "sv",
            "nixexprpath": "./ci/ci.nix",
            "checkinterval": 300,
            "schedulingshares": 1,
            "enableemail": false,
            "emailoverride": "",
            "keepnr": 5,
            "inputs": {
                "sv": { "type": "git", "value": "https://github.com/qfpl/sv.git master", "emailresponsible": false },
                "nixpkgs": { "type": "git", "value": "https://github.com/NixOS/nixpkgs.git release-18.03", "emailresponsible": false }
            }
        }
    }
    EOF
  '';
}
