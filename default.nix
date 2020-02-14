{ mkDerivation, base
, pure-core, pure-dom, pure-default, pure-lifted, pure-json, pure-txt, stdenv
, containers
}:
mkDerivation {
pname = "pure-render";
version = "0.8.0.0";
src = ./.;
libraryHaskellDepends = [ base pure-core pure-dom pure-default pure-lifted pure-json pure-txt stdenv containers ];
homepage = "github.com/grumply/pure-render";
license = stdenv.lib.licenses.bsd3;
}
