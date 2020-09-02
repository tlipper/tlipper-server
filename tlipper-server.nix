{ mkDerivation, aeson, aeson-casing, amazonka, amazonka-core
, amazonka-s3, amazonka-s3-streaming, attoparsec, base, bytestring
, conduit, containers, directory, dotenv, esqueleto, exceptions
, filepath, hpack, http-client, http-client-tls, http-types, lens
, lifted-async, monad-control, monad-logger, mtl, persistent
, persistent-postgresql, persistent-template
, privileged-concurrency, process, prometheus, regex-base
, regex-posix, servant, servant-client, servant-server, stdenv
, text, time, transformers, typed-duration, unix
, unordered-containers, wai, wai-cors, wai-logger, warp
}:
mkDerivation {
  pname = "tlipper-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-casing amazonka amazonka-core amazonka-s3
    amazonka-s3-streaming attoparsec base bytestring conduit containers
    directory dotenv esqueleto exceptions filepath http-client
    http-client-tls http-types lens lifted-async monad-control
    monad-logger mtl persistent persistent-postgresql
    persistent-template privileged-concurrency process prometheus
    regex-base regex-posix servant servant-client servant-server text
    time transformers typed-duration unix unordered-containers wai
    wai-cors wai-logger warp
  ];
  prePatch = "hpack";
  homepage = "https://github.com/yigitozkavci/tlipper-server#readme";
  license = stdenv.lib.licenses.bsd3;
}
