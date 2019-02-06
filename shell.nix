{ compiler ? "ghc844" }:

(import ./. { inherit compiler; }).env
