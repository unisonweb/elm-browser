# Unison Codebase Explorer

The Unison Codebase Explorer is an Elm application and companion Haskell server
for exploring your Unison codebase. It is very alpha-quality software, but you
can still use it in anger. Pull requests welcome and appreciated :)

## Building

To build, you need `elm` 0.19+ and either `cabal-install` 2.4+ or `stack`.

* Generate the `index.html` JavaScript blob.

      elm make client-src/Main.elm

* If you are using `cabal`, make sure you've configured it to install
  executables somewhere on your `$PATH`. Open `~/.cabal/config` and uncomment
  the `symlink-bindir` line to be something like:

      symlink-bindir = /home/mitchell/.local/bin

* Build and install the Haskell server, which bundles the `index.html` to make
  it available by the server at runtime.

      // One or the other
      cabal v2-install . --overwrite-policy=always
      stack install

That's it! Now you have a `unison-browser` executable that you can run in any
directory with a `.unison` folder.

### Note: Development Workflow

For a slightly optimized workflow, you can have live reloading with [elm-live](https://github.com/wking-io/elm-live)

- Start the haskell server in dev mode

      DEV=true stack exec unison-browser

- Use elm-live

      elm-live client-src/Main.elm --debug

## Code guide

Module guide:

* `GitHub`

  GitHub API.

* `Misc`

  Random junk! Find a home for me pls!

* `Ucb.Main.*`

  Main stuff, model-view-update loop.

* `Ucb.Unison.*`

  Unison-specific app stuff (needs better organization).

* `Ucb.Util.*`

  Miscellaneous stuff missing from the core Elm ecosystem.

* `Unison.*`

  Core Unison language kit, suitable as a standalone library. Contains types and
  pure functions that roughly match the Haskell source.
