# Unison Codebase Explorer

The Unison Codebase Explorer is an Elm application and companion Haskell server
for exploring your Unison codebase. It is very alpha-quality software, but you
can still use it in anger. Pull requests welcome and appreciated :)

## Building

To build, you need `elm` 0.19+ and either `cabal-install` 2.4+ or `stack`.

- Generate the `main.js` JavaScript blob.

      elm make client-src/Main.elm --output main.js

- If you are using `cabal`, make sure you've configured it to install executables somewhere on your `$PATH`. Open `~/.cabal/config` and uncomment the `symlink-bindir` line to be something like:

      symlink-bindir = /home/mitchell/.local/bin

- Build and install the Haskell server, which bundles the `index.html` to make it available by the server at runtime.

      // One or the other
      cabal v2-install . --overwrite-policy=always
      stack install

That's it! Now you have a `unison-browser` executable that you can run in any
directory with a `.unison` folder.

### Note: Development Workflow

#### Client

For a slightly optimized workflow, you can have live reloading with [elm-live](https://github.com/wking-io/elm-live)

- Start the haskell server

      // One or the other
      cabal v2-run
      stack run

- Use elm-live

      elm-live client-src/Main.elm -- --output=main.js
      # or, equivalently
      make watch

The `--debug` for `elm-live`/`elm-make` is possible with tiny Unison codebases, but it becomes untenable by the time you have around 1000 definitions.

#### Server

Use `ghcid` for live reloading.

    // One or the other
    ghcid -c "cabal v2-repl -O0" --restart unison-browser.cabal
    ghcid -c "stack ghci" --restart unison-browser.cabal

Remember that `ucm` saves your `.unison` codebase in your home directory by default, so you'll probably want to start the server in there.

## Code guide

Module guide:

- `GitHub`

  GitHub API.

- `Misc`

  Random junk! Find a home for me pls!

- `Ucb.Main.*`

  Main stuff, model-view-update loop.

- `Ucb.Unison.*`

  Unison-specific app stuff (needs better organization).

- `Ucb.Util.*`

  Miscellaneous stuff missing from the core Elm ecosystem.

- `Unison.*`

  Core Unison language kit, suitable as a standalone library. Contains types and
  pure functions that roughly match the Haskell source.
