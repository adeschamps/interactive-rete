[![Build Status](https://travis-ci.com/adeschamps/interactive-rete.svg?branch=master)](https://travis-ci.com/adeschamps/interactive-rete)

# Interactive Rete

This is an interactive demo of the Rete pattern matching algorithm. It is a work in progress.

It was bootstrapped using [Create Elm App](https://github.com/halfzebra/create-elm-app). See the [this README](./README-create-elm-app.md) for details.

The interface is written in Elm, but the Rete itself is a [separate Rust library](https://github.com/adeschamps/rete), included here as a Git submodule. It needs to be built with [wasm-pack](https://github.com/rustwasm/wasm-pack) before running `npm install`:

```bash
$ git submodule update --init
$ cd rete
$ wasm-pack build --target web -- --features trace
```
