# Other Language's Compiler Status

## Build Environment

* CPU: intel i7-8700 3.20GHz
* RAM: 32GB
* SSD: NVMe
* WSL

## Go

* [Go - source](https://go.googlesource.com/go)
* [Go - Installing Go from source](https://golang.org/doc/install/source)
    + The Go toolchain is written in Go.
    + Build
        - install go lang
            > follow [Go - Getting Started](https://golang.org/doc/install)
        - clone source
            ``` bash
            git clone https://go.googlesource.com/go goroot
            cd goroot
            git checkout go1.13
            ```
        - build
            ``` bash
            cd src
            ./all.bash
            ```
        - some tests are failed in WSL
            > MAYBE: try linux test sets in WSL but WSL is different from real linux...
        - output
            ``` bash
            goroot/bin/
            ```
        - time
            ``` bash
            real    11m22.156s
            user    11m43.047s
            sys     25m28.188s
            ```

## Swift

* [Swift: Source Code](https://swift.org/source-code/)
    + based on llvm
    + repository: https://github.com/apple/swift

## JavaScript - V8

* [Documentation V8](https://v8.dev/docs)
    + repository: https://chromium.googlesource.com/v8/v8.git
    + mirror: https://github.com/v8/v8
    + c++

## Python 3

* [GitHub: Python](https://github.com/python/cpython)
    + c++

## Haskell

* [GHC: The Glasgow Haskell Compiler](https://www.haskell.org/ghc/)
    + repository: https://gitlab.haskell.org/ghc/ghc
    + written by haskell and yacc, c

## elixir

* [GitHub: elixir](https://github.com/elixir-lang/elixir)
    + written by erlang

## Erlang

* [Building and Installing Erlang/OTP](http://erlang.org/doc/installation_guide/INSTALL.html)
    + written by c++
