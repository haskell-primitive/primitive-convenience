# primitive-convenience

[![Hackage](https://img.shields.io/hackage/v/primitive-convenience.svg)](https://hackage.haskell.org/package/primitive-convenience)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Build status](https://secure.travis-ci.org/haskell-primitive/primitive-convenience.svg)](https://travis-ci.org/haskell-primitive/primitive-convenience)

convenience classes for PrimMonad/PrimBase. The 'PrimMonad' state token
type can be annoying to handle in constraints. The typeclasses provided
in this library let users (visually) notice 'PrimState' equality
constraints less, by witnessing that `s ~ 'PrimState' m`.
