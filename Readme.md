# Hackaroons

Macaroons for Haskell!

This is incomplete. Currently there is no way to add third-party caveats or verify macaroons.

# Usage

## Basic Macaroon

```haskell
> let m = baseMacaroon "key" "id" "location"
> putStrLn $ inspect m

identifier id
location loc
signature be9ae46bcfba7353d9dd29afd9686c11b8539a71cffa25b2a1af5a1e9180171b
```

## First Party Caveat

```haskell
> let m = baseMacaroon "key" "id" "loc"
> let mf = addFirstPartyCaveat m "predicate"
> putStrLn $ inspect mf

identifier id
location loc
cid predicate
signature 007692a3aa3e4bf1967a2650d19885970a3d5137abb043ca628ea6e0b47d8ce4
```

## Serializing

```haskell
> let m = baseMacaroon "key" "id" "loc"
> let mf = addFirstPartyCaveat m "predicate"
> serialize mf

"MDAwZGlkZW50aWZpZXIgaWQKMDAwY2xvY2F0aW9uIGxvYwowMDBkY2lkIHByZWRpY2F0ZQowMDJhc2lnbmF0dXJlIAB2kqOqPkvxlnomUNGYhZcKPVE3q7BDymKOpuC0fYzkCg"
```

# Deserializing

```haskell
> let m = deserialize "MDAwZGlkZW50aWZpZXIgaWQKMDAwY2xvY2F0aW9uIGxvYwowMDBkY2lkIHByZWRpY2F0ZQowMDJhc2lnbmF0dXJlIAB2kqOqPkvxlnomUNGYhZcKPVE3q7BDymKOpuC0fYzkCg"
> putStrLn $ inspect m

identifier id
location loc
signature be9ae46bcfba7353d9dd29afd9686c11b8539a71cffa25b2a1af5a1e9180171b
```
