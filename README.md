# hash-cons

An opportunistic hash-consing data structure in Haskell.

## Description

`HashCons` provides a pure interface for hash-consing values, allowing faster comparisons and structural sharing. As opposed to hash-consing methods that use a global hash table, this approach is entirely local.  When two `HashCons` values "bump into each other" (e.g. by getting compared for equality), if they are equal, the internals of one is discarded and replaced with the internals of the other.  Since the internals are equivalent, this is safe.

## Features

- **Pure Functional Semantics**: Despite mutation under the hood, the interface is pure.
- **Opportunistic Deduplication**: Deduplicates values upon equality checks.
- **Thread Safety**: Designed to be thread-safe for concurrent use.

## Installation

Add `hash-cons` to your project's dependencies.

## Usage

```haskell
import Data.HashCons

main :: IO ()
main = do
    let x = hashCons "hello"
        y = hashCons "hello"
    print (x == y)       -- True
    -- At this point, the memory associated with one
    -- of the above "hello" values can be freed
    print (unHashCons x) -- "hello"
```

## Important Note on `Ord`

The `Ord` instance is based on hash values, not actual values. This improves performance but may result in incorrect ordering if hash collisions occur. The `Eq` instance uses actual values for comparison.

## Testing

Run the test suite using:

```bash
cabal test
```

## License

This project is licensed under the BSD-3-Clause License.

## Author

Ryan Trinkle - [ryan@trinkle.org](mailto:ryan@trinkle.org)
