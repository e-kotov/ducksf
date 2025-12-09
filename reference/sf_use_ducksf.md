# Enable or disable preferring ducksf replacements over sf (no attach)

Since ducksf does **not export** any functions with the same names as
`sf`, there is nothing to prefer/mask at runtime. This helper is kept
only for backward compatibility and will just inform the user.

## Usage

``` r
sf_use_ducksf(use = NULL, allow = NULL, deny = NULL, silent = FALSE)
```

## Arguments

- use:

  Ignored (kept for compatibility).

- allow:

  Ignored.

- deny:

  Ignored.

- silent:

  Logical; if TRUE, suppress messages.

## Value

Invisibly returns character(0).
