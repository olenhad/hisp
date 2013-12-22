module Utils ((|>)) where

infixr 9 |>
f |> g = g . f
