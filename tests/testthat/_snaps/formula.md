# is trained?

    Code
      rec5 <- prep(rec4, training = iris)
    Condition
      Warning in `prep()`:
      ! The previous data will be used by `prep()`.
      i The data passed using `training` will be ignored.

# bad args

    Code
      formula(rec10)
    Condition
      Error in `formula()`:
      ! The recipe must be prepped before the formula can be computed.

