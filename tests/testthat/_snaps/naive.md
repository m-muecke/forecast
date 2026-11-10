# rw_model requires more observations than lag

    Code
      rw_model(ts(1:4, frequency = 4), lag = 4)
    Condition
      Error in `rw_model()`:
      ! `y` must have more observations than `lag`.

---

    Code
      snaive(ts(1:4, frequency = 4))
    Condition
      Error in `rw_model()`:
      ! `y` must have more observations than `lag`.

