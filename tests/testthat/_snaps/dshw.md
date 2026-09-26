# dshw() errors on invalid forecast horizon

    Code
      dshw(taylor, h = 0)
    Condition
      Error in `dshw()`:
      ! Forecast horizon out of bounds
    Code
      dshw(taylor, h = 0.5)
    Condition
      Error in `dshw()`:
      ! Forecast horizon out of bounds

