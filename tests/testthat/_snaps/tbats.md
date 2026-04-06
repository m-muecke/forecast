# tbats() num.cores is deprecated

    Code
      invisible(tbats(rep(1, 100), num.cores = 1, use.parallel = FALSE))
    Condition
      Warning in `tbats()`:
      num.cores is deprecated. Use mirai::daemons() to set up parallel workers.

