# ARIMA errors fail clearly when the regressors cannot be found

    Code
      residuals(fit_arima(), type = "regression")
    Condition
      Error:
      ! Could not find the regressors used to fit this model.
        Please use Arima() or auto.arima() rather than arima().

