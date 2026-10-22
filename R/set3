library(forecast)

data("AirPassengers")
ap <- AirPassengers

train <- window(ap, start=c(1949,1), end=c(1958,12))
test <- window(ap, start=c(1959,1), end=c(1960,12))

hw_fit <- HoltWinters(train, seasonal="multiplicative")
hw_forecast <- forecast(hw_fit, h=24)

sarima_fit <- Arima(
  train,
  order=c(1,1,1),
  seasonal=list(order=c(1,1,0), period=12)
)

print(summary(sarima_fit))

sarima_forecast <- forecast(sarima_fit, h=24)

hw_accuracy <- accuracy(hw_forecast, test)
sarima_accuracy <- accuracy(sarima_forecast, test)

comparison <- data.frame(
  Model=c("Holt-Winters Multiplicative", "SARIMA(1,1,1)(1,1,0)[12]"),
  RMSE=c(hw_accuracy["Test set","RMSE"],
         sarima_accuracy["Test set","RMSE"]),
  MAE=c(hw_accuracy["Test set","MAE"],
        sarima_accuracy["Test set","MAE"])
)

print(comparison)

plot(test, main="Forecast Comparison: Holt-Winters vs SARIMA",
     xlab="Year", ylab="Passengers",
     col="black", lwd=2)

lines(hw_forecast$mean, col="blue", lty=2, lwd=2)
lines(sarima_forecast$mean, col="red", lty=3, lwd=2)

legend("topleft",
       legend=c("Actual", "Holt-Winters", "SARIMA"),
       col=c("black", "blue", "red"),
       lty=c(1,2,3), lwd=2)

cat("Holt-Winters RMSE:", hw_accuracy["Test set","RMSE"], "\n")
cat("Holt-Winters MAE:", hw_accuracy["Test set","MAE"], "\n")
cat("SARIMA RMSE:", sarima_accuracy["Test set","RMSE"], "\n")
cat("SARIMA MAE:", sarima_accuracy["Test set","MAE"], "\n")
