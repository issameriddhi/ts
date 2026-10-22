library(forecast)

data("AirPassengers")
x <- log(AirPassengers)

model <- Arima(x, order=c(2,1,2), method="ML")
print(summary(model))

coef_values <- model$coef
se_values <- sqrt(diag(model$var.coef))

parameters <- c("ar1", "ar2", "ma1", "ma2")
estimates <- coef_values[parameters]
standard_errors <- se_values[parameters]

t_ratios <- estimates / standard_errors
p_values <- 2 * (1 - pnorm(abs(t_ratios)))

significance <- data.frame(
  Parameter=c("phi1", "phi2", "theta1", "theta2"),
  Estimate=as.numeric(estimates),
  Standard_Error=as.numeric(standard_errors),
  T_Ratio=as.numeric(t_ratios),
  P_Value=as.numeric(p_values),
  Significant=as.numeric(p_values) < 0.05
)

print(significance)

cat("Non-significant parameters:\n")
print(significance$Parameter[significance$P_Value >= 0.05])

models <- list(
  "ARIMA(2,1,2)"=Arima(x, order=c(2,1,2), method="ML"),
  "ARIMA(1,1,2)"=Arima(x, order=c(1,1,2), method="ML"),
  "ARIMA(2,1,1)"=Arima(x, order=c(2,1,1), method="ML"),
  "ARIMA(1,1,1)"=Arima(x, order=c(1,1,1), method="ML"),
  "ARIMA(0,1,2)"=Arima(x, order=c(0,1,2), method="ML"),
  "ARIMA(2,1,0)"=Arima(x, order=c(2,1,0), method="ML"),
  "ARIMA(1,1,0)"=Arima(x, order=c(1,1,0), method="ML"),
  "ARIMA(0,1,1)"=Arima(x, order=c(0,1,1), method="ML")
)

model_comparison <- data.frame(
  Model=names(models),
  AIC=sapply(models, AIC),
  BIC=sapply(models, BIC)
)

model_comparison <- model_comparison[order(model_comparison$AIC), ]
print(model_comparison)

best_name <- model_comparison$Model[1]
cat("Selected parsimonious candidate:", best_name, "\n")
print(summary(models[[best_name]]))
