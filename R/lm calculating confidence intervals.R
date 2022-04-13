mean.cis <- function(x, y, pred.x) {
  n <- length(y) # Find sample size
  lm.model <- lm(y ~ x) # Fit linear model
  y.fitted <- lm.model$fitted.values # Extract the fitted values of y
  
  # Coefficients of the linear model, beta0 and beta1
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]
  
  pred.y <- b1 * pred.x + b0 # Predict y at the given value of x (argument pred.x)
  
  # Find SSE and MSE
  sse <- sum((y - y.fitted)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(0.975, n - 2) # Critical value of t
  
  mean.se.fit <- (1 / n + (pred.x - mean(x))^2 / (sum((x - mean(x))^2))) # Standard error of the mean estimate
  pred.se.fit <- (1 + (1 / n) + (pred.x - mean(x))^2 / (sum((x - mean(x))^2))) # Standard error of the prediction
  
  # Mean Estimate Upper and Lower Confidence limits at 95% Confidence
  mean.conf.upper <- pred.y + t.val * sqrt(mse * mean.se.fit)
  mean.conf.lower <- pred.y - t.val * sqrt(mse * mean.se.fit)
  
  # Build data.frame of upper and lower limits calculated above, as well as the predicted y and beta 1 values
  upper <- data.frame(rbind(round(mean.conf.upper, 2)))
  lower <- data.frame(rbind(round(mean.conf.lower, 2)))
  fit <- data.frame(rbind(round(pred.y, 2)))
  
  # Collect all into data.frame and rename columns
  results <- data.frame(cbind(lower, upper, fit))
  colnames(results) <- c('Lower', 'Upper', 'Fit')
  
  return(results)
}