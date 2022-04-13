# Covariate predictions from RMark
# Anna Moeller 
# 3/22/2021

# https://www.rdocumentation.org/packages/RMark/versions/2.2.7/topics/covariate.predictions 

# My top model is called res$cat_phidot_plen

# Define values I want to predict to 
# Make sure to name the column the same as it's named in encounter history
pred_vals <- data.frame(
  fl = seq(0, 1000, by = 20)
)

# Use this to find what index p is in 
PIMS(res$cat_phidot_plen,
     "p",
     simplified = FALSE)
# For me, this shows that indices 232:462 all refer to p not Phi

# Get covariate predictions
pred <- covariate.predictions(
  model = res$cat_phidot_plen,
  data = pred_vals,
  indices = 232 # Can pick any in 232:462 or all of them
)

# Look at the estimates
pred$estimates

# Plot it
plot(
  pred$estimates$covdata, 
  pred$estimates$estimate,
  type = "l",
  lwd = 2,
  xlab = "Length",
  ylab = "Detection probability",
  ylim=c(0, 0.2)
)
lines(pred$estimates$covdata, pred$estimates$lcl, lty = 2)
lines(pred$estimates$covdata, pred$estimates$ucl, lty = 2)

