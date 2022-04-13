# Example code for RMark 
# Anna Moeller 
# 2/23/2021

library(tidyverse)
library(RMark)

eh <- readRDS("Data/encounter_histories.rds") %>% 
  rename(ch = EH,
         fl = firstlength,
         fl_cs = firstlen_cs)

# See if centering and scaling covariate changes the model at all 
run_catfish <- function(){
  # RMark step 1.
  cat_proc <- process.data(eh, model = "CJS")
  
  # RMark step 2. 
  cat_ddl <- make.design.data(cat_proc)
  
  # Step 4.
  #  Define models for Phi
  Phidot <- list(formula = ~1)
  
  #  Define models for p
  pdot <- list(formula = ~1)
  # pre <- list(formula = ~1 | age)
  plen <- list(formula = ~fl)
  plencs <- list(formula = ~fl_cs)
  
  # Step 5. Run assortment of models
  # cat_phidot_pre <- mark(
  #   cat_proc,
  #   cat_ddl,
  #   model.parameters = list(Phi = Phidot, 
  #                           p = pre),
  #   delete = T
  # )
  cat_phidot_pdot <- mark(
    cat_proc,
    cat_ddl,
    model.parameters = list(Phi = Phidot, 
                            p = pdot),
    delete = T
  )
  cat_phidot_plen <- mark(
    cat_proc,
    cat_ddl,
    model.parameters = list(Phi = Phidot, 
                            p = plen),
    delete = T
  )
  cat_phidot_plencs <- mark(
    cat_proc,
    cat_ddl,
    model.parameters = list(Phi = Phidot, 
                            p = plencs),
    delete = T
  )
  
  return(collect.models() )
}

res <- run_catfish()
res

# Compare the two
res$cat_phidot_plen$results$beta
res$cat_phidot_plen$results$real

res$cat_phidot_plencs$results$beta
res$cat_phidot_plencs$results$real

# get.real(res$cat_phidot_plen, 
#          se = T, 
#          parameter = "p")
# 
# res$cat_phidot_plen$results$beta
# res$cat_phidot_plen$results$real
#
# summary(res$cat_phidot_plen)$reals$p[[1]]$pim


# Realistic results from non-centered and non-scaled
# How to find out what link function it used 
res$cat_phidot_plen$links

# My linear model for p is 
# logit(p) = B0 + B1*cov1
# To get detection probability for each animal, based on its weight: 
plogis(-2.602 + (-0.00112) * range(eh$fl))

# p at several useful weights 
x <- seq(10, 1000, by = 50)
plogis(-2.602 + (-0.00112) * x)

# Try this with my transformed data too. 
res$cat_phidot_plencs$links
res$cat_phidot_plencs$results$beta
B0 <- res$cat_phidot_plencs$results$beta$estimate[2]
B1 <- res$cat_phidot_plencs$results$beta$estimate[3]
head(plogis(B0 + B1*eh$fl_cs))
plogis(-3.123 + (-.226)*range(eh$fl_cs))

# p at several useful weights
xcs <- (x - mean(x))/sd(x)
plogis(-3.123 + (-.226) * xcs)


# Try compute.real
compute.real(res$cat_phidot_plencs, 
             data = data.frame(fl_cs= -3:3))

# Covariate predictions
# Use this to find what index p is in 
PIMS(res$cat_phidot_plencs,"p",simplified=FALSE)
# p is 232:462
pred <- covariate.predictions(
  model = res$cat_phidot_plencs,
  data = data.frame(fl_cs = seq(-3, 3, by = 1)),
  indices = 232 # Can pick any in 232:462 or all of them
)
# Look at the estimates
pred$estimates
# Plot it
plot(
  pred$estimates$covdata, 
  pred$estimates$estimate,
  type="l",
  lwd=2,
  xlab="Length (centered and scaled)",
  ylab="Survival",
  ylim=c(0, 0.2)
)
lines(pred$estimates$covdata, pred$estimates$lcl,lty=2)
lines(pred$estimates$covdata, pred$estimates$ucl,lty=2)

