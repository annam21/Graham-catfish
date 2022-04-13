# RMark code
# Anna Moeller 
# 1/27/2021

library(RMark)

data(dipper)

run.dipper <- function(){
  
  # RMark step 1.
  # dipper.processed <- process.data(dipper, model = "CJS") 
  dipper.processed <- process.data(dipper, groups = "sex") # Groups is optional
  
  # RMark step 2. 
  # Puts together study design metadata for RMark
  dipper.ddl <- make.design.data(dipper.processed)
  
  # Optional step 3. Add covariates to Phi and p 
  # This is just simulated data!
  dipper.ddl$p$Temp <- rnorm(nrow(dipper.ddl$p))
  
  # RMark step 4. 
  #  Define models for Phi
  Phidot <- list(formula = ~1)
  Phitime <- list(formula = ~time)
  
  #  Define models for p
  pdot <- list(formula = ~1)
  ptemp <- list(formula = ~Temp)
  ptempTime <- list(formula = ~Temp + Time)
  
  # Step 5. Run assortment of models
  dipper.phidot.pdot <- mark(
    dipper.processed,
    dipper.ddl,
    model.parameters = list(Phi = Phidot, 
                            p = pdot),
    # delete = T
    )
  dipper.phitime.ptemp <- mark(
    dipper.processed,
    dipper.ddl,
    model.parameters = list(Phi = Phitime, 
                            p = ptemp),
    # delete = T
    )
  dipper.phidot.ptempTime <- mark(
    dipper.processed, 
    dipper.ddl,
    model.parameters = list(Phi = Phidot,
                            p = ptempTime),
    # delete = T
    )
 
  # RMark step 6. 
  # Return model table and list of models
  return(collect.models() )
}

# RMark step 7. 
# Run the models and look at the results
dipper.results <- run.dipper()
dipper.results
# Sorts automatically by AICc

# RMark step 8. inspect a single model from tmp file
dipper.results$dipper.phidot.ptempTime
get.real(dipper.results$dipper.phidot.ptempTime, 
         se = T, 
         parameter = "p")

dipper.results$dipper.phidot.pdot$results$beta
dipper.results$dipper.phidot.pdot$results$real

summary(dipper.results$dipper.phidot.pdot)$reals$Phi[[1]]$pim

# GOF
# Run dipper.processed outside of function 
release.gof(dipper.processed)

