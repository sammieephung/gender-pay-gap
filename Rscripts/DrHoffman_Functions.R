# NOTE: This version adds an SE column to the LM and glht effect size tables

######################################################################################################
# Check to see if needed packages are downloaded, install if not; then load

# To get sums of squares table in GLMs
if (!require("supernova")) install.packages("supernova"); library(supernova)

# To get nested model comparisons in GLMs
if (!require("lmhelprs")) install.packages("lmhelprs"); library(lmhelprs)


######################################################################################################
# LMsummary provides a custom summary of an lm object, including a sums of squares table,
# fixed effects table, and an optional effect sizes table (each with optional explanations)
LMsummary = function(model, level=.95, digits=3, effectsizes=FALSE, explain=FALSE, print=TRUE) {
  
  # Build variables out of digits argument needed below
  digitchar=paste0("%0.", digits, "f")  # Number of digits after decimal
  lowerp=10^(-digits)                   # Set lower p value basd on digits for formatting
  
  # Reformat sums of squares table from supernova output
  ss_start = supernova(model)                    # Run supernova to get all SS
  ss_start = as.data.frame(ss_start[["tbl"]])    # Convert result table to data frame
  ss_model = ss_start[ss_start$term=="Model", ]  # Save "Model" row from table
  ss_error = ss_start[ss_start$term=="Error", ]  # Save "Error" row from table
  ss_total = ss_start[ss_start$term=="Total", ]  # Save "Total" row from table
  ss_end = rbind(ss_model, ss_error, ss_total)       # Combine rows into final SS table
  rownames(ss_end) = ss_end$term                     # Set row names to term names
  ss_end = subset(ss_end, select=-c(1,2))            # Drop unneeded columns
  ss_end = ss_end[, c(1,2,3,4,6,5)]                  # Reorder columns so Rsquare is last
  colnames(ss_end) = c("SS","DF","MS","F","p","R2")  # Rename columns
  ss_end_data = ss_end                                   # Save a numeric copy as object
  # Format sums of squares table as characters for printing to console
  ss_end$SS = sprintf(digitchar, ss_end$SS)  # Format sums of squares values
  ss_end$MS = sprintf(digitchar, ss_end$MS)  # Format mean square values
  ss_end$F  = sprintf(digitchar, ss_end$F)   # Format F test-statistic value
  ss_end$R2 = sprintf(digitchar, ss_end$R2)  # Format R-square value
  # Format p-value or change to < digits
  ss_end$p  = ifelse(ss_end_data$p<lowerp, paste0("<", lowerp), sprintf(digitchar, ss_end_data$p))
  # Convert any NA values in ss_end to ""
  ss_end$SS[ss_end$SS == "NA" | is.na(ss_end$SS)] = ""
  ss_end$MS[ss_end$MS == "NA" | is.na(ss_end$MS)] = ""
  ss_end$DF[ss_end$DF == "NA" | is.na(ss_end$DF)] = ""
  ss_end$F[ss_end$F == "NA"   | is.na(ss_end$p)] = ""
  ss_end$p[ss_end$p == "NA"   | is.na(ss_end$p)] = ""
  ss_end$R2[ss_end$R2 == "NA" | is.na(ss_end$p)] = "" 
  
  # Reformat lm output for fixed effects solution  
  modelsummary = summary(model)         # Save model summary
  fix_CIs = confint(model, level=level)     # Save fixed effects CIs
  fix = data.frame(modelsummary$coefficients, fix_CIs)       # Combine into one table
  colnames(fix) = c("Est","SE","t","p","LCI","UCI")          # Rename columns
  rownames(fix)[rownames(fix)=="(Intercept)"] = "Intercept"  # Rename row
  fix_data = fix  # Save a numeric copy as object
  # Format fixed effects table p-value for printing to console
  fix$Est  = sprintf(digitchar, fix$Est)   # Format estimate values
  fix$SE   = sprintf(digitchar, fix$SE)    # Format standard error values
  fix$t    = sprintf(digitchar, fix$t)     # Format t test-statistic values
  fix$LCI  = sprintf(digitchar, fix$LCI)   # Format lower CI values
  fix$UCI  = sprintf(digitchar, fix$UCI)   # Format upper CI values
  # Format p value or change to < digits
  fix$p = sprintf(digitchar, fix$p)
  fix$p = ifelse(fix_data$p<lowerp, paste0("<", lowerp), sprintf(digitchar, fix_data$p))
  
  # Compute and print effect sizes if requested
  # Cohen's d = (2*t)/sqrt(df), Partial r = t/sqrt(t^2 + df), Semi-Partial r = t*sqrt((1-R2)/df)
  modelsummary = summary(model)         # Save model summary
  cd = (2*modelsummary$coefficients[,"t value"])/sqrt(model$df.residual)       # Cohen's d
  pr =    modelsummary$coefficients[,"t value"]/
     sqrt(modelsummary$coefficients[,"t value"]^2+model$df.residual); pR2=pr^2 # Partial R and Rsquare
  sr =    modelsummary$coefficients[,"t value"]*
     sqrt((1-summary(model)$r.squared)/model$df.residual); sR2 = sr^2          # Semi-Partial R and Rsquare
  fix_size = data.frame(modelsummary$coefficients, cd, pr, sR2)    # Merge to one table
  fix_size = subset(fix_size, select=-c(3))                        # Drop unneeded t column
  fix_size = fix_size[rownames(fix_size)!="(Intercept)", ]   # Remove row with intercept 
  colnames(fix_size) = c("Est","SE","p","d","pr","sR2")   # Rename columns
  fix_size_data = fix_size  # Save a numeric copy as object
  # Format effect fix_sizes table for printing to console
  fix_size$Est = sprintf(digitchar, fix_size$Est)   # Format estimate values
  fix_size$SE  = sprintf(digitchar, fix_size$SE)    # Format SE values
  fix_size$d   = sprintf(digitchar, fix_size$d)     # Format Cohen's d values
  fix_size$pr  = sprintf(digitchar, fix_size$pr)    # Format partial r values
  fix_size$sR2 = sprintf(digitchar, fix_size$sR2)   # Format semi-partial R-square values
  # Format p-value or change to < digits
  fix_size$p = sprintf(digitchar, fix_size$p) # Format p value or change to < digits
  fix_size$p = ifelse(fix_size_data$p<lowerp, paste0("<", lowerp), sprintf(digitchar, fix_size_data$p))

  # Print results if requested (otherwise just saves data as objects to environment)
  if (print==TRUE) {
      cat("\n"); cat("Sums of Squares Table\n")
      print(ss_end); cat("\n") # Print results and insert blank line
      if (explain==TRUE){
          cat("Explanation:\n")
          cat("SS = Sum of Squares, MS = Mean Square, DF = Degrees of Freedom,\n")
          cat("F = F test-statistic, p = two-sided p-value, R2 = R-square\n"); cat("\n")
          } # Close explain
      cat("Fixed Effects Table\n")
      print(fix); cat("\n") # Print results and insert blank line
      if (explain==TRUE){
          cat("Explanation:\n")
          cat("Est = Estimate, SE = Standard Error, t = t test-statistic, p = p-value,\n")
          cat("LCI and UCI = Lower and Upper Confidence Interval\n"); cat("\n") 
          } # Close explain
      if (effectsizes==TRUE) {
          cat("Effect Sizes for Fixed Effects Table\n")
          print(fix_size); cat("\n") # Print results and insert blank line
             if (explain==TRUE){
                 cat("Explanation:\n")
                 cat("Est = Estimate, p = two-sided p-value, d = Cohen's d,\n")
                 cat("pr = Partial r, sR2 = Semi-Partial R-square\n"); cat("\n")
             } # Close explain
      }  # Close effectsizes
  } # Close print if   
  # Save numeric results as objects
  return(list(ss1=ss_end_data, fix1=fix_data, fix_size1=fix_size_data))
  
} # Close function


######################################################################################################
# glhtSummary provides a custom summary of a glht object, including a linear combinations of
# fixed effects table and an optional effect sizes table (each with optional explanations)
glhtSummary = function(glhtObject, level=.95, digits=3, effectsizes=FALSE, explain=FALSE, print=TRUE){
  
  model = glhtObject$model  # Extract model from glht object
  # Build variables out of digits argument needed below
  digitchar=paste0("%0.", digits, "f")  # Number of digits after decimal
  lowerp=10^(-digits)                   # Set lower p value basd on digits for formatting
  
  glhtSummary = summary(glhtObject, test=adjusted("none"))  # Save summary with unadjusted p-values
  glht_CIs = confint(glhtObject, level=level, calpha=univariate_calpha())  # Get CIs
  # Merge to one table
  combo = data.frame(Estimate=glhtSummary$test$coefficients, SE=glhtSummary$test$sigma,
                     t=glhtSummary$test$tstat, p=glhtSummary$test$pvalues, glht_CIs$confint) 
  combo = subset(combo, select=-c(Estimate.1))  # Drop repeat estimate column
  colnames(combo) = c("Est","SE","t","p","LCI","UCI")
  combo_data = combo  # Save a numeric copy as object
  # Format linear combinations table for printing to console
  combo$Est  = sprintf(digitchar, combo$Est)   # Format estimate values
  combo$SE   = sprintf(digitchar, combo$SE)    # Format standard error values
  combo$t    = sprintf(digitchar, combo$t)     # Format t test-statistic values
  combo$LCI  = sprintf(digitchar, combo$LCI)   # Format lower CI values
  combo$UCI  = sprintf(digitchar, combo$UCI)   # Format upper CI values
  combo$p = sprintf(digitchar, combo$p) # Format p value or change to < digits
  combo$p = ifelse(combo_data$p<lowerp, paste0("<", lowerp), sprintf(digitchar, combo_data$p))
  
  # Compute and print effect sizes if requested
  # Cohen's d = (2*t)/sqrt(df), Partial r = t/sqrt(t^2 + df), Semi-Partial r = t*sqrt((1-R2)/df)
  glhtSummary = summary(glhtObject, test=adjusted("none"))       # Save summary with unadjusted p-values
  cd = (2*glhtSummary$test$tstat)/sqrt(model$df.residual)  # Cohen's d
  pr = glhtSummary$test$tstat/sqrt(glhtSummary$test$tstat^2+model$df.residual); pR2=pr^2      # Partial r and R2
  sr = glhtSummary$test$tstat*sqrt((1-summary(model)$r.squared)/model$df.residual); sR2=sr^2  # Semi-partial r and R2
  combo_size = data.frame(Est=glhtSummary$test$coefficients, Est=glhtSummary$test$sigma,  # Merge to one table
                          p=glhtSummary$test$pvalues, cd, pr, sR2)
  colnames(combo_size) = c("Est","SE","p","d","pr","sR2")  # Rename columns
  combo_size_data = combo_size  # Save a numeric copy as object
  # Format linear combinations effect size table for printing to console
  combo_size$Est = sprintf(digitchar, combo_size$Est)   # Format estimate values
  combo_size$SE  = sprintf(digitchar, combo_size$SE)    # Format SE values
  combo_size$d   = sprintf(digitchar, combo_size$d)     # Format Cohen's d values
  combo_size$pr  = sprintf(digitchar, combo_size$pr)    # Format partial r values
  combo_size$sR2 = sprintf(digitchar, combo_size$sR2)   # Format semi-partial R-square values
  # Format p-value or change to < digits
  combo_size$p = sprintf(digitchar, combo_size$p) # Format p value or change to < digits
  combo_size$p = ifelse(combo_size_data$p<lowerp, paste0("<", lowerp), sprintf(digitchar, combo_size_data$p))  
  
  # Print results if requested (otherwise just saves data as objects to environment)
  if (print==TRUE) {
    cat("\n"); cat("Linear Combinations Table\n")
      print(combo); cat("\n") # Print results and insert blank line
      if (explain==TRUE){
        cat("Explanation:\n")
        cat("Est = Estimate, SE = Standard Error, t = t test-statistic, p = p-value,\n")
        cat("LCI and UCI = Lower and Upper Confidence Interval\n"); cat("\n") 
      } # Close explain
      if (effectsizes==TRUE) {
          cat("Effect Sizes for Linear Combinations Table\n")
          print(combo_size); cat("\n") # Print results and insert blank line
      if (explain==TRUE){
          cat("Explanation:\n")
          cat("Est = Estimate, p = p-value, d = Cohen's d,\n")
          cat("pr = Partial r, sR2 = Semi-Partial R-square\n"); cat("\n")
          } # Close explain
       }  # Close effectsizes
  } # Close print if   
  # Save numeric results as objects
  return(list(combo1=combo_data, combo_size1=combo_size_data))
  
} # Close function


######################################################################################################
# R2compare prints the F-test and change in R2 for a model comparison
#   (as provided by the hierarchical_lm function in the lmhelprs package),
#   as well as partial R2 and semi-partial R2 (which is equal to R2 change here)
R2compare = function(ReducedModel, FullModel, PredName, digits=3, explain=FALSE, print=TRUE){

  # Build variables out of digits argument needed below
  digitchar=paste0("%0.", digits, "f")  # Number of digits after decimal
  lowerp=10^(-digits)                   # Set lower p value basd on digits for formatting
  
  FullModelTable = supernova(FullModel) # Get total sums of squares for computations
  SStotal = FullModelTable[["tbl"]][["SS"]][length(FullModelTable[["tbl"]][["SS"]])]  
  R2F = hierarchical_lm(ReducedModel, FullModel) # Get values for F-test and R2 change
  SSeffect   = R2F[2,7]  # Save effect sums of squares (as reduction in SS from full model)
  SSresidual = R2F[2,5]  # Save residual sums of squares from full model
  R2reduced  = R2F[1,2]  # Save reduced model R2
  R2full     = R2F[2,2]  # Save full model R2
  pR2 = SSeffect/(SSeffect+SSresidual)  # Compute Partial R2
  sR2 = SSeffect/SStotal                # Compute Semi-Partial R2
  colnames(R2F) = c("AdjR2","R2total","R2diff","DFden","RSS","DFnum","SSeffect","F","p")  # Better names
  R2F = R2F[2, ]  # Remove unnecessary first row
  comp = data.frame(R2reduced=R2reduced, R2full=R2full, R2diff=R2F$R2diff, DFnum=R2F$DFnum, DFden=R2F$DFden, 
                    F=R2F$F, p=R2F$p, pR2=pR2, sR2=sR2)  # Re-order, add effect sizes computed above
  row.names(comp) = as.character(row.names(comp))  # Rename rows as characters
  rownames(comp)[rownames(comp)=="1"] = ""   # Remove row label
  comp_data = comp  # Save a numeric copy as object
  # Format model comparison table for printing to console
  comp$R2reduced = sprintf(digitchar, comp$R2reduced)  # Format R-square reduced value
  comp$R2full    = sprintf(digitchar, comp$R2full)     # Format R-square full value
  comp$R2diff    = sprintf(digitchar, comp$R2diff)     # Format R-square difference value
  comp$F         = sprintf(digitchar, comp$F)          # Format F test-statistic values
  comp$pR2       = sprintf(digitchar, comp$pR2)        # Format partial R-square values
  comp$sR2       = sprintf(digitchar, comp$sR2)        # Format semi-partial R-square values
  # Format p-value or change to < digits
  comp$p = sprintf(digitchar, comp$p) # Format p value or change to < digits
  comp$p = ifelse(comp_data$p<lowerp, paste0("<", lowerp), sprintf(digitchar, comp_data$p))  
  
  # Print results if requested (otherwise just saves data as objects to environment)
  if (print==TRUE) {
    cat("\n"); cat(paste("F-Test and R2 Change for", PredName, "Slopes \n")) # Add better title
    print(comp); cat("\n") # Print results and insert blank line
    if (explain==TRUE){
      cat("Explanation:\n")
      cat("R2reduced and R2full = Reduced and Full Model R-squares, R2diff = Change in Model R-square,\n")
      cat("DFnum and DFden = Numerator and Denominator Degrees of Freedom for Change in R-square,\n")
      cat("F = F test-statistic for change in R-square, p = two-sided p-value\n"); cat("\n") 
    } # Close explain
  } # Close print if   
  # Save numeric results as objects
  return(comp1=comp_data) # Only print second row (first is no longer needed)
  
} # Close function