


#### F1. Compute wt with multiple vars #####

compute_mult_wt <- function(auxvars, svydata, vector_calib_total, svywt = "none") {
  
  require(survey)
  require(crayon)
  
  if(svywt == "none"){
    svydata$none <- 1
  }
  
  #scale-up the starting wt
  svydata$swt <- (svydata[[svywt]]*sum(vector_calib_total[[1]])/sum(svydata[[svywt]]))
  
  # ifelse(sum(svydata$swt) - vector.calib.total[[1]] == 0,
  #  cat(green("CHECK 1 [sum start wt = population total]: OK \n")),
  cat(green(paste("CHECK 1: Survey data" , sum(svydata$swt), "pop. total", vector_calib_total[[1]], "\n", sep = " " )))
  
  
  #treat NAs
  for(auxvar in auxvars){
    if(sum(is.na(svydata[[auxvar]])) < 15) {
      svydata[[auxvar]] <- ifelse(is.na(svydata[[auxvar]]), 5, svydata[[auxvar]])  
    } else {
      svydata[[auxvar]] <- ifelse(is.na(svydata[[auxvar]]), 6, svydata[[auxvar]])  
    }
  }
  
  #set-survey design
  for(auxvar in auxvars){
    svydata[[auxvar]] <- as.character(svydata[[auxvar]])
  }
  
  svy_des <- svydesign(id = ~ 0, weights = ~ swt, data = svydata)
  
  form <- as.formula(paste("~", paste(auxvars, sep = " ", collapse = "+"), sep = " "))
  
  #create survey wts
  svy_des_wt <- calibrate(svy_des, form, vector_calib_total, calfun = "logit", bounds = c(0, 999999999))
  
  return_list <- list("svy_des_wt" = svy_des_wt, "svy_des" = svy_des)
  return(return_list)
  
}




#### F2. Mutate valoraciones #####

mutate_valora <- function(x){
  as_factor(case_when(x %in% c("0 Muy Mal", "1", "2", "3") ~ "0-3",
                      x %in% c("4", "5", "6") ~ "4-6",
                      x %in% c("7", "8", "9", "10 Muy bien") ~ "7-10",
                      x %in% c("No conoce", "N.S.", "N.C.") ~ "No conoce"))
}