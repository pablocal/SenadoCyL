


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
  as_factor(case_when(x %in% c("0 Muy Mal", "1 Muy mal", "1", "2", "3") ~ "0-3",
                      x %in% c("4", "5", "6") ~ "4-6",
                      x %in% c("7", "8", "9", "10 Muy bien") ~ "7-10",
                      x %in% c("No conoce", "N.S.", "N.C.") ~ "No conoce"))
}

#### F3. Esc d'hont ####

seats_dhont <- function(data, party, votes, seats, barrier = 0){
  
require(tidyverse)

df <- data[,c(party, votes)]
colnames(df) <- c("Party", "Votes") 
  
### apply barrier ###
if(barrier > 0){
  df_dhont <- df %>% 
    mutate(prc = Votes/sum(Votes)*100,
           Barrier = ifelse(prc > !! barrier, "Y", "N")) %>% 
    filter(Barrier ==  "Y")
} else {
  df_dhont <- df
}

### prepare matrix for dhont###
dhont <- matrix(nrow = nrow(df_dhont), ncol = seats)
dhont[,1] <- as.matrix(df_dhont$Votes)

for(i in 2:seats) {
  dhont[ , i] <- dhont[ , 1]/i
}

### set cut point for seats ###
limit <- sort(as.vector(dhont))[length(as.vector(dhont))-seats]

### compute seats ### 
esc <- dhont > limit
esc <- 1*esc
esc <- rowSums(esc)

### return df###
esc_df <- data.frame(Party = df_dhont$Party, Seats = esc)
return_df <- data.frame(Party = df$Party, Votes = df$Votes)

if(barrier > 0){
  return_df <- df %>% 
    mutate(Perc = round(Votes/sum(Votes)*100,2),
           Barrier = ifelse(Perc > !! barrier, "Y", "N")) %>%
    left_join(esc_df, by = "Party") %>% 
    mutate(Seats = ifelse(is.na(Seats), 0, Seats))
} else {
  return_df <- df %>% 
    mutate(Perc = round(Votes/sum(Votes)*100,2)) %>%
    left_join(esc_df, by = "Party")
}

return(return_df)

}


#### F4. wt pol ####

  
  pop_totals <- function(popdata, prov_code){  
    
    pop_total <- as.numeric(popdata[which(popdata$cprov == prov_code), c(4:12, 14:18, 21, 20, 22)])
    pop_total <- c(sum(popdata[which(popdata$cprov == prov_code), 3:12]), pop_total)
    names <- c("(Intercept)", colnames(popdata[which(popdata$cprov == prov_code),c(4:12, 14:18, 21, 20, 22)]))
    names(pop_total) <- names
    
    pop_total_socdem <- as.numeric(popdata[which(popdata$cprov == prov_code), c(4:12, 21, 20, 22)])
    pop_total_socdem <- c(sum(popdata[which(popdata$cprov == prov_code), 3:12]), pop_total_socdem)
    names <- c("(Intercept)", colnames(popdata[which(popdata$cprov == prov_code), c(4:12, 21, 20, 22)]))
    names(pop_total_socdem) <- names
    
    return_list <- list(pop_total = pop_total, pop_total_socdem = pop_total_socdem)
    
    return(return_list)
    
  }
  
  
  pol_wts <- function(svydata, pop_totals, prov){
    
    pop_total <- pop_totals$pop_total
    pop_total_socdem <- pop_totals$pop_total_socdem
    
    wt <- compute_mult_wt(auxvars = c("sexo_edad", "recuerdo", "tamuni"), svydata = svydata, vector_calib_total = pop_total, svywt = "peso_estu")
    
    wt_socdem <- compute_mult_wt(auxvars = c("sexo_edad", "tamuni"), svydata = svydata, vector_calib_total = pop_total_socdem, svywt = "peso_estu")
    
    peso <- weights(wt$svy_des_wt)
    peso_socdem <- weights(wt_socdem$svy_des_wt)
    
    svydata_names <- colnames(svydata)
    svydata <- cbind(svydata, peso)
    svydata <- cbind(svydata, peso_socdem)
    
    svydata_names <- c(svydata_names, paste0("peso_", prov), paste0("peso_socdem_", prov))
    colnames(svydata) <- svydata_names
    
    return(svydata)
    
  }
  

  
  wt_estim <- function(popdata, prov_code, svydata, prov){
    pop_totales <- pop_totals(popdata, prov_code)
    return_df <- pol_wts(svydata, pop_totales, prov)
    return(return_df)
  }

  
    