library("tidyverse")

# tables OPTN
epts_table <- read_csv2("data/EPTStable2020.csv")
kdpi_table <- read_csv2("data/KDPItable2020.csv")


# function EPTS
epts <- function(age = 40, 
                 diabetes = F, 
                 priortx = F, 
                 tdialysis = 200 # time on dialysis on months
                 ){
  
  if(!is.numeric(age)){stop("age is not a numerical value!")}
  if(!is.numeric(tdialysis)){stop("time on dialysis is not a numerical value!")}
  if(!is.logical(diabetes)){stop("diabetes is not a logical value!")}
  if(!is.logical(priortx)){stop("prior transplant is not a logical value!")}
  
  tdialysis <- tdialysis/12
  
  l1 <- 0.047*max(age-25,0)-0.015*diabetes*max(age-25,0)
  l2 <- 0.398*priortx-0.237*diabetes*priortx
  l3 <- 0.315*log(tdialysis+1)-0.099*diabetes*log(tdialysis+1)
  l4 <- 0.13*ifelse(tdialysis == 0, 1, 0)-0.348*diabetes*ifelse(tdialysis == 0, 1, 0)+1.262*diabetes

  raw_epts <- l1+l2+l3+l4  
  
  epts_table %>% filter(lower <= raw_epts,
                        upper > raw_epts) %>% .$score -> score_epts
  score_epts <- score_epts * 100
  
  list(raw_EPTS = raw_epts,
       score_EPTS = score_epts)
  
}

# function KDPI kdpi(age = , height = , weight= , mmB = 1, mmDR = 1)
kdpi <- function(age = 40, raceAA = F, # African American T/F
                 hypertension = F, # History of Hypertension T/F
                 diabetes = F, # diabetic T/F
                 creatinine = 0.9, # mg/dL
                 stroke = F, # cause of death stroke T/F
                 height = 180, weight = 80, dcd = F, # Donation after cardiac death T/F
                 hcv = F, 
                 mmB = 0, # number of mesmatches HLA-B
                 mmDR = 0, # number of mesmatches HLA-DR
                 cold = 15, # cold esquemia time (hr)
                 enbloc = F, # enbloc kidney transplant T/F
                 double = F # double kidney transplant T/F
                 ){
  
  l1 <- -0.0194*ifelse(age<18, 1, 0)*(age-18)+0.0128*(age-40)+0.0107*ifelse(age>50, 1, 0)*(age-50)
  l2 <- 0.179*ifelse(raceAA,1,0)+0.123*ifelse(hypertension, 1, 0)+0.13*ifelse(diabetes, 1, 0)
  l3 <- 0.22*(creatinine-1)-0.209*ifelse(creatinine >1.5, 1, 0)*(creatinine-1.5)
  l4 <- 0.0881*ifelse(stroke,1,0)-0.0464*((height-170)/10)-0.0199*ifelse(weight<80, 1, 0)*((weight-80)/5)
  l5 <- 0.133*ifelse(dcd, 1, 0)+0.24*ifelse(hcv, 1, 0)
  l6 <- -0.0766*ifelse(mmB==0, 1, 0)-0.061*ifelse(mmB==1, 1, 0)-0.13*ifelse(mmDR==0, 1, 0)+0.0765*ifelse(mmDR==2, 1, 0)
  l7 <- 0.00548*(cold-20)-0.364*ifelse(enbloc,1,0)-0.148*ifelse(double, 1, 0)
  
  KDRIr <- exp(l1+l2+l3+l4+l5+l6+l7)
  KDRIm <- KDRIr/1.28991045343964
  
  kdpi_table %>% filter(lower <= KDRIm,
                        upper > KDRIm) %>% .$score -> score_kdpi
  score_kdpi <- score_kdpi * 100
  
  list(KDRIr = KDRIr,
       KDRIm = KDRIm,
       score_KDPI = score_kdpi)
  
}


# function txscore(ageR = , race = , insurance= , causeESRD = 1, timeD = 1, diabetesR = F, coronary = F, albumin = , hemoglobin =, ageD = , diabetesD= F, ECD = F, mmHLA = )
txscore <- function(ageR = "18-34"
                    , race = "White"
                    #, insurance = 0
                    , causeESRD = "Other"
                    , timeD = "<1 yr"
                    , diabetesR = F
                    , coronary = F
                    , albumin = 1.5
                    , hemoglobin = 10
                    , ageD = 30
                    , diabetesD= "Absence"
                    , ECD = F
                    , mmHLA = "0"
){
  
  
  ageR <- ifelse(ageR == "18-34", 0.0993, 
                 ifelse(ageR == "35-49", -0.0784,
                        ifelse(ageR == "50-64", 0, 0.1881)))
  race <- ifelse(race == "White", 0, 
                 ifelse(race == "Black", 0.1609,
                        ifelse(race == "Hispanic", -0.2554, -0.4475)))
  causeESRD <- ifelse(causeESRD == "Diabetes", 0, 
                      ifelse(causeESRD == "Hypertension", 0.1541,
                             ifelse(causeESRD == "Glomerulonephritis", 0.1447,
                                    ifelse(causeESRD == "Cystic Disease", -0.1870, 0.3209))))
  timeD <- ifelse(timeD == "<1 yr", 0, 
                  ifelse(timeD == ">=1yr, <3 yr", -0.2618,
                         ifelse(timeD == ">=3yr, <=5yr", -0.3747, -0.1432)))
  diabetesR <- ifelse(diabetesR == T, 0.3021, 0)
  coronary <- ifelse(coronary == T, 0.2617, 0)
  albumin <- (albumin - 4)*(-0.2644)
  hemoglobin <- (hemoglobin - 12.3)*(-0.0451)
  ageD <- (ageD - 39)*0.0059
  diabetesD <- ifelse(diabetesD == "Absence", 0,  
                      ifelse(diabetesD == "Presence", 0.4596, -0.3308))
  ECD <- ifelse(ECD == T, 0.2082, 0)
  mmHLA <- ifelse(mmHLA == "0" , 0,
                  ifelse(mmHLA == "1-3", 0.3241, 0.3115))
  
  LP <- ageR + race + causeESRD + timeD + diabetesR + coronary + albumin + hemoglobin + ageD + diabetesD + ECD + mmHLA
  
  gamma <- 0.916
  
  PS = gamma * LP
  
  prob5y <- round((1-0.752292^exp(PS))*100,2)
  
  list(LP = LP
       , gamma = gamma
       , PS = PS
       , prob5y = prob5y)
  
}

#txscore()$prob5y

