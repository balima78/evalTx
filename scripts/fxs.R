library("tidyverse")

# tables OPTN
epts_table <- read_csv2("data/EPTStable2020.csv")
kdpi_table <- read_csv2("data/KDPItable2020.csv")


# function EPTS
epts <- function(age, 
                 diabetes = F, 
                 priortx = F, 
                 tdialysis # time on dialysis on months
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
kdpi <- function(age, raceAA = F, # African American T/F
                 hypertension = F, # History of Hypertension T/F
                 diabetes = F, # diabetic T/F
                 creatinine = 1, # mg/dL
                 stroke = F, # cause of death stroke T/F
                 height, weight, dcd = F, # Donation after cardiac death T/F
                 hcv = F, 
                 mmB, # number of mesmatches HLA-B
                 mmDR, # number of mesmatches HLA-DR
                 cold = 18, # cold esquemia time (hr)
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
