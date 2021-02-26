# data used in UK algorithm 

# file example for UK candidates
ex.candidates.uk <- read_csv2("files/candidates.csv")
ex.candidates.uk<-ex.candidates.uk %>% 
  mutate_at(vars(A1,A2,B1,B2,DR1,DR2),as.character) %>% select(!(a1:MMP))

# create random column with matchability score 
set.seed(1)
ex.candidates.uk$MS<-sample(1:10, 
                            size = 500, 
                            replace = T, 
                            prob = c(0.12,0.12,0.11,0.11,0.11,0.1,0.1,0.1,0.1,0.03))
# compute column Tier with options A (MS = 10 OR cPRA = 100% OR dialysis >= 84) or B (others)
set.seed(1)
ex.candidates.uk<-ex.candidates.uk %>% 
  mutate(Tier = ifelse(MS == 10 | cPRA == 100 | dialysis >= 84, "A","B"),
         diabetic = sample(0:1,500,replace = T,prob = c(0.9,0.1)), # imput 10% of diabetic patients
         atregist = sample(0:1,500,replace = T,prob = c(0.95,0.05)), # imput 5% of patients preemptive registation
         rri = exp((0.016 * (age -75)) +
                     (0.361 * atregist) +
                     (0.033 * ((dialysis*30 -950) / 365.25)) +
                     (0.252 * diabetic)
         ), # compute value for Recipient Risk Index
         RRI = case_when(rri <= 0.74 ~ 'R1',
                         rri <= 0.94 ~ 'R2',
                         rri <= 1.2 ~ 'R3',
                         TRUE ~ 'R4') # compute Recipient Risk Index from 'rri'
  )

candidates.uk <- ex.candidates.uk %>% 
  mutate(diabetesR = as.logical(diabetic)) %>% 
  select(ID, bg, diabetic, diabetesR) 

# file example for UK donors
ex.donors <- read_csv2("files/donors.csv")
ex.donors<-ex.donors %>% 
  mutate_at(vars(A1,A2,B1,B2,DR1,DR2),as.character)
## columns description
# ID - donors' identification; type integer
# bg - donors' blood group; type character (A, AB, B, O)
# A1, A2, B1, B2, DR1, DR1 - HLA typing; type character (same resolution as defined for candidates and antibodies)
# age - donors' age; type integer

# UK donors example file
library(msm)
set.seed(1)
ex.donors.uk <-ex.donors %>% mutate(sex = sample(0:1, 70, replace = T, prob = c(0.6, 0.4)), # imput donors' sex (0- male; 1 - female)
                                    height = ifelse(sex == 1,
                                                    round(rtnorm(n = 70, mean = 165, sd = 10, lower=145, upper=180)),
                                                    round(rtnorm(n = 70, mean = 175, sd = 10, lower=150, upper=199))), # impute donor's height by sex
                                    ht = ifelse(sex == 1,
                                                sample(0:1, 70, replace = T, prob = c(0.85, 0.15)),
                                                sample(0:1, 70, replace = T, prob = c(0.80, 0.20))), # imput history of hipertension by sex
                                    cmv = sample(0:1, 70, replace = T, prob = c(0.2, 0.8)), # imput 1 when CMV+
                                    gfr = round(rtnorm(n = 70, mean = 100, sd = 10, lower=80, upper=120)), # imput values for eGFR
                                    hospital = round(rtnorm(n = 70, mean = 30, sd = 10, lower=0, upper=90)), # days in hospital
                                    dri = exp((0.023 * (age-50)) +
                                                (-0.152 * (height - 170) / 10) +
                                                (0.149 * ht) +
                                                (-0.184 * sex) +
                                                (0.190 * cmv) +
                                                (-0.023 * (gfr-90)/10) +
                                                (0.015 * hospital)), # compute value for Donor Risk Index
                                    DRI = case_when(dri <=0.79 ~ 'D1',
                                                    dri <= 1.12 ~ 'D2',
                                                    dri <= 1.5 ~ 'D3',
                                                    TRUE ~ 'D4')) # compute 4 levels Donor Risk Index from dri values

donors.uk <- ex.donors.uk %>% 
  mutate(hypertensive = as.logical(ht)) %>% 
  select(ID, bg, height, hypertensive)

# results from UK algorithm on KARS app
txs.uk <- read_csv2("files/UK_results20210212.csv") 
txs.uk <- txs.uk %>%
  select(-bg, -A1, -A2, -B1, -B2, -DR1, -DR2, -matchability, -cPRA, -Tier, -pointsUK)

# JOIN candidates and donors characteristics
txs.uk <- txs.uk %>% 
  left_join(candidates.uk %>% select(ID, diabetesR),
            by=c("ID"="ID")) %>% 
  left_join(donors.uk %>% select(-bg),
            by=c("donor"="ID")) %>% 
  # mutate(timeD = ifelse(dialysis/12 < 1, "<1 yr", #"<1 yr", ">=1yr, <3yr",">=3yr, <=5yr", ">5yr"
  #                       ifelse(dialysis/12 < 3, ">=1yr, <3yr",
  #                              ifelse(dialysis/12 <= 5, ">=3yr, <=5yr", ">5yr"))),
  #        ageR = ifelse(age < 35, "18-34", # "35-49", "50-64", "65+"
  #                      ifelse(age < 50, "35-49",
  #                             ifelse(age < 65, "50-64", "65+")))) %>%
  rowwise() %>% 
  mutate(epts = epts(age = age,
                     diabetes = diabetesR,
                     #priortx = F,
                     tdialysis = dialysis)$score_EPTS,
         kdpi = kdpi(age = donor_age
                     #, raceAA = F
                     , hypertension = hypertensive
                     #, diabetes = F
                     #, creatinine = 0.9
                     #, stroke = F
                     , height = height
                     #, weight = 80
                     #, dcd = F
                     #, hcv = F
                     , mmB = mmB
                     , mmDR = mmDR
                     #, cold = 15
                     #, enbloc = F
                     #, double = F
                     )$score_KDPI,
         txscore = txscore(ageR = age # alterado para variavel continua; usa ageR para .v0
                           #, race = "White"
                           #, insurance = 0
                           #, causeESRD = "Other"
                           , timeD = dialysis # alterado para variavel continua; usa timeD para .v0
                           , diabetesR = diabetesR
                           #, coronary = F
                           #, albumin = 1.5
                           #, hemoglobin = 10
                           , ageD = donor_age
                           #, diabetesD= "Absence"
                           #, ECD = F
                           , mmHLA_A = mmA
                           , mmHLA_B = mmB
                           , mmHLA_DR = mmDR
                           # , mmHLA = ifelse(mmHLA == 0, "0", # usado para .v0
                           #                  ifelse(mmHLA < 4, "1-3","4-6"))
                                            
                                            )$prob5y,
         USgood = ifelse(epts< 40 & kdpi < 40, 1, 2),
         # age_N = case_when(donor_age < 30 ~ "<30",
         #                   donor_age < 40 ~ "30-39",
         #                   donor_age < 50 ~ "40-49",
         #                   donor_age < 60 ~ "50-59", 
         #                   donor_age < 60 ~ "60-69", 
         #                   TRUE ~ "70+"),
         hyper_N = ifelse(hypertensive == T, "Yes; duration unknown", "None"),
         # HLAmm_N = case_when(mmHLA == 0 ~ "0", #c("0", "1-2", "3-4", "5-6")
         #                     mmHLA < 3 ~ "1-2",
         #                     mmHLA <5 ~ "3-4",
         #                     TRUE ~ "5-6"),
         Nyberg_score = nybergscore (age = donor_age
                                     , hyper = hyper_N
                                     #, cretin = "100+"
                                     , HLAmm_A = mmA
                                     , HLAmm_B = mmB
                                     , HLAmm_DR = mmDR
                                     #, HLAmm = HLAmm_N
                                     #, CVA = FALSE
                                     )
         ) %>% 
  ungroup()
  
# ROC plots
library(pROC)

rr<-roc(txs.uk$USgood, txs.uk$txscore)

dataroc<-data.frame(cutoff=round(rr$thresholds,1),
                    sensitivity=round(rr$sensitivities, 2),
                    specificity=round(rr$specificities, 2))

#YOUDEN index
# coords(rr, "best")
youden <-  coords(rr, x="best", input="threshold", best.method="youden", transpose = FALSE)[1]

#txs.uk %>% count(USgood)

# txs.uk %>% ggplot(aes(txscore, Nyberg_score)) + geom_point() + geom_smooth(method = "lm")
# 
# with(txs.uk,
#      cor.test(txscore, Nyberg_score
#               #, data=txs.score
#      ))

