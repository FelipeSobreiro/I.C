install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")

#install.packages("devtools")
#install.packages("usethis")
#install.packages("read.dbc") - instalar do computador "C:\Users\lbnuc\OneDrive\Pesquisa\Projeto Universal CNPq\Dados mortalidade 2019 SIM\read.dbc_1.0.6.tar.gz"
#install.packages("writexl")
#devtools::install_github("rfsaldanha/microdatasus")
#library(usethis)

#library(writexl)
#library(foreign)
#library(devtools)
library(epikit)
library(read.dbc)
library(microdatasus)
library(eeptools)
library(doParallel)
library(dplyr)
registerDoParallel(cores=4) #as many physical cores as available.

library(lubridate)

ano<-2019

crude_data <- fetch_datasus(year_start = ano, year_end = ano, information_system = "SIM-DO")
dat_SIM<- process_sim(data=crude_data)
dim_crude<-dim(crude_data)
rm(crude_data)
Sim2019<-dat_SIM  #n=1349801 total
rm(dat_SIM)

#cria idade
temp1<-Sim2019[!is.na(Sim2019$DTNASC),] # exclui missing na data de nascimento
dim_1<-dim(temp1) # n=1346010
rm(Sim2019)

temp2<-temp1[!is.na(temp1$DTOBITO),] # exclui missing na data do óbito (não tem!)
rm(temp1)
dim_2<-dim(temp2) # n=1346010
startDate = as.Date(temp2$DTNASC);
endDate = as.Date(temp2$DTOBITO);

temp2$Idade_calc<-trunc(age_calc(dob = startDate, enddate = endDate, units = "years"))

# seleciona >=15 anos
temp3<-subset(temp2,Idade_calc>=15)
rm(temp2)
dim_3<-dim(temp3) # n=1297731

# cria categorias de idade
temp3$age_cat = age_categories(temp3$Idade_calc, breakers = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85))
table(temp3$age_cat)

# cria causa básica para Prime
temp3$causa_group<-case_when(substr(temp3$CAUSABAS,1,2)=="I6"~"01-CVD",   #I60-I69: Cerebrovascular diseases
                             substr(temp3$CAUSABAS,1,3)=="I20"~"02-IHD",  #I20-I25: Ischaemic heart diseases
                             substr(temp3$CAUSABAS,1,3)=="I21"~"02-IHD",
                             substr(temp3$CAUSABAS,1,3)=="I22"~"02-IHD",
                             substr(temp3$CAUSABAS,1,3)=="I23"~"02-IHD",
                             substr(temp3$CAUSABAS,1,3)=="I24"~"02-IHD",
                             substr(temp3$CAUSABAS,1,3)=="I25"~"02-IHD",
                             substr(temp3$CAUSABAS,1,2)=="C0"~"03-CA_lip",  #C00-C14: Lip, oral cavity and pharynx
                             substr(temp3$CAUSABAS,1,3)=="C10"~"03-CA_lip",
                             substr(temp3$CAUSABAS,1,3)=="C11"~"03-CA_lip",
                             substr(temp3$CAUSABAS,1,3)=="C12"~"03-CA_lip",
                             substr(temp3$CAUSABAS,1,3)=="C13"~"03-CA_lip",
                             substr(temp3$CAUSABAS,1,3)=="C14"~"03-CA_lip",
                             substr(temp3$CAUSABAS,1,3)=="C15"~"04-CA_esof", #C15: Oesophagus
                             substr(temp3$CAUSABAS,1,3)=="C16"~"05-CA_esto",	#C16: Stomach
                             substr(temp3$CAUSABAS,1,3)=="C34"~"06-CA_lung", #C34: Bronchus and lung
                             substr(temp3$CAUSABAS,1,3)=="C25"~"07-CA_panc", #C25: Pancreas
                             substr(temp3$CAUSABAS,1,3)=="C18"~"08-CA_colo", #C18-20: Colorectum
                             substr(temp3$CAUSABAS,1,3)=="C19"~"08-CA_colo", 
                             substr(temp3$CAUSABAS,1,3)=="C20"~"08-CA_colo",
                             substr(temp3$CAUSABAS,1,3)=="C50"~"09-CA_mama", #C50: Breast
                             substr(temp3$CAUSABAS,1,4)=="C541"~"10-CA_endo",#C54.1: Endometrium 
                             substr(temp3$CAUSABAS,1,3)=="C23"~"11-CA_vesi", #C23: Gallbladder
                             substr(temp3$CAUSABAS,1,3)=="C64"~"12-CA_kidn", #C64: Kidney
                             substr(temp3$CAUSABAS,1,3)=="I10"~"13-HAS", #I10-I15: Hypertensive disease
                             substr(temp3$CAUSABAS,1,3)=="I11"~"13-HAS",
                             substr(temp3$CAUSABAS,1,3)=="I12"~"13-HAS",
                             substr(temp3$CAUSABAS,1,3)=="I13"~"13-HAS",
                             substr(temp3$CAUSABAS,1,3)=="I14"~"13-HAS",
                             substr(temp3$CAUSABAS,1,3)=="I14"~"13-HAS",
                             substr(temp3$CAUSABAS,1,3)=="E11"~"14-Diab", #E11,E14: Diabetes
                             substr(temp3$CAUSABAS,1,3)=="E12"~"14-Diab",
                             substr(temp3$CAUSABAS,1,3)=="E13"~"14-Diab",
                             substr(temp3$CAUSABAS,1,3)=="E14"~"14-Diab",
                             substr(temp3$CAUSABAS,1,3)=="C67"~"15-CA_Blad", #C67: Bladder cancer
                             substr(temp3$CAUSABAS,1,3)=="C22"~"16-CA_liv",  #C22: Liver cancer
                             substr(temp3$CAUSABAS,1,3)=="C53"~"17-CA_cerv", #C53: Cervix cancer
                             substr(temp3$CAUSABAS,1,3)=="J40"~"18-COPD",    #J40-J44: Chronic obstructive pulmonary disease
                             substr(temp3$CAUSABAS,1,3)=="J41"~"18-COPD",
                             substr(temp3$CAUSABAS,1,3)=="J42"~"18-COPD",
                             substr(temp3$CAUSABAS,1,3)=="J43"~"18-COPD",
                             substr(temp3$CAUSABAS,1,3)=="J44"~"18-COPD",
                             substr(temp3$CAUSABAS,1,3)=="K70"~"19-liver", #K70, K74: Liver disease
                             substr(temp3$CAUSABAS,1,3)=="K74"~"19-liver",
                             substr(temp3$CAUSABAS,1,3)=="I50"~"20-heart", #I50: Heart failure
                             substr(temp3$CAUSABAS,1,3)=="I71"~"21-aneur", #I71: Aortic aneurysm
                             substr(temp3$CAUSABAS,1,3)=="I26"~"22-pulm_emb", #	I26: Pulmonary embolism	
                             substr(temp3$CAUSABAS,1,3)=="I05"~"23-Rheum", #I05-09: Rheumatic heart disease
                             substr(temp3$CAUSABAS,1,3)=="I06"~"23-Rheum",
                             substr(temp3$CAUSABAS,1,3)=="I07"~"23-Rheum",
                             substr(temp3$CAUSABAS,1,3)=="I08"~"23-Rheum",
                             substr(temp3$CAUSABAS,1,3)=="I09"~"23-Rheum",
                             substr(temp3$CAUSABAS,1,3)=="N18"~"24-Renal", #N18: Chronic renal failure
                             TRUE~"Outras_tot")


table(temp3$age_cat,temp3$causa_group,temp3$SEXO)                            
                             	 							
                             
                               
  substr(temp3$CAUSABAS,1,1)=="V"~"acid",
                                substr(sim_1521$CAUSABAS,1,1)=="W"~"acid",
                               
