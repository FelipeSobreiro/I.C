library(microdatasus)
library(dplyr)
library(tidyr)
library(tidyverse)
library(eeptools)



#------------------------------------------------processo data sus ACRE------------------------------------------#

SIH_RD <- fetch_datasus(year_start = 2019,
                        month_start = 1,
                        year_end = 2019, 
                        month_end = 12, uf = "AC", information_system = "SIH-RD",vars =c("US_TOT","SEXO", "NASC","IDADE"))



temp1 <- process_sih(SIH_RD)


#------------------------------------Feminino intervalo de 15-19-------------------------------------------------------------------#


Idade_15_19_F <-temp1

Idade_15_19_F <-temp1 %>% filter(IDADE >= 15 & IDADE <= 19, SEXO == "Feminino" )


Idade_15_19_F$US_TOT <-as.numeric(Idade_15_19_F$US_TOT)


SOMA_TOTAL_15_19_F <- sum(Idade_15_19_F$US_TOT)


print(SOMA_TOTAL_15_19_F)

#------------------------------------------------F intervalo 20-24---------------------------------------------------------#


Idade_20_24_F <-temp1

Idade_20_24_F <-temp1 %>% filter(IDADE >= 20 & IDADE <= 24, SEXO == "Feminino" )


Idade_20_24_F$US_TOT <-as.numeric(Idade_20_24_F$US_TOT)


SOMA_TOTAL_20_24_F <- sum(Idade_20_24_F$US_TOT)

print(SOMA_TOTAL_20_24_F)


#-----------------------------------------------F intervalo 25-29-------------------------------------------------#


Idade_25_29_F <-temp1

Idade_25_29_F <-temp1 %>% filter(IDADE >= 25 & IDADE <= 29, SEXO == "Feminino" )


Idade_25_29_F$US_TOT <-as.numeric(Idade_25_29_F$US_TOT)


SOMA_TOTAL_25_29_F <- sum(Idade_25_29_F$US_TOT)

print(SOMA_TOTAL_25_29_F)


#----------------------------------------------------F intervalo 30-34---------------------------------------------------#

Idade_30_34_F <-temp1

Idade_30_34_F <-temp1 %>% filter(IDADE >= 30 & IDADE <= 34, SEXO == "Feminino" )


Idade_30_34_F$US_TOT <-as.numeric(Idade_30_34_F$US_TOT)


SOMA_TOTAL_30_34_F <- sum(Idade_30_34_F$US_TOT)

print(SOMA_TOTAL_30_34_F)



#---------------------------------------------- F 35-39 -----------------------------------------------------------------#

Idade_35_39_F <-temp1

Idade_35_39_F <-temp1 %>% filter(IDADE >= 35 & IDADE <= 39, SEXO == "Feminino" )


Idade_35_39_F$US_TOT <-as.numeric(Idade_35_39_F$US_TOT)


SOMA_TOTAL_35_39_F <- sum(Idade_35_39_F$US_TOT)

print(SOMA_TOTAL_35_39_F)


#-----------------------------------------F 40-44----------------------------------------------------------------#


Idade_40_44_F <-temp1

Idade_40_44_F <-temp1 %>% filter(IDADE >= 40 & IDADE <= 44, SEXO == "Feminino" )


Idade_40_44_F$US_TOT <-as.numeric(Idade_40_44_F$US_TOT)


SOMA_TOTAL_40_44_F <- sum(Idade_40_44_F$US_TOT)

print(SOMA_TOTAL_40_44_F)




#-----------------------------------------f 45-49-----------------------------------------------------------------------#


Idade_45_49_F <-temp1

Idade_45_49_F <-temp1 %>% filter(IDADE >= 45 & IDADE <= 49, SEXO == "Feminino" )


Idade_45_49_F$US_TOT <-as.numeric(Idade_45_49_F$US_TOT)


SOMA_TOTAL_45_49_F <- sum(Idade_45_49_F$US_TOT)

print(SOMA_TOTAL_45_49_F)

#---------------------------------------f 50-54-------------------------------------------------------------------#


Idade_50_54_F <-temp1

Idade_50_54_F <-temp1 %>% filter(IDADE >= 50 & IDADE <= 54, SEXO == "Feminino" )


Idade_50_54_F$US_TOT <-as.numeric(Idade_50_54_F$US_TOT)


SOMA_TOTAL_50_54_F <- sum(Idade_50_54_F$US_TOT)

print(SOMA_TOTAL_50_54_F)


#-------------------------------------- F 55-59-------------------------------------------------------------------#

Idade_55_59_F <-temp1

Idade_55_59_F <-temp1 %>% filter(IDADE >= 55 & IDADE <= 59, SEXO == "Feminino" )


Idade_55_59_F$US_TOT <-as.numeric(Idade_55_59_F$US_TOT)


SOMA_TOTAL_55_59_F <- sum(Idade_55_59_F$US_TOT)

print(SOMA_TOTAL_55_59_F)

#-----------------------------------F 60-64 ------------------------------------------------------------------------------#


Idade_60_64_F <-temp1

Idade_60_64_F <-temp1 %>% filter(IDADE >= 60 & IDADE <= 64, SEXO == "Feminino" )


Idade_60_64_F$US_TOT <-as.numeric(Idade_60_64_F$US_TOT)


SOMA_TOTAL_60_64_F <- sum(Idade_60_64_F$US_TOT)

print(SOMA_TOTAL_60_64_F)


#----------------------------------------------F 65-69----------------------------------------------------------------#


Idade_65_69_F <-temp1

Idade_65_69_F <-temp1 %>% filter(IDADE >= 65 & IDADE <= 69, SEXO == "Feminino" )


Idade_65_69_F$US_TOT <-as.numeric(Idade_65_69_F$US_TOT)


SOMA_TOTAL_65_69_F <- sum(Idade_65_69_F$US_TOT)

print(SOMA_TOTAL_65_69_F)


#--------------------------------------F 70-74--------------------------------------------------------------------------#


Idade_70_74_F <-temp1

Idade_70_74_F <-temp1 %>% filter(IDADE >= 70 & IDADE <= 74, SEXO == "Feminino" )


Idade_70_74_F$US_TOT <-as.numeric(Idade_70_74_F$US_TOT)


SOMA_TOTAL_70_74_F <- sum(Idade_70_74_F$US_TOT)

print(SOMA_TOTAL_70_74_F)

#-----------------------------------F 75-79----------------------------------------------------------------------------#


Idade_75_79_F <-temp1

Idade_75_79_F <-temp1 %>% filter(IDADE >= 75 & IDADE <= 79, SEXO == "Feminino" )


Idade_75_79_F$US_TOT <-as.numeric(Idade_75_79_F$US_TOT)


SOMA_TOTAL_75_79_F <- sum(Idade_75_79_F$US_TOT)

print(SOMA_TOTAL_75_79_F)


#----------------------------------------F 80-84-------------------------------------------------------------------#

Idade_80_84_F <-temp1

Idade_80_84_F <-temp1 %>% filter(IDADE >= 80 & IDADE <= 84, SEXO == "Feminino" )


Idade_80_84_F$US_TOT <-as.numeric(Idade_80_84_F$US_TOT)


SOMA_TOTAL_80_84_F <- sum(Idade_80_84_F$US_TOT)

print(SOMA_TOTAL_80_84_F)

#----------------------------------------F 85+ --------------------------------------------#

Idade_85mais_F <-temp1

Idade_85mais_F <-temp1 %>% filter(IDADE >= 80, SEXO == "Feminino" )


Idade_85mais_F$US_TOT <-as.numeric(Idade_85mais_F$US_TOT)


SOMA_TOTAL_85mais_F <- sum(Idade_85mais_F$US_TOT)

print(SOMA_TOTAL_85mais_F)






n_obs <- nrow(Idade_15_19_F)

# Calculando a média
media_15_19_F <- SOMA_TOTAL_15_19_F / n_obs

# Imprimindo a média
print(media_15_19_F)

