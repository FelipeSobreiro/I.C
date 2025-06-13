library(microdatasus)
library(dplyr)
library(tidyr)
library("writexl")

#Read SIH - hospital Information System for US$ values
SIH_2021 <- fetch_datasus(
  year_start = 2021,
  #uf = "AC",
  month_start = 1,
  year_end = 2021,
  vars = c("DIAG_PRINC", "DIAGSEC1", "DIAGSEC2", "DIAGSEC3", "US_TOT", "IDADE", "SEXO", "VAL_TOT"),
  month_end = 12,
  information_system = "SIH-RD"
)  

SIH_2021 <- process_sih(SIH_2021)

#create group disease based on ICD
#create age groups
SIH_2021 <- SIH_2021 %>%
  mutate(
    us_val = as.numeric(US_TOT),
    val_REAIS =as.numeric(VAL_TOT),
    idade_n = as.numeric(IDADE),
    causa_group = case_when(
      substr(DIAG_PRINC, 1, 4) %in% c("E11", "E12", "E13", "E14") ~ "Diabetes",
      substr(DIAGSEC1, 1, 4)   %in% c("E11", "E12", "E13", "E14") ~ "Diabetes",
      substr(DIAGSEC2, 1, 4)   %in% c("E11", "E12", "E13", "E14") ~ "Diabetes",
      substr(DIAGSEC3, 1, 4)   %in% c("E11", "E12", "E13", "E14") ~ "Diabetes",
      substr(DIAG_PRINC, 1, 4) %in% c("I20", "I21", "I22", "I23", "I24", "I25") ~ "Isch_HD",
      substr(DIAGSEC1, 1, 4)   %in% c("I20", "I21", "I22", "I23", "I24", "I25") ~ "Isch_HD",
      substr(DIAGSEC2, 1, 4)   %in% c("I20", "I21", "I22", "I23", "I24", "I25") ~ "Isch_HD",
      substr(DIAGSEC3, 1, 4)   %in% c("I20", "I21", "I22", "I23", "I24", "I25") ~ "Isch_HD",
      substr(DIAG_PRINC, 1, 4) %in% c("I10", "I11", "I12", "I13", "I14") ~ "Hypert_HD",
      substr(DIAGSEC1, 1, 4)   %in% c("I10", "I11", "I12", "I13", "I14") ~ "Hypert_HD",
      substr(DIAGSEC2, 1, 4)   %in% c("I10", "I11", "I12", "I13", "I14") ~ "Hypert_HD",
      substr(DIAGSEC3, 1, 4)   %in% c("I10", "I11", "I12", "I13", "I14") ~ "Hypert_HD",
      
      #ACRÉSCIMO DE NOVAS DOENÇAS
      substr(DIAG_PRINC, 1, 4) %in% c("I64") ~ "Stroke",
      substr(DIAGSEC1, 1, 4)   %in% c("I64") ~ "Stroke",
      substr(DIAGSEC2, 1, 4)   %in% c("I64") ~ "Stroke",
      substr(DIAGSEC3, 1, 4)   %in% c("I64") ~ "Stroke",
 
      substr(DIAG_PRINC, 1, 4) %in% c("M16") ~ "OsteoA",
      substr(DIAGSEC1, 1, 4)   %in% c("M16") ~ "OsteoA",
      substr(DIAGSEC2, 1, 4)   %in% c("M16") ~"OsteoA",
      substr(DIAGSEC3, 1, 4)   %in% c("M16") ~ "OsteoA",
      #NOVA DOENÇA
      substr(DIAG_PRINC, 1, 4) %in% c("M17") ~ "OsteoA",
      substr(DIAGSEC1, 1, 4)   %in% c("M17") ~ "OsteoA",
      substr(DIAGSEC2, 1, 4)   %in% c("M17") ~ "OsteoA",
      substr(DIAGSEC3, 1, 4)   %in% c("M17") ~ "OsteoA",
      
      substr(DIAG_PRINC, 1, 4) %in% c("M545") ~ "Low_back_pain",
      substr(DIAGSEC1, 1, 4)   %in% c("M545") ~ "Low_back_pain",
      substr(DIAGSEC2, 1, 4)   %in% c("M545") ~ "Low_back_pain",
      substr(DIAGSEC3, 1, 4)   %in% c("M545") ~ "Low_back_pain",
      
      
      .default = "Other"
    ),
    age_cat = cut(
      idade_n, 
      breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 94, 99),
      labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"),
      include.lowest = TRUE
    )
  )


#create table - single age
BR_Single_age2021 <- SIH_2021 %>%
  filter(idade_n >= 0 & idade_n <= 99) %>%
  group_by(causa_group, SEXO, idade_n) %>%
  summarize(
    us_val_total = sum(us_val, na.rm = TRUE),
    BR_val_total =sum(val_REAIS, na.rm = TRUE),
    n = n(),
    media = mean(us_val, na.rm = TRUE),
    media_BR = mean(val_REAIS, na.rm = TRUE),
    sd = sd(us_val, na.rm = TRUE),
    sd_BR = sd(val_REAIS, na.rm =TRUE),
    .groups = 'drop'
  ) %>%
  complete(causa_group, SEXO, idade_n = 0:99)

# create table - age group
BR_age_cat2021 <- SIH_2021 %>%
  filter(idade_n >= 0 & idade_n <= 99) %>%
  group_by(causa_group, SEXO, age_cat) %>%
  summarize(
    us_val_total = sum(us_val, na.rm = TRUE),
    BR_val_total =sum(val_REAIS, na.rm = TRUE),
    n = n(),
    media = mean(us_val, na.rm = TRUE),
    media_BR = mean(val_REAIS, na.rm = TRUE),
    sd = sd(us_val, na.rm = TRUE),
    sd_BR =sd(val_REAIS, na.rm =TRUE),
    .groups = 'drop'
  ) %>%
  complete(causa_group, SEXO, age_cat)


 Vigitel2006_2023_Luciana %>%
  filter(q6 >= 0 & q6 <= 99) %>%
  group_by(q6, q7, q9, q11, ano) %>%
  summarize(
    IMC_total = sum(imc_i, na.rm = TRUE),
    n = n(),
    media = mean(imc_i, na.rm = TRUE),
    sd = sd(imc_i, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  complete(q6 = 0:99, q7, q9, q11, ano)

 #----------------------------------------------#
 
 for (year in unique(Vigitel2006_2023_Luciana$ano)) {
   subset_data <- Vigitel2006_2023_Luciana %>%
     filter(ano == year)
   
   # Processa os dados subsetados
   result <- subset_data %>%
     filter(q6 >= 0 & q6 <= 99) %>%
     group_by(q6, q7, q9, q11, ano) %>%
     summarize(
       IMC_total = sum(imc_i, na.rm = TRUE),
       n = n(),
       media = mean(imc_i, na.rm = TRUE),
       sd = sd(imc_i, na.rm = TRUE),
       .groups = 'drop'
     )
   
   # Combine com os resultados anteriores
   if (exists("BMI_T")) {
     BMI_T <- bind_rows(BMI_T, result)
   } else {
     BMI_T <- result
   }
 }
 
 BMI_T <- Vigitel2006_2023_Luciana %>%
   filter(q6 >= 0 & q6 <= 99) %>%  
   group_by(q7, q6, ano) %>%      
   summarize(
     IMC_total = sum(imc_i, na.rm = TRUE),    
     n = n(),                              
     media = mean(imc_i, na.rm = TRUE),      
     sd = sd(imc_i, na.rm = TRUE),           
     .groups = 'drop'
   ) %>%
   arrange(q7, q6, ano) 
 
 BMI_T <- Vigitel2006_2023_Luciana %>%
   filter(q6 >= 0 & q6 <= 99) %>%  
   group_by(q7, q6,ano) %>%          
   summarize(
     IMC_total = sum(imc_i, na.rm = TRUE),    
     n = n(),
    sd= sd(imc_i,na.rm =TRUE),
     media = IMC_total / n,                  
     .groups = 'drop'
   ) %>%
   arrange(q7, q6,ano)  
 
 # Converter bmi_cat para numérico
 Vigitel2006_2023_Luciana <- Vigitel2006_2023_Luciana %>%
   mutate(bmi_cat = as.numeric(bmi_cat))
 
 # Calcular a soma e a média
 BMI_total <- Vigitel2006_2023_Luciana %>%
   filter(q6 >= 0 & q6 <= 99) %>%  
   group_by(q19, q27, q29, ano, q7) %>%
   summarize(
     BMI_total = sum(bmi_cat, na.rm = TRUE),  # Soma de bmi_cat
     n = n(),                                # Número de observações
     sd = sd(bmi_cat, na.rm = TRUE),         # Desvio padrão de bmi_cat
     media = mean(bmi_cat, na.rm = TRUE),    # Média de bmi_cat
     .groups = 'drop'
   )
 

# Export to Excel
write_xlsx(BR_Single_age2021, "Cost_BR2021all.xlsx")
write_xlsx(BR_age_cat2021 , "Cost_BRgr2021all.xlsx")
