# Source of Contraception in Nairobi PMA and DHS
# Kristin Bietsch, PhD
# Avenir Health

library(tidyverse)
library(haven)
library(sjlabelled)
library(questionr)
library(survey)
library(jtools)

pma2020 <- read_dta("C:/Users/KristinBietsch/files/PMA2020 Data/Kenya/PMA2020_KEP2_HQFQ_v1.0_25Aug2021_0/PMA2020_KEP2_HQFQ_v1.0_25Aug2021/PMA2020_KEP2_HQFQ_v1.0_25Aug2021.dta")

pma2020_nairobi <- pma2020 %>% filter(HHQ_result==1 & usually_live==1) %>% filter(!is.na(FQweight)) %>%
  filter(county==6) %>% filter(!is.na(fp_provider_rw)) %>% filter(fp_provider_rw!=-88)

lab_provider=get_labels(pma2020_nairobi$fp_provider_rw)
val_provider=get_values(pma2020_nairobi$fp_provider_rw)
Z_provider=as.data.frame(cbind(lab_provider, val_provider)) %>% rename(Var1=val_provider)

lab_method=get_labels(pma2020_nairobi$current_methodnum_rc)
val_method=get_values(pma2020_nairobi$current_methodnum_rc)
Z_method=as.data.frame(cbind(lab_method, val_method)) %>% rename(Var2=val_method)

method_n <- as.data.frame(table(pma2020_nairobi$current_methodnum_rc)) %>% rename(Var2=Var1, N=Freq)

pma2020_nairobi_total <- as.data.frame(prop.table(wtd.table(x = as.factor(pma2020_nairobi$fp_provider_rw), weights = pma2020_nairobi$FQweight))) %>% left_join(Z_provider, by="Var1")
pma2020_nairobi_bymethod <- as.data.frame(prop.table(wtd.table(x = as.factor(pma2020_nairobi$fp_provider_rw), y=(as.factor(pma2020_nairobi$current_methodnum_rc)), weights = pma2020_nairobi$FQweight),2)) %>%
  left_join(Z_provider, by="Var1") %>% left_join(Z_method, by="Var2") %>% left_join(method_n, by="Var2") %>%
  filter(N>=25)

############################################
pma2019 <- read_dta("C:/Users/KristinBietsch/files/PMA2020 Data/Kenya/PMA_KEP1_HQFQ_Baseline_v1.1_15Feb2021/PMA_KEP1_HQFQ_Baseline_v1.1_15Feb2021.dta")
pma2019_nairobi <- pma2019 %>% filter(HHQ_result==1 & usually_live==1) %>% filter(!is.na(FQweight)) %>%
  filter(county==6) %>% filter(!is.na(fp_provider_rw_kn)) %>% filter(fp_provider_rw_kn!=-88)

lab_provider=get_labels(pma2019_nairobi$fp_provider_rw_kn)
val_provider=get_values(pma2019_nairobi$fp_provider_rw_kn)
Z_provider=as.data.frame(cbind(lab_provider, val_provider)) %>% rename(Var1=val_provider)

lab_method=get_labels(pma2019_nairobi$current_methodnum_rc)
val_method=get_values(pma2019_nairobi$current_methodnum_rc)
Z_method=as.data.frame(cbind(lab_method, val_method)) %>% rename(Var2=val_method)

method_n <- as.data.frame(table(pma2019_nairobi$current_methodnum_rc)) %>% rename(Var2=Var1, N=Freq)

pma2019_nairobi_total <- as.data.frame(prop.table(wtd.table(x = as.factor(pma2019_nairobi$fp_provider_rw_kn), weights = pma2019_nairobi$FQweight))) %>% left_join(Z_provider, by="Var1")
pma2019_nairobi_bymethod <- as.data.frame(prop.table(wtd.table(x = as.factor(pma2019_nairobi$fp_provider_rw_kn), y=(as.factor(pma2019_nairobi$current_methodnum_rc)), weights = pma2019_nairobi$FQweight),2)) %>%
  left_join(Z_provider, by="Var1") %>% left_join(Z_method, by="Var2") %>% left_join(method_n, by="Var2") %>%
  filter(N>=25)

############################################
pma2018 <- read_dta("C:/Users/KristinBietsch/files/PMA2020 Data/Kenya/PMA2018_KER7_HHQFQ_v2_2Dec2019/PMA2018_KER7_HHQFQ_2Dec2019.dta")
pma2018_nairobi <- pma2018 %>% filter(HHQ_result==1 & usually_live==1) %>% filter(!is.na(FQweight)) %>%
  filter(county==6) %>% filter(!is.na(fp_provider_rw_kn)) %>% filter(fp_provider_rw_kn!=-88)

lab_provider=get_labels(pma2018_nairobi$fp_provider_rw_kn)
val_provider=get_values(pma2018_nairobi$fp_provider_rw_kn)
Z_provider=as.data.frame(cbind(lab_provider, val_provider)) %>% rename(Var1=val_provider)

lab_method=get_labels(pma2018_nairobi$current_methodnum_rc)
val_method=get_values(pma2018_nairobi$current_methodnum_rc)
Z_method=as.data.frame(cbind(lab_method, val_method)) %>% rename(Var2=val_method)

method_n <- as.data.frame(table(pma2018_nairobi$current_methodnum_rc)) %>% rename(Var2=Var1, N=Freq)

pma2018_nairobi_total <- as.data.frame(prop.table(wtd.table(x = as.factor(pma2018_nairobi$fp_provider_rw_kn), weights = pma2018_nairobi$FQweight))) %>% left_join(Z_provider, by="Var1")
pma2018_nairobi_bymethod <- as.data.frame(prop.table(wtd.table(x = as.factor(pma2018_nairobi$fp_provider_rw_kn), y=(as.factor(pma2018_nairobi$current_methodnum_rc)), weights = pma2018_nairobi$FQweight),2)) %>%
  left_join(Z_provider, by="Var1") %>% left_join(Z_method, by="Var2") %>% left_join(method_n, by="Var2") %>%
  filter(N>=25)

############################################
pma2017 <- read_dta("C:/Users/KristinBietsch/files/PMA2020 Data/Kenya/PMA2017_KER6_HHQFQ_v1_28Aug2018/PMA2017_KER6_HHQFQ_v1_28Aug2018.dta")
pma2017_nairobi <- pma2017 %>% filter(HHQ_result==1 & usually_live==1) %>% filter(!is.na(FQweight)) %>%
  filter(county==6) %>% filter(!is.na(fp_provider_rw)) %>% filter(fp_provider_rw!=-88)

lab_provider=get_labels(pma2017_nairobi$fp_provider_rw)
val_provider=get_values(pma2017_nairobi$fp_provider_rw)
Z_provider=as.data.frame(cbind(lab_provider, val_provider)) %>% rename(Var1=val_provider)

lab_method=get_labels(pma2017_nairobi$current_methodnum_rc)
val_method=get_values(pma2017_nairobi$current_methodnum_rc)
Z_method=as.data.frame(cbind(lab_method, val_method)) %>% rename(Var2=val_method)

method_n <- as.data.frame(table(pma2017_nairobi$current_methodnum_rc)) %>% rename(Var2=Var1, N=Freq)

pma2017_nairobi_total <- as.data.frame(prop.table(wtd.table(x = as.factor(pma2017_nairobi$fp_provider_rw), weights = pma2017_nairobi$FQweight))) %>% left_join(Z_provider, by="Var1")
pma2017_nairobi_bymethod <- as.data.frame(prop.table(wtd.table(x = as.factor(pma2017_nairobi$fp_provider_rw), y=(as.factor(pma2017_nairobi$current_methodnum_rc)), weights = pma2017_nairobi$FQweight),2)) %>%
  left_join(Z_provider, by="Var1") %>% left_join(Z_method, by="Var2") %>% left_join(method_n, by="Var2") %>%
  filter(N>=25)

############################################
pma2016 <- read_dta("C:/Users/KristinBietsch/Files/PMA2020 Data/Kenya/PMA2016_KER5_HHQFQ_v1_27Jul2017/PMA2016_KER5_HHQFQ_v1_27Jul2017.dta")
pma2016_nairobi <- pma2016 %>% filter(HHQ_result==1 & usually_live==1) %>% filter(!is.na(FQweight)) %>%
  filter(county==6) %>% filter(!is.na(fp_provider_rw)) %>% filter(fp_provider_rw!=-88)

lab_provider=get_labels(pma2016_nairobi$fp_provider_rw)
val_provider=get_values(pma2016_nairobi$fp_provider_rw)
Z_provider=as.data.frame(cbind(lab_provider, val_provider)) %>% rename(Var1=val_provider)

lab_method=get_labels(pma2016_nairobi$current_methodnum_rc)
val_method=get_values(pma2016_nairobi$current_methodnum_rc)
Z_method=as.data.frame(cbind(lab_method, val_method)) %>% rename(Var2=val_method)

method_n <- as.data.frame(table(pma2016_nairobi$current_methodnum_rc)) %>% rename(Var2=Var1, N=Freq)

pma2016_nairobi_total <- as.data.frame(prop.table(wtd.table(x = as.factor(pma2016_nairobi$fp_provider_rw), weights = pma2016_nairobi$FQweight))) %>% left_join(Z_provider, by="Var1")
pma2016_nairobi_bymethod <- as.data.frame(prop.table(wtd.table(x = as.factor(pma2016_nairobi$fp_provider_rw), y=(as.factor(pma2016_nairobi$current_methodnum_rc)), weights = pma2016_nairobi$FQweight),2)) %>%
  left_join(Z_provider, by="Var1") %>% left_join(Z_method, by="Var2") %>% left_join(method_n, by="Var2") %>%
  filter(N>=25)

###############################################
##############################################
# dhs 2014
dhs2014 <- read_dta("C:/Users/KristinBietsch/Files/DHSLoop/KEIR71FL.DTA")
dhs2014_nairobi <- dhs2014 %>%
  filter(scounty==90) %>% filter(!is.na(v326)) 

lab_provider=get_labels(dhs2014_nairobi$v326)
val_provider=get_values(dhs2014_nairobi$v326)
Z_provider=as.data.frame(cbind(lab_provider, val_provider)) %>% rename(Var1=val_provider)

lab_method=get_labels(dhs2014_nairobi$v312)
val_method=get_values(dhs2014_nairobi$v312)
Z_method=as.data.frame(cbind(lab_method, val_method)) %>% rename(Var2=val_method)

method_n <- as.data.frame(table(dhs2014_nairobi$v312)) %>% rename(Var2=Var1, N=Freq)

dhs2014_nairobi_total <- as.data.frame(prop.table(wtd.table(x = as.factor(dhs2014_nairobi$v326), weights = dhs2014_nairobi$v005))) %>% left_join(Z_provider, by="Var1")
dhs2014_nairobi_bymethod <- as.data.frame(prop.table(wtd.table(x = as.factor(dhs2014_nairobi$v326), y=(as.factor(dhs2014_nairobi$v312)), weights = dhs2014_nairobi$v005),2)) %>%
  left_join(Z_provider, by="Var1") %>% left_join(Z_method, by="Var2") %>% left_join(method_n, by="Var2") %>%
  filter(N>=25)
