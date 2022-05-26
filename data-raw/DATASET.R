## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

library(tidyverse)
library(data.table)

# Model output data
data_raw <-data.table::fread("data-raw/220303_random300grouped_reallakes_V4exp_300grouped_all_V4.csv")

data <- data_raw %>%
  rename(Species = specName) %>%
  mutate(Biomass=depth_1+depth_2+depth_3+depth_4) %>% # REEVALUATE
  ungroup() %>%
  mutate(Biomass_cat = ifelse(Biomass == 0, 0, 1)) %>%
  rename(Lake = lakeName) %>% select(-"V1") %>%
  filter(Lake != "lake_11")

# PARAMETERS for Lake and Species - if necessary output FOR FURTHER ANALYSIS
# data_lakes_env <-
#   data %>% group_by(Lake) %>% select(
#     "Lake",
#     "latitude",
#     #"minI","maxI","iDelay","maxW","minW","minTemp",
#     #"tempDelay","minKd","levelCorrection"
#     "maxNutrient",
#     "maxTemp",
#     "maxKd"
#   ) %>% unique() %>%
#   select(where( ~ n_distinct(.) > 1))
# # fwrite(data_lakes_env, "Lakes_env_2.csv")
#
# data_spec_para <- data %>% group_by(Species) %>%
#   select(
#     -"Lake",
#     -"lakeNr",-"latitude",
#     #-"minI",-"maxI",-"iDelay",-"minTemp",
#     -"Name",-"maxNutrient",
#     -"maxW",
#     -"minW",
#     -"maxTemp",-"tempDelay",
#     -"maxKd",
#     -"minKd",
#     -"levelCorrection",-"depth_1",
#     -"depth_2",
#     -"depth_3",
#     -"depth_4",
#     -"Biomass",
#     -"Biomass_cat"
#   ) %>%
#   unique() %>%
#   select(where( ~ n_distinct(.) > 1))
# # fwrite(data_spec_para, "Species_parameters_2.csv")
#
#
# # Optional: read in here
# spec_para <-  data_spec_para
# #fread(paste0(wd, "/2_Macroph/scenarios/species_parameters_2.csv"))
# env_para_base <- data_lakes_env
# #fread(paste0(wd, "/2_Macroph/scenarios/Lakes_env_2.csv"))
# lakenames <- data_lakes_env
# #fread(paste0(wd,"/scenarios/Lakes_env.csv"))
#
#
# # List with names of species that can grow in base scenario
# speciesBase <- data %>% filter(Biomass>0) %>% select(Species) %>% unique()
# #data %>% select(Species) %>% unique()
# # top50<-data %>%
# #   mutate(Trophie = ifelse(specNr %in% c(14001:14300), "oligo",
# #                           ifelse(specNr %in% c(15001:15300), "meso",
# #                                  ifelse(specNr %in% c(16001:16300), "eu",NA)))) %>%
# #   filter(Biomass>0) %>% select(Species,Trophie)%>% unique() %>%
# #   group_by(Trophie) %>% slice_head(n=50) %>% ungroup() %>%select(Species)
# # speciesBase_Rest<-speciesBase[!speciesBase$Species %in% top50$Species]
# # fwrite(speciesBase_Rest, "survSpec_Base_Rest_V4.csv")
#
# survSpec_para<- data %>% filter(Biomass>0) %>% group_by(specNr) %>%
#   summarise_all(mean) %>%
#   select(-lakeNr,-Species,-Lake,-Name)

usethis::use_data(data, overwrite = TRUE)



## Preprocess todays real species distribution
# # Read in raw data
MacrophData_raw<-fread("data-raw/Makrophyten_WRRL_05-17_nurMakrophytes.csv")

## Filter data ----
### For complete datasets ----
MacrophData <- MacrophData_raw %>%
  filter(!(Gewässer=="Chiemsee" & (YEAR==2011))) %>%
  filter(!(Gewässer=="Chiemsee" & YEAR==2012)) %>% # 1 plot per year -> wrong
  filter(!(Gewässer=="Chiemsee" & (YEAR==2014))) %>%
  filter(!(Gewässer=="Chiemsee" & (YEAR==2015))) %>%
  filter(!(Gewässer=="Chiemsee" & (YEAR==2017))) %>%
  filter(!(Gewässer=="Staffelsee - Sued" & (YEAR==2012))) %>%
  filter(!(Gewässer=="Gr. Alpsee" & (YEAR==2012))) %>%
  filter(!(Gewässer=="Gr. Alpsee" & (YEAR==2013))) %>%
  filter(!(Gewässer=="Pilsensee" & (YEAR==2015))) %>%
  filter(!(Gewässer=="Langbuergner See" & (YEAR==2014))) %>%
  filter(!(Gewässer=="Pelhamer See" & (YEAR==2017))) %>%
  filter(!(Gewässer=="Weitsee" & (YEAR==2017))) %>%
  distinct()

### Rename depth ----
MacrophData$Probestelle <- plyr::revalue(MacrophData$Probestelle, c("0-1 m"="0-1", "1-2 m"="1-2", "2-4 m"="2-4",">4 m"="4-x" ))

### Selection of species that were determined until species level ----
MacrophData <- MacrophData %>%
  filter(str_detect(Taxon, " ")) %>%
  filter(Taxon != "Ranunculus, aquatisch")

### Reduce columns
MacrophData <- MacrophData %>% select(-`Mst.-Name`,-`DV-Nr.`,-Dimension, -CF, -LAKE_TYPE_SIMPLIFIED, -LAKE_TYPE2, -LAKE_TYPE)



## Dataset with all possible PLOTS ----
Makroph_dataset <- MacrophData %>% group_by(Gewässer, MST_NR, YEAR) %>%
  select(Gewässer, MST_NR, YEAR) %>% #3590 * Gew?sser, MST_NR, YEAR, Probestelle IIII 1013 *4 => 4052 m?sstens eigentlich mind sein
  distinct()
Probestelle <- tibble(Probestelle=c("4-x","0-1","1-2", "2-4")) # tibble(Probestelle)
Makroph_dataset <- merge(Makroph_dataset, Probestelle, by=NULL) #996 * 4 = 3984

## Select for submerged species ----
Makroph_comm_S2 <- MacrophData %>% #group_by(Gewässer, MST_NR, DATUM, Probestelle, Taxon) %>%
  filter(Form=="S") %>%
  filter(Erscheinungsform!="Bryophyta" ) %>%
  filter(Erscheinungsform!="Pteridophyta" ) %>%

  #ungroup()%>%
  group_by(Gewässer, MST_NR, Probestelle, YEAR, Taxon) %>%
  summarise(Messwert = mean(Messwert)) %>% #get rid of double values for DATUM
  select(Gewässer, MST_NR, YEAR, Probestelle, Taxon, Messwert) %>%  #duplicated %>% which %>% #check for duplications
  spread(Taxon, Messwert)%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  select_if(~sum(!is.na(.)) > 0)

## Fill up dataset with all possible plots to have also zero values ----
Makroph_comm_S <-  right_join(Makroph_comm_S2,
                              Makroph_dataset, by=c("Gewässer", "MST_NR", "YEAR", "Probestelle"))
Makroph_comm_S$Tiefe <- plyr::revalue(Makroph_comm_S$Probestelle, c("0-1"="-0.5", "1-2"="-1.5", "2-4"="-3","4-x"="-5"))
Makroph_comm_S<-Makroph_comm_S%>%
  mutate(Tiefe=as.numeric(Tiefe))%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
  rename(Lake=Gewässer, Depth=Tiefe)

## Calculate Gamma richness per depth
Makroph_commLastYear <- Makroph_comm_S %>%
  group_by(Lake, YEAR,Depth) %>%
  summarise_at(vars(-"MST_NR", -"Probestelle"), mean, na.rm=T)%>%
  ungroup() %>%
  group_by(Lake) %>%
  #filter(YEAR==max(YEAR))%>%
  select(where(~ any(. != 0)))

# fwrite(Makroph_commLastYear, "Makroph_commLastYear.csv")
# fwrite(Makroph_comm_S, "Makroph_comm_S.csv")

usethis::use_data(Makroph_comm_S, overwrite = TRUE)
usethis::use_data(Makroph_commLastYear, overwrite = TRUE)

