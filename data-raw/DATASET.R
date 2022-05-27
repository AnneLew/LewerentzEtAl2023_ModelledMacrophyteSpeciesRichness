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



## Species groups
GroupsSpecies <- fread("data-raw/Indexmakrophyten_Gruppenzuordnung.csv")

GroupsSpecies$Taxon <- gsub(" ", ".", GroupsSpecies$Taxon)
usethis::use_data(GroupsSpecies, overwrite = TRUE)


# Lake morphology
load("data-raw/Morphology.rda")
usethis::use_data(Morphology, overwrite = TRUE)



## Scenario data

scenariofolder1<-"/data-raw/scenarios_V4_first50each/"
scenariofolder2<-"/data-raw/scenarios_V4_Rest/"

filenames1 <- list.files(path=paste0(here::here(),scenariofolder1), pattern="*.txt", full.names=TRUE)
filenames2 <- list.files(path=paste0(here::here(),scenariofolder2), pattern="*.txt", full.names=TRUE)
filenames<-cbind(filenames1,filenames2)

scenario_data <- read.table(filenames[1], header = T)

for (f in 2:length(filenames)){
  scenario_data <- rbind(scenario_data, read.table(filenames[f], header = T))
}


scenario_data <- scenario_data %>% filter(lakeID!=11)
usethis::use_data(scenario_data, overwrite = TRUE)


## Preprocess scenario data
all_diff_tobase <- scenario_data %>%
  group_by(speciesID,lakeID,variable)%>%
  unique() %>% # ich weiß nicht warum
  spread(scenario, value) %>%
  mutate(
    S0_m1minusBase = S0_m1 - base,
    S0_1minusBase = S0_1 - base,
    S0_2minusBase = S0_2 - base,
    S1_m1minusBase = S1_m1 - base,
    S1_0minusBase = S1_0 - base,
    S1_1minusBase = S1_1 - base,
    S1_2minusBase = S1_2 - base,
    S2_m1minusBase = S2_m1 - base,
    S2_0minusBase = S2_0 - base,
    S2_1minusBase = S2_1 - base,
    S2_2minusBase = S2_2 - base
  )
usethis::use_data(all_diff_tobase, overwrite = TRUE)


all_diff_presabs_tobase <- scenario_data %>%
  mutate_at(vars(value), ~ 1 * (. != 0)) %>%
  group_by(speciesID,lakeID,variable)%>%
  unique() %>% # ich weiß nicht warum
  spread(scenario, value) %>%
  mutate(
    S0_m1minusBase = S0_m1 - base,
    S0_1minusBase = S0_1 - base,
    S0_2minusBase = S0_2 - base,
    S1_m1minusBase = S1_m1 - base,
    S1_0minusBase = S1_0 - base,
    S1_1minusBase = S1_1 - base,
    S1_2minusBase = S1_2 - base,
    S2_m1minusBase = S2_m1 - base,
    S2_0minusBase = S2_0 - base,
    S2_1minusBase = S2_1 - base,
    S2_2minusBase = S2_2 - base
  )
usethis::use_data(all_diff_presabs_tobase, overwrite = TRUE)


all_diff_presabs_TempEffect <- scenario_data %>%
  mutate_at(vars(value), ~ 1 * (. != 0)) %>%
  group_by(speciesID,lakeID,variable)%>%
  unique() %>% # ich weiß nicht warum
  spread(scenario, value) %>%
  mutate(
    S1_m1minS0_m1 = S1_m1 - S0_m1,
    S1_0minBase = S1_0 - base,
    S1_1minS0_1 = S1_1 - S0_1,
    S1_2minS0_2 = S1_2 - S0_2,
    S2_m1minS1_m1 = S2_m1 - S1_m1,
    S2_0minS1_0 = S2_0 - S1_0,
    S2_1minS1_1 = S2_1 - S1_1,
    S2_2minS1_2 = S2_2 - S1_2
  ) %>%
  select(speciesID,lakeID,variable,S1_0minBase,S1_m1minS0_m1,S1_1minS0_1,S1_2minS0_2,
         S2_m1minS1_m1,S2_0minS1_0,S2_1minS1_1,S2_2minS1_2)
usethis::use_data(all_diff_presabs_TempEffect, overwrite = TRUE)


all_diff_prsabs_TurbEffect <- scenario_data %>%
  mutate_at(vars(value), ~ 1 * (. != 0)) %>%
  group_by(speciesID,lakeID,variable)%>%
  unique() %>% # ich weiß nicht warum
  spread(scenario, value) %>%
  mutate(
    S0_m1minBase = S0_m1 - base,
    S0_1minBase = S0_1 - base,
    S0_2minBase = S0_2 - base,
    S1_m1minS1_0 = S1_m1 - S1_0,
    S1_1minS1_0 = S1_1 - S1_0,
    S1_2minS1_0 = S1_2 - S1_0,
    S2_m1minS2_0 = S2_m1 - S2_0,
    S2_1minS2_0 = S2_1 - S2_0,
    S2_2minS2_0 = S2_2 - S2_0
  )
usethis::use_data(all_diff_prsabs_TurbEffect, overwrite = TRUE)
# Process Nutrient / Turbidity scenarios
scenTurbNutr_diff_presabs_toZero <- scenario_data %>%
  mutate_at(vars(value), ~ 1 * (. != 0)) %>%
  group_by(speciesID,lakeID,variable)%>%
  unique() %>% # ich weiß nicht warum
  spread(scenario, value) %>%
  mutate(
    S2_m1minusS2_0 = S2_m1 - S2_0,
    S2_N0_Tm1minusS2_0 = S2_N0_Tm1 - S2_0,
    S2_N1_Tm1minusS2_0 = S2_N1_Tm1 - S2_0,
    S2_Nm1_T0minusS2_0 = S2_Nm1_T0 - S2_0,
    S2_N1_T0minusS2_0 = S2_N1_T0 - S2_0,
    S2_Nm1_T1minusS2_0 = S2_Nm1_T1 - S2_0,
    S2_N0_T1minusS2_0 = S2_N0_T1 - S2_0,
    S2_1minusS2_0 = S2_1 - S2_0
  )
usethis::use_data(scenTurbNutr_diff_presabs_toZero, overwrite = TRUE)


#### WHATEVER
SPlength <- length(Makroph_commLastYear)-3
Makroph_commLastYear$GAMMA <- vegan::specnumber(Makroph_commLastYear[,c(4:85)])

Makroph_commLastYearfin<-Makroph_commLastYear %>% # %>%
  filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                      "Grosser Brombachsee",
                      "Gruentensee","Igelsbachsee",
                      "Kleiner Brombachsee",
                      "Liebensteinspeicher","Rottachsee",
                      "Steinberger See","Untreusee",
                      "Walchensee","Murnersee","Rothsee",
                      "Seehamer See"))%>%
  select(where(~ any(. != 0)))

MAK_MAPPED_NSPEC <- Makroph_commLastYearfin %>%
  select(-Lake, -YEAR, -Depth, -GAMMA) %>% ncol()

MAK_MAPPED<-Makroph_commLastYearfin %>%
  #rename(Lake=Gewässer, Depth=Tiefe) %>% #%>%  spread(Depth, GAMMA)
  select(Lake, YEAR, Depth, GAMMA) %>%
  mutate(GAMMAperc = GAMMA/MAK_MAPPED_NSPEC*100)

MAK_MAPPED$LakeID <- MAK_MAPPED  %>% group_indices(Lake)

MAK_MAPPED <- MAK_MAPPED %>% mutate(LakeID=paste0("lake_",LakeID))

fulllakenames <- MAK_MAPPED %>% select(Lake,LakeID) %>% unique() %>%
  filter(LakeID!="lake_11")

# MAK_MAPPED_di<-Makroph_commLastYearfin %>%
#   group_by(Lake) %>%
#   summarise_at(vars(Callitriche.cophocarpa:Zannichellia.palustris), mean, na.rm = TRUE)
#
# MAK_MAPPED_di$GAMMA <- vegan::specnumber(MAK_MAPPED_di[,c(2:72)])
#
# MAK_MAPPED_din<-MAK_MAPPED_di %>%
#   select(Lake,GAMMA)%>%
#   mutate(GAMMAperc = GAMMA/MAK_MAPPED_NSPEC*100)
#
#
# MAK_MAPPED_din$LakeID <- MAK_MAPPED_din  %>% group_indices(Lake)
#
# MAK_MAPPED_din <- MAK_MAPPED_din %>% mutate(LakeID=paste0("lake_",LakeID))


#### ###########################################################
#AB HIER LEFT JOIN GEHT NICHT!
  #############################################################

## Gamma richness per indicator group
MAK_MAPPED_NSPEC_indicationspec <- Makroph_comm_S %>%
  gather("Species", "Kohler",5:89) %>%
  left_join(GroupsSpecies, by = c("Species"="Taxon")) %>%
  dplyr::select(-Trophie,-Typ) %>%
  mutate(Gruppe = ifelse(is.na(Gruppe),"none", Gruppe)) %>%

  group_by(Lake, YEAR,Depth, Species, Gruppe) %>%
  summarise_at(vars(-"MST_NR", -"Probestelle"), mean, na.rm=T)%>% # Mean of Kohler per Lake, YEAR, Depth, Species and Group
  ungroup() %>%
  group_by(Lake) %>%
  filter(YEAR==max(YEAR))%>% #select last mapping per lake
  filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                      "Grosser Brombachsee",
                      "Gruentensee","Igelsbachsee", "Kleiner Brombachsee",
                      "Liebensteinspeicher","Rottachsee",
                      "Steinberger See",
                      "Untreusee","Walchensee","Murnersee","Rothsee",
                      "Seehamer See")) %>%
  filter(Gruppe!="none") %>% ungroup() %>%
  filter(Kohler!=0) %>%
  dplyr::select(Species) %>% unique() %>% plyr::count()


MAK_grouped <- Makroph_comm_S %>% gather("Species", "Kohler",5:89) %>%
  left_join(GroupsSpecies, by = c("Species"="Taxon")) %>% select(-Trophie,-Typ) %>%
  mutate(Gruppe = ifelse(is.na(Gruppe),"none", Gruppe)) %>%
  group_by(Lake, YEAR,Depth, Species, Gruppe) %>%
  summarise_at(vars(-"MST_NR", -"Probestelle"), mean, na.rm=T)%>% # Mean of Kohler per Lake, YEAR, Depth, Species and Group
  ungroup() %>%
  group_by(Lake) %>%
  filter(YEAR==max(YEAR))%>% #select last mapping per lake
  ungroup() %>% group_by(Lake, Depth, Gruppe) %>%
  mutate(Kohler=ifelse(Kohler>0,1,0)) %>%
  #select(where(~ any(. != 0))) %>%
  summarise(NSPEC=sum(Kohler)) %>%
  filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                      "Grosser Brombachsee",
                      "Gruentensee","Igelsbachsee", "Kleiner Brombachsee",
                      "Liebensteinspeicher","Rottachsee",
                      "Steinberger See",
                      "Untreusee","Walchensee","Murnersee","Rothsee",
                      "Seehamer See"))%>%
  rename(Group=Gruppe)%>%
  mutate(NSPECperc = NSPEC/MAK_MAPPED_NSPEC_indicationspec$n*100)

MAK_grouped$LakeID <- MAK_grouped  %>% group_indices(Lake)

MAK_mapped_grouped <- MAK_grouped %>% filter(LakeID!=11) %>%
  mutate(LakeID=paste0("lake_",LakeID))

## Depth independent
MAK_grouped_depthindep <- Makroph_comm_S %>% gather("Species", "Kohler",5:89) %>%
  left_join(GroupsSpecies, by = c("Species"="Taxon")) %>%
  select(-Trophie,-Typ) %>%
  mutate(Gruppe = ifelse(is.na(Gruppe),"none", Gruppe)) %>%
  group_by(Lake, YEAR,Species, Gruppe) %>%
  summarise_at(vars(-"MST_NR", -"Probestelle"), mean, na.rm=T)%>% # Mean of Kohler per Lake, YEAR, Depth, Species and Group
  ungroup() %>%
  group_by(Lake) %>%
  filter(YEAR==max(YEAR))%>% #select last mapping per lake
  ungroup() %>% group_by(Lake, Gruppe) %>%
  mutate(Kohler=ifelse(Kohler>0,1,0)) %>%
  #select(where(~ any(. != 0))) %>%
  summarise(NSPEC=sum(Kohler)) %>%
  filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                      "Grosser Brombachsee",
                      "Gruentensee","Igelsbachsee", "Kleiner Brombachsee",
                      "Liebensteinspeicher","Rottachsee",
                      "Steinberger See",
                      "Untreusee","Walchensee","Murnersee","Rothsee",
                      "Seehamer See"))%>%
  rename(Group=Gruppe)%>%
  mutate(NSPECperc = NSPEC/MAK_MAPPED_NSPEC_indicationspec$n*100)

MAK_grouped_depthindep$LakeID <- MAK_grouped_depthindep  %>% group_indices(Lake)

MAK_mapped_depthind_grouped <- MAK_grouped_depthindep %>%
  filter(LakeID!=11) %>%
  mutate(LakeID=paste0("lake_",LakeID))

## Depth independent and alle lakes
MAK_grouped_depthindep_alllakes <- Makroph_comm_S %>%
  gather("Species", "Kohler",5:89) %>%
  left_join(GroupsSpecies, by = c("Species"="Taxon")) %>%
  select(-Trophie,-Typ, -Depth) %>%
  mutate(Gruppe = ifelse(is.na(Gruppe),"none", Gruppe)) %>%

  filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                      "Grosser Brombachsee",
                      "Gruentensee","Igelsbachsee", "Kleiner Brombachsee",
                      "Liebensteinspeicher","Rottachsee",
                      "Steinberger See","Hofstaetter See",
                      "Untreusee","Walchensee","Murnersee","Rothsee",
                      "Seehamer See"))%>%
  rename(Group=Gruppe)%>%
  ungroup() %>%
  group_by(Lake) %>%
  filter(YEAR==max(YEAR))%>% #select last mapping per lake
  ungroup() %>%
  group_by(Species, Group) %>%
  summarise_at(vars("Kohler"), sum, na.rm=T)%>% # Mean of Kohler per Lake, YEAR, Depth, Species and Group
  ungroup()%>%
  group_by(Group) %>%
  mutate(Kohler=ifelse(Kohler>0,1,0)) %>%
  #select(where(~ any(. != 0))) %>%
  #filter(Kohler!=0)%>%
  summarise(NSPEC=sum(Kohler)) %>%
  mutate(NSPECperc = NSPEC/MAK_MAPPED_NSPEC_indicationspec$n*100)


