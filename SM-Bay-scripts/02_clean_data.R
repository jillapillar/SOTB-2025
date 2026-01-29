# SOTB Report 2025
# Step 2: Clean data


#fish ----
Seine_Data_clean <- SOP_9_fish_data %>% 
  mutate(date = ymd(samplecollectiondate)) %>% 
  #just keep the date, no time
  mutate(date = date(date)) %>% 
  #select important columns 
  select(siteid, estuaryname, date, method,
         stationno, netreplicate, scientificname, commonname,
         abundance, region:estuarytype, status, surveytype) %>% 
  ## add season 
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(8, 9, 10, 11, 12) ~ "Fall",
      month %in% c(3, 4, 5, 6, 7) ~ "Spring",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-month)

## add lookup list for fish or invert 
fish_lookup <- fish_lookup %>% 
  select(scientificname, fish_or_invert)

Seine_df <- Seine_Data_clean %>% 
  left_join(.,fish_lookup) %>% 
  #add 'zero' fish or invert type to note when species = Not recorded
  mutate(fish_or_invert = 
           ifelse(fish_or_invert == '', "zero", fish_or_invert)) %>% 
  #select only Bight data
  filter(year %in% c("2023", "2024")) %>% 
  filter(siteid %in% c("SC-MAL", "SC-TOP", "SC-ZUM", "SC-ASE", "SC-BSC"))


#add Arroyo Sequit Fall
new_row <- data.frame(
  siteid = "SC-ASE",
  estuaryname = "Arroyo Sequit",
  estuarytype = "Temporarily Closed",
  year = 2024,
  season = "Fall")

Site_df <- Seine_df %>% 
  select(siteid, estuaryname, estuarytype, year, season) %>% 
  unique() %>% 
  filter(year == 2024) %>% 
  #add a row for arroyo sequit
  rbind(., new_row) %>% 
  mutate(star = if_else(estuarytype == "Temporarily Closed", "*", "")) %>% 
 # mutate(siteid_star = paste0(siteid, star)) %>% 
  select(siteid, estuaryname, estuarytype, year, season)


#fish length ----

Length_Data_clean <- SOP_9_fish_length_data %>% 
  mutate(date = ymd(samplecollectiondate)) %>% 
  #just keep the date, no time
  mutate(date = date(date)) %>% 
  #select important columns 
  select(siteid, estuaryname, date,
         stationno, netreplicate, scientificname, commonname, length_mm, replicate,
         region:estuarytype, status, surveytype) %>% 
  ## add season 
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(8, 9, 10, 11, 12) ~ "Fall",
      month %in% c(3, 4, 5, 6, 7) ~ "Spring",
      TRUE ~ NA_character_
    )
  ) %>%
  #remove lengths that are -88
  filter(length_mm != -88) %>% 
  select(-month) %>% 
  left_join(.,fish_lookup) %>% 
  #add 'zero' fish or invert type to note when species = Not recorded
  mutate(fish_or_invert = 
           ifelse(fish_or_invert == '', "zero", fish_or_invert)) %>% 
  #select only SM BAY data
  filter(year %in% c("2023", "2024")) %>% 
  filter(siteid %in% c("SC-GOL", "SC-VEN", "SC-MAL", "SC-NEW", "SC-BAT",
                       "SC-TOP", "SC-ZUM", "SC-ASE", "SC-BSC"))

#create a dataframe that is just Presence/Absence data
PA_Fish <- Seine_df %>%
  mutate(presence_absence = ifelse(abundance != 0, 1, 0))

## filter fish/invert and clean
PA_filter <- PA_Fish %>% 
  #add star so can flag in graphs where sites are open
  mutate(star = if_else(estuarytype == "Temporarily Closed", "*", "")) %>% 
  #mutate(siteid_star = paste0(siteid, star)) %>% 
  #select only fish
  #by keeping zero - keeps in the sites with no fish
  filter(fish_or_invert == "fish" |
           fish_or_invert == "zero") %>% 
  #filter(commonname != "Not recorded") %>%
  #remove unknown fish
  filter(!grepl("unknown", commonname, ignore.case = TRUE)) %>% 
  #remove juvenile fish
  filter(!grepl("juvenile", commonname, ignore.case = TRUE)) %>% 
  filter(year == "2024")

## find order of PA from most to least for ordering of heat map
SpeciesOrder <- PA_filter %>%
  #filter(presence_absence == 1) %>% 
  select(commonname, siteid) %>% 
  distinct() %>% 
  group_by(commonname) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))

## get the ordered list of species by their count
SpeciesList <- SpeciesOrder %>% pull(commonname)

## add order of sites (north to south)
site_order <- c("SC-BSC", "SC-ASE", "SC-ZUM","SC-TOP", "SC-MAL")

## summarize df and create color category for PA heat map. will add new color if present in both years 
PA_summarized <- PA_filter %>%
  group_by(siteid, commonname) %>%
  summarise(color_category = case_when(
    sum(presence_absence == 1 & season == "Fall") > 0 & 
      sum(presence_absence == 1 & season == "Spring") > 0 ~ "Both",
    sum(presence_absence == 1 & season == "Spring") > 0 ~ "Spring",
    sum(presence_absence == 1 & season == "Fall") > 0 ~ "Fall",
    TRUE ~ "None"
  )) %>%
  ungroup() #%>% 
#intentionally add 0s
#group_by(siteid, siteid_star) %>% 
# complete(siteid_star, siteid, commonname, fill = list(color_category = NA)) %>% 
# filter(commonname != "Not recorded")


## order based on site order
PA_filter_order <-PA_summarized %>% 
  mutate(siteid =  factor(siteid, levels = site_order)) %>%
  arrange(siteid)  %>% 
  #order based on species count
  mutate(commonname =  factor(commonname, levels = SpeciesList)) %>%
  arrange(commonname)  

## define colors for color category
colors <- c("Spring" = "#DDCC77", "Fall" = "#88CCEE", "Both" = "#117733",
            "None" = "white")

##GGPLOT TIME 
#adjust data because we don't want Not recorded species
fish_filtered_data <- PA_filter_order %>%
  filter(commonname != "Not recorded") %>%
  mutate(siteid = factor(siteid, levels = unique(PA_filter_order$siteid)))

n_species <- length(unique(fish_filtered_data$commonname))



#inverts ----

#total sotb
sotb_invert_data_clean <- sotb_invert_data %>%
  mutate(date = mdy(samplecollectiondate)) %>%
  mutate(year = year(date),
         month = month(date),
         season = case_when(
           month %in% c(8, 9, 10, 11, 12) ~ "Fall",
           month %in% c(3, 4, 5, 6) ~ "Spring",
           TRUE ~ NA_character_
         )
  ) %>%
  select(-month) %>% 
  select(siteid, estuaryname, stationno, samplelocation, scientificname, abundance, year, season) %>%
  #select only SM BAY data
  filter(year %in% c(2023, 2024)) %>% 
  filter(siteid %in% c("SC-MAL", "SC-TOP", "SC-ZUM", "SC-ASE", "SC-BSC"))


#other invert data

macro_invert_data_clean <- macro_invert_data %>% 
  mutate(date = ymd(samplecollectiondate)) %>% 
  #just keep the date, no time
  mutate(date = date(date)) %>% 
  #select important columns 
  select(siteid, estuaryname, date, stationno, samplelocation, scientificname, live_dead,
         shell_type, unknown_replicate, abundance, region:estuarytype) %>% 
  ## add season 
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(8, 9, 10, 11, 12) ~ "Fall",
      month %in% c(3, 4, 5, 6) ~ "Spring",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-month) %>% 
  #select only SM BAY data
  filter(year %in% c("2023", "2024")) %>% 
  filter(siteid %in% c("SC-MAL", "SC-TOP", "SC-ZUM", "SC-ASE", "SC-BSC"))

epifauna_data_clean <- epifauna_data %>% 
  mutate(date = ymd(samplecollectiondate)) %>% 
  #just keep the date, no time
  mutate(date = date(date)) %>% 
  #select important columns 
  select(siteid, estuaryname, date, stationno, transectreplicate, plotreplicate,
         burrows, scientificname, commonname, status, 
         enteredabundance, quadratsize, estimatedabundance, region:estuarytype) %>% 
  ## add season 
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(8, 9, 10, 11, 12) ~ "Fall",
      month %in% c(3, 4, 5, 6) ~ "Spring",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-month) %>% 
  #select only SM BAY data
  filter(year %in% c("2023", "2024")) %>% 
  filter(siteid %in% c("SC-MAL", "SC-TOP", "SC-ZUM", "SC-ASE", "SC-BSC"))

#join the data together for PA
PA_macro_inverts <- macro_invert_data_clean %>%
  #add star so can flag in graphs where sites are open
  mutate(star = if_else(estuarytype == "Temporarily Closed", "*", "")) %>% 
  #mutate(siteid_star = paste0(siteid, star)) %>% 
  filter(year == 2024) %>% 
  #remove Not recorded
  filter(scientificname != "Not recorded") %>% 
  #filter out unknowns
  filter(!grepl("unknown", scientificname, ignore.case = TRUE)) %>% 
  mutate(presence_absence = ifelse(abundance != 0, 1, 0)) %>% 
  #add column that identifies the data type
  mutate(data_type = "large_inverts")

PA_epifauna <- epifauna_data_clean %>%
  #add star so can flag in graphs where sites are open
  mutate(star = if_else(estuarytype == "Temporarily Closed", "*", "")) %>% 
 # mutate(siteid_star = paste0(siteid, star)) %>% 
  filter(year == 2024) %>% 
  #remove Not recorded
  filter(scientificname != "Not recorded") %>% 
  #filter out unknowns
  filter(!grepl("unknown", scientificname, ignore.case = TRUE)) %>% 
  mutate(presence_absence = ifelse(enteredabundance != 0, 1, 0)) %>% 
  #add column that identifies the data type
  mutate(data_type = "epifauna")

#we are going to rbind the two datasets

PA_all_invert <- PA_macro_inverts %>% 
  select(siteid, estuaryname, date, stationno, scientificname,
         presence_absence, year, season, estuarytype, data_type) %>% 
  #we are getting rid of live and dead and location, therefore get rid of dups
  unique() %>% 
  rbind(., PA_epifauna %>% 
          select(siteid, estuaryname, date, stationno, scientificname,
                 presence_absence, year, season, estuarytype, data_type) %>% 
          unique())

## find order of PA from most to least for ordering of heat map for infauna
SpeciesOrder_invert <- PA_all_invert %>%
  filter(presence_absence == 1) %>% 
  select(scientificname, siteid) %>% 
  distinct() %>% 
  group_by(scientificname) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))

## get the ordered list of species by their count
SpeciesList_invert <- SpeciesOrder_invert %>% pull(scientificname)

## summarize df and create color category for PA heat map. will add new color if present in both years 
PA_summarized_invert <- PA_all_invert %>%
  group_by(siteid, scientificname) %>%
  summarise(color_category = case_when(
    sum(presence_absence == 1 & season == "Fall") > 0 & 
      sum(presence_absence == 1 & season == "Spring") > 0 ~ "Both",
    sum(presence_absence == 1 & season == "Spring") > 0 ~ "Spring",
    sum(presence_absence == 1 & season == "Fall") > 0 ~ "Fall",
    TRUE ~ "None"
  )) %>%
  ungroup() #

## order based on site order
PA_filter_order_invert <-PA_summarized_invert %>% 
 # mutate(siteid_star =  factor(siteid_star, levels = site_order)) %>%
  arrange(siteid)  %>% 
  #order based on species count
  mutate(scientificname =  factor(scientificname, levels = SpeciesList_invert)) %>%
  arrange(scientificname)  



#temperature + DO ----
head(temp_do)

#birds ----
head(bird_data)
bird_count_clean <- bird_data %>% 
  mutate(date = mdy(samplecollectiondate)) %>% 
  #just keep the date, no time
  mutate(date = date(date)) %>% 
  ## add season 
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(8, 9, 10, 11, 12) ~ "Fall",
      month %in% c(3, 4, 5, 6) ~ "Spring",
      TRUE ~ NA_character_
    )
  ) %>% 
  #add other site info
  left_join(., Site_df %>% 
              select(siteid, estuaryname, estuarytype) %>% 
              unique()) %>% 
  #change abundnace NA to 0
  mutate(abundance = replace_na(abundance, 0))

#add full bird names to abbreviations

bird_count_df <- bird_count_clean %>% 
  left_join(., bird_list %>% 
              select(birds = Abbreviation, commonname = Name))


#CRAM ----

str(CRAM_data)

CRAM_data_clean <- CRAM_data %>% 
  select(aaname, visitdate, county, ecoregion, indexscore:biotic_structure) %>% 
  filter(ecoregion == "south coast" |
           county == "Santa Barbara") %>% 
  #crosswalk names
  mutate(estuaryname = case_when(
    str_detect(aaname, "Big Sycamore") ~ "Big Sycamore Canyon",
    str_detect(aaname, "Zuma") ~ "Zuma Lagoon",
    str_detect(aaname, "Topanga") ~ "Topanga Lagoon",
    str_detect(aaname, "Batiquitos") ~ "Batiquitos Lagoon",
    str_detect(aaname, "SC-VEN") ~ "Ventura River",
    str_detect(aaname, "Ventura") ~ "Ventura River",
    str_detect(aaname, "SC-GOL") ~ "Goleta Slough",
    str_detect(aaname, "Goleta Slough") ~ "Goleta Slough",
    str_detect(aaname, "SC-MAL") ~ "Malibu Lagoon",
    str_detect(aaname, "Malibu") ~ "Malibu Lagoon",
    str_detect(aaname, "NWCA21-CA-10156-San Diego Bay") ~ "Sweetwater Marsh",
    str_detect(aaname, "Newport") ~ "Newport Bay",
    str_detect(aaname, "Mugu") ~ "Point Mugu",
    str_detect(aaname, "Tijuana") ~ "Tijuana River Estuary",
    str_detect(aaname, "Los Pen") ~ "Los Penasquitos Lagoon",
    str_detect(aaname, "Salt Pond") ~ "San Diego Bay: South salt ponds",
    str_detect(aaname, "Mission Bay") ~ "Mission Bay: Kendall-Frost Reserve",
    str_detect(aaname, "San Dieguito") ~ "San Dieguito Lagoon",
    str_detect(aaname, "San Elijo") ~ "San Elijo Lagoon",
    str_detect(aaname, "Agua Hedionda") ~ "Agua Hedionda",
    str_detect(aaname, "Talbert") ~ "Huntington Beach Wetlands",
    str_detect(aaname, "Brookhurst") ~ "Huntington Beach Wetlands",
    str_detect(aaname, "Magnolia") ~ "Huntington Beach Wetlands",
    str_detect(aaname, "Seal") ~ "Seal Beach",
    str_detect(aaname, "Arroyo Sequit") ~ "Arroyo Sequit",
    str_detect(aaname, "Carpenteria Salt Marsh") ~ "Carpinteria Estuary",
    str_detect(aaname, "Carpenteria Marsh") ~ "Carpinteria Estuary",
    str_detect(aaname, "Upper Devereux") ~ "Devereaux Slough",
    #str_detect(aaname, "Bolsa Chica") ~ "Bolsa Chica", #the CRAM area is not the same as the site, so ignore for now
    TRUE ~ "Not recorded"
  )) %>% 
  left_join(., Site_df %>% 
              select(siteid, estuaryname)) %>% 
  #only select estaruies we want
  filter(!is.na(siteid))


#vegetation ----
Vegcover_df <- SOP_11_vegcover_data %>% 
  mutate(date = ymd(samplecollectiondate)) %>% 
  #just keep the date, no time
  mutate(date = date(date)) %>% 
  #select important columns 
  select(siteid, estuaryname, stationno, date, method,
         habitat, transectreplicate, plotreplicate, covertype, live_dead,
         scientificname, commonname, status, estimatedcover, tallestplantheight_cm,
         region:estuarytype) %>% 
  # add season
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(8, 9, 10, 11, 12) ~ "Fall",
      month %in% c(3, 4, 5, 6) ~ "Spring",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-month) %>% 
  #select only SM BAY data
  filter(year %in% c("2023", "2024")) %>% 
  filter(siteid %in% c("SC-MAL", "SC-TOP", "SC-ZUM", "SC-ASE", "SC-BSC"))



####### veg sample data

Vegsample_Data_clean <- SOP_11_vegsample_data %>% 
  mutate(date = ymd(samplecollectiondate)) %>% 
  #just keep the date, no time
  mutate(date = date(date)) %>% 
  #select important columns 
  select(siteid, estuaryname, stationno, date, method,
         habitat, transectreplicate, plotreplicate, vegetated_cover,
         non_vegetated_cover,
         region:estuarytype) %>% 
  ## add season 
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(8, 9, 10, 11, 12) ~ "Fall",
      month %in% c(3, 4, 5, 6) ~ "Spring",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-month) %>% 
  #select only SM BAY data
  filter(year %in% c("2023", "2024")) %>% 
  filter(siteid %in% c("SC-MAL", "SC-TOP", "SC-ZUM", "SC-ASE", "SC-BSC"))

## presence absence vegetation

PA_veg <- Vegcover_df %>%
  #add star so can flag in graphs where sites are open
  mutate(star = if_else(estuarytype == "Temporarily Closed", "*", "")) %>% 
  #mutate(siteid_star = paste0(siteid, star)) %>% 
  filter(year == 2024) %>% 
  #remove Not recorded
  filter(scientificname != "Not recorded") %>% 
  #keep only vegetation cover class
  filter(covertype == "vegetation") %>% 
  #filter out unknowns
  filter(!grepl("unknown", commonname, ignore.case = TRUE)) %>% 
  mutate(presence_absence = ifelse(estimatedcover != 0, 1, 0)) %>% 
  #only filter SM Bay sites bc other sites sampled Fall only, and only in marsh
  filter(siteid %in% c("SC-MAL", "SC-TOP", "SC-ZUM", "SC-ASE", "SC-BSC"))

## find order of PA from most to least for ordering of heat map
SpeciesOrder_veg <- PA_veg %>%
  filter(presence_absence == 1) %>% 
  select(scientificname, siteid) %>% 
  distinct() %>% 
  group_by(scientificname) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))

## get the ordered list of species by their count
SpeciesList_veg <- SpeciesOrder_veg %>% pull(scientificname)

## summarize df and create color category for PA heat map. will add new color if present in both years 
PA_summarized_veg <- PA_veg %>%
  group_by(siteid, scientificname) %>%
  summarise(color_category = case_when(
    sum(presence_absence == 1 & season == "Fall") > 0 & 
      sum(presence_absence == 1 & season == "Spring") > 0 ~ "Both",
    sum(presence_absence == 1 & season == "Spring") > 0 ~ "Spring",
    sum(presence_absence == 1 & season == "Fall") > 0 ~ "Fall",
    TRUE ~ "None"
  )) %>%
  ungroup() #

## order based on site order
PA_filter_order_veg <-PA_summarized_veg %>% 
  #mutate(siteid_star =  factor(siteid, levels = site_order)) %>%
  arrange(siteid)  %>% 
  #order based on species count
  mutate(scientificname =  factor(scientificname, levels = SpeciesList_veg)) %>%
  arrange(scientificname)  


#macroalgae ----
#Floating macroalgae
Macro_Floating_df <- SOP_7_macro_data %>% 
  mutate(date = ymd(samplecollectiondate)) %>% 
  #just keep the date, no time
  mutate(date = date(date)) %>% 
  #select important columns 
  select(siteid, estuaryname, date, stationno, surveyarea, estimatedcover,
         stationno, scientificname, commonname,
         status, region:estuarytype) %>% 
  #remove -88
  filter(estimatedcover != -88) %>% 
  ## add season 
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(8, 9, 10, 11, 12) ~ "Fall",
      month %in% c(3, 4, 5, 6) ~ "Spring",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-month) %>% 
  #select only SM BAY data
  filter(year %in% c("2023", "2024")) %>% 
  filter(siteid %in% c( "SC-MAL", "SC-TOP", "SC-ZUM", "SC-ASE", "SC-BSC"))

#intertidal macroalgae
Macro_Intertidal_df <- SOP_7_intertidal_data %>% 
  mutate(date = ymd(samplecollectiondate)) %>% 
  #just keep the date, no time
  mutate(date = date(date)) %>% 
  #select important columns 
  select(siteid, estuaryname, date, stationno, transectlocation,
         transectreplicate, plotreplicate, non_algae_cover, total_algae_cover,
         region:estuarytype)  %>%
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(8, 9, 10, 11, 12) ~ "Fall",
      month %in% c(3, 4, 5, 6) ~ "Spring",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-month) %>% 
  #select only SM BAY data
  filter(year %in% c("2023", "2024")) %>% 
  filter(siteid %in% c("SC-MAL", "SC-TOP", "SC-ASE", "SC-BSC"))


#SAV ----

#sediment ----

#nuts ----

#nutrients

