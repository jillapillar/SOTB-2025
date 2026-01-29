# SOTB Report - 2025
## Step 1: Load data

### Sources
# 1 - EMPA Data Portal
# 2 - CRAM data portal (saved locally, oops)
# 3 - Christine W (saved locally, oops)

# fish ----
SOP_9_fish_data <- read.csv("https://nexus.sccwrp.org/empachecker/export?tablename=tbl_fish_abundance_data")
fish_lookup <- read.csv(here("data-inventory", "lu_fishmacrospecies.csv"))

# fish length
SOP_9_fish_length_data <- read.csv("https://nexus.sccwrp.org/empachecker/export?tablename=tbl_fish_length_data")

# inverts ----
#macro-inverts
macro_invert_data <- read.csv("https://nexus.sccwrp.org/empachecker/export?tablename=tbl_benthiclarge_abundance")
#epifauna
epifauna_data <- read.csv("https://nexus.sccwrp.org/empachecker/export?tablename=tbl_epifauna_data")
#small inverts - CW
invert_small_data <- read.csv(here("data-inventory", "CW_invert_counts.csv"))

# SOTB invert totals
sotb_invert_data <- read.csv(here("data-inventory", "Prop50_infauna_totals.csv"))

# temp + do ----
temp_do <- read.csv("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/SM-Bay/data-inventory/DO_temp_SMBay.csv")

# birds ----
bird_data <-read.csv(here("data-inventory", "Bird_counts_2024.csv"))
bird_list <-read.csv(here("data-inventory", "bird_abbreviations.csv"))


# CRAM ----
CRAM_data <- read.csv(here("data-inventory", "SouthernCA_cramdata_July2023.csv"))

# vegetation ----
# Veg metatdata table
SOP_11_vegsample_data <- read.csv("https://nexus.sccwrp.org/empachecker/export?tablename=tbl_vegetation_sample_metadata")

# Veg cover table 
SOP_11_vegcover_data <- read.csv("https://nexus.sccwrp.org/empachecker/export?tablename=tbl_vegetativecover_data")

# macroalgae ----
# floating macroalgae
SOP_7_macro_data <- read.csv("https://nexus.sccwrp.org/empachecker/export?tablename=tbl_macroalgae_floating")
# intertidal macroalgae
SOP_7_intertidal_data <- read.csv("https://nexus.sccwrp.org/empachecker/export?tablename=tbl_macroalgae_transect_meta")

# SAV ----
SOP_2_WQ_SAV_data <- read.csv("https://nexus.sccwrp.org/empachecker/export?tablename=tbl_waterquality_metadata")


# Sediment ----
SOP_3_gs_data <- read.csv("https://nexus.sccwrp.org/empachecker/export?tablename=tbl_sedgrainsize_data")

# Nutrients ----
SOP3_Nutrients_data <- read.csv("https://nexus.sccwrp.org/empachecker/export?tablename=tbl_sedchem_data")
