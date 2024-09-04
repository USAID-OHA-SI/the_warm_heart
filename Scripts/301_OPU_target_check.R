# PROJECT:  C:/Users/tessam/Documents/Github/the_warm_heart
# PURPOSE:  Target Table Review
# AUTHOR:   T. Essam | USAID
# REF ID:   da48b744 
# LICENSE:  MIT
# DATE:     2024-09-04
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
#general
library(tidyverse)
library(glue)
#oha
library(gagglr) 
#viz extensions
library(scales, warn.conflicts = FALSE)
library(systemfonts)
library(tidytext)
library(patchwork)
library(ggtext)
library(selfdestructin5)
library(gt)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "da48b744"  #a reference to be places in viz captions 
  
  path_msd <-  si_path() %>% return_latest("PSNU_IM.*Malawi")
  
  get_metadata(path_msd)  #extract MSD metadata
  
  load_secrets()
  
# IMPORT ------------------------------------------------------------------
  
  df_msd <- read_psd(path_msd)
  
  
  

# MUNGE -------------------------------------------------------------------

  # Per OPU: Mechanism 84562, HTS_RECENT targets were allocated in Balaka District
  # (overall target remains the same, it was just the allocation missing).
  
  df_msd %>% 
    filter(mech_code == "84562", indicator == "HTS_RECENT", fiscal_year == 2025) %>% 
    count(mech_code, psnu, indicator, standardizeddisaggregate, wt = targets) %>% 
    rename(targets = n)

  
  #In Neno District, CXCA targets were removed entirely. This affects
  # mechanism 85172 and also the overall CXCA target.
  df_msd %>% 
    filter(mech_code == "85172", str_detect(indicator, "CXCA"), fiscal_year == 2025) %>% 
    count(mech_code, indicator, standardizeddisaggregate, psnu, wt = targets) %>% 
    arrange(psnu) %>% 
    rename(targets = n)
  
  df_msd %>% 
    filter(mech_code %in% c("87477", "87478"), fiscal_year == 2025) %>% 
    count(mech_code, psnu, indicator, standardizeddisaggregate, wt = targets) %>% 
    rename(targets = n) %>% prinf()
  
# Summary Tables ---------------------------------------------------------------------

  set_metadata(gophr::get_metadata(path_msd))

  # Main Table
  # Create the long mdb_df of the main summary indicators 
  # This will remove mechs with known issues by default. If you want to keep all mechs set `resolve_issues == FALSE`
  summary_df   <- make_mdb_df(df_msd)
  
  # Create the reshaped df that is gt() ready
  summary_tbl  <- reshape_mdb_df(summary_df)
  
  # Try a specific country now
  create_mdb(summary_tbl, ou = "Malawi", type = "main") %>% 
    gtExtras::gtsave_extra("Images/MWI_mdb_main_FY24Q3.pdf")

  
  i# Create the treatment data frame needed for derived indicators
  summary_df_tx    <- make_mdb_tx_df(df_msd)
  summary_tbl_tx   <- reshape_mdb_tx_df(summary_df_tx)
  
  create_mdb(summary_tbl_tx, ou = "Malawi", type = "treatment") %>% 
    gtsave("Images/MWI_mdb_treatment_FY24Q3.png")
