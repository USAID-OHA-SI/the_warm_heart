# PROJECT: Analysis of Malawi FY23 data
# PURPOSE: Munge and Analysis of FY23 MSD into MDB tables
# AUTHOR:  Tim Esssam | SI
# REF ID:  be3ff80d
# LICENSE: MIT
# DATE:   2023-11-26
# NOTES:  Demo project for Malawi Mission   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(cascade)
    library(selfdestructin5)
    library(scales)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library(gtExtras)

    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "PSNU_IM.*Malawi")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "be3ff80d"
    
  # Source custom functions
    source("Scripts/00-helpers_setup.R")
    
  # Functions  
  # Run legend_chunk only once
    remove(legend_chunk)
    legend_chunk <- gt::md(glue::glue("<img src= '{legend}' style='height:20px;'> "))
    cntry  <- "Malawi"

    
# LOAD DATA ==============================================================================  

  df_msd <- read_psd(file_path)
    
  mech_list <- df_msd %>% 
    filter(funding_agency == "USAID", fiscal_year == metadata$curr_fy)  %>% 
    distinct(mech_code, mech_name, prime_partner_name) %>% 
    pull(mech_code)
  

# ACHV Tables ============================================================================
  
  # Step 1. Create the summary table summary data frame
  # make_mdb_df creates the data frame, reshape_mdb_df reshapes to a gt ready format
    mdb_df   <- make_mdb_df(df_msd)
    mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd) 
    
  # Step 2. Create the treatment data frame needed for derived indicators
    mdb_df_tx    <- make_mdb_tx_df(df_msd)
    mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd) 
    
  # Step 3. Create the overall summary table  
    mdb_tbl %>% 
      #filter(agency == "USAID") %>% 
      create_mdb(ou = cntry, type = "main", metadata$curr_pd, metadata$source, legend = legend_chunk) %>%
      shrink_rows() %>% 
      gtsave_extra(path = "Images", filename = glue::glue("{metadata$curr_pd}_{cntry}_mdb_summary.png"))  
    
  # Step 4. Create the treatment overall summary table  
    mdb_tbl_tx %>% 
      #filter(agency == "USAID") %>% 
    create_mdb(ou = cntry, type = "treatment", metadata$curr_pd, metadata$source) %>% 
      bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
      embiggen() %>% 
      tab_options(
        data_row.padding = px(1),
        row_group.padding = px(2),
        heading.padding = px(1)
      ) %>% 
      gtsave_extra(., path = "Images", filename = glue::glue("{metadata$curr_pd}_{cntry}_MMD_VL_MD.png")) 
    
    
  # Step 5. Create USAID Only tables
    mdb_tbl %>% 
      filter(agency == "USAID") %>% 
      create_mdb(ou = cntry, type = "main", metadata$curr_pd, metadata$source, legend = legend_chunk) %>%
      shrink_rows() %>% 
      gtsave_extra(path = "Images", 
                   filename = glue::glue("{metadata$curr_pd}_{cntry}_USAID_mdb_summary.png")) 
    
    mdb_tbl_tx %>% 
      filter(agency == "USAID") %>% 
      create_mdb(ou = cntry, type = "treatment", metadata$curr_pd, metadata$source) %>% 
      bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
      embiggen() %>% 
      tab_options(
        data_row.padding = px(1),
        row_group.padding = px(2),
        heading.padding = px(1)
      ) %>% 
      gtsave_extra(., path = "Images", 
                   filename = glue::glue("{metadata$curr_pd}_{cntry}_USAID_MMD_VL_MD.png")) 
    


# PEPFAR ONLY VERSION OF THE TABLE ----------------------------------------

    mdb_df_pf   <- make_mdb_df(df_msd %>% mutate(funding_agency = "USAID"))
    mdb_tbl_pf  <- reshape_mdb_df(mdb_df_pf, metadata$curr_pd) %>% mutate(agency = "PEPFAR")
    
    mdb_df_tx_pf    <- make_mdb_tx_df(df_msd %>% mutate(funding_agency = "USAID"))
    mdb_tbl_tx_pf   <- reshape_mdb_tx_df(mdb_df_tx_pf, metadata$curr_pd) %>% mutate(agency = "PEPFAR")
    
    mdb_tbl_pf %>% 
      create_mdb(ou = cntry, type = "main", metadata$curr_pd, metadata$source, legend = legend_chunk) %>%
      shrink_rows() %>% 
      gtsave_extra(path = "Images", 
                   filename = glue::glue("{metadata$curr_pd}_{cntry}_PEPFAR_mdb_summary.png")) 
    
    mdb_tbl_tx_pf %>% 
      create_mdb(ou = cntry, type = "treatment", metadata$curr_pd, metadata$source) %>% 
      bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
      embiggen() %>% 
      tab_options(
        data_row.padding = px(1),
        row_group.padding = px(2),
        heading.padding = px(1)
      ) %>% 
      gtsave_extra(., path = "Images", 
                   filename = glue::glue("{metadata$curr_pd}_{cntry}_PEPFAR_MMD_VL_MD.png")) 
      
  
# Partner Tables ============================================================================

  # Check the USAID mech list
  # Mech tables require metadata object and legend_chunk objects be created before hand
  mech_list
  map(mech_list, ~mk_ptr_tbl(df_msd, .x, cntry = cntry))  
    

# Cascade plots ============================================================================
  
  # Create a directory for the cascade plots
  dir.create("Images/Cascade")
  dir.create("Images/Cascade/PEPFAR")
  dir.create("Images/Cascade/USAID")
  
  # Filter out extra TX_CURR indicators / they create errors
  df_msd <- df_msd %>%  filter(str_detect(indicator, "TX_CURR_Lag", negate = T))
  
  # Test to ensure the correct cascade df is returned
  df_msd %>% return_cascade(., 1) %>% prinf()
  
  # Test a plot (interactively)
  return_cascade_plot(df_msd, export = F)
  
  # Create PEPFAR cascade
  batch_cascade_plot(df_msd, 
                     imgpath = "Images/Cascade/PEPFAR", imgtype = ".png")
  
  # Create USAID specific ones
  batch_cascade_plot(df_msd %>% filter(funding_agency == "USAID"), 
                     imgpath = "Images/Cascade/USAID", imgtype = ".png")
    

