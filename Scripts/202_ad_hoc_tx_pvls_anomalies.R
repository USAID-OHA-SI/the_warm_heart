# PROJECT: Ivestigate FY23Q3 TX_PVLS anonmaly
# PURPOSE: Munge and Analysis of MSD site level data
# AUTHOR:  Tim Esssam | SI
# REF ID:  a8a64054
# LICENSE: MIT
# DATE:   2023-11-28
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- glamr::return_latest(folderpath = merdata, pattern = "Site_IM_.*Malawi")
    shpdata <- file.path(glamr::si_path("path_vector")) # for shapefiles
    latdata <- list.files(file.path(shpdata, "OU-sites"), pattern = "facilities_locations_2023-11-29", full.names = T)
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "a8a64054"
    
  # Functions  
    calc_pct_change <- function(x){
      (x / lag(x, n = 1)) - 1
    }
    
    vlc_plot <- function(.data, font_size = 10){
  
      font_size <- font_size/.pt
      
      .data %>% 
      ggplot(aes(x = period)) +
        geom_col(aes(y = TX_CURR_lag2), 
                 fill = grey20k, width = 0.5, 
                 position = position_nudge(x = -0.1)) + 
        geom_col(aes(y = TX_PVLS_D), 
                 fill = scooter_med, width = 0.5, 
                 position = position_nudge(x = 0.1)) +
        geom_text(aes(y = TX_PVLS_D, label = percent(VLC, 1)), 
                  size = font_size,
                  position = position_nudge(x = 0.1), 
                  vjust = -0.1,
                  family = "Source Sans Pro", 
                  color = grey90k) +
        si_style_ygrid(facet_space = 0.5) +
        scale_y_continuous(labels = comma) 
    }
    
  # Notes
  # TX_PVLS_N jumped from nearly 700K to over 800K
  # repeated #s only account for about 3% of this jump

# LOAD DATA ============================================================================  


  df_site <- read_psd(file_path) %>% 
      filter(fiscal_year %in% c(2022, 2023), indicator %in% c("TX_PVLS", "TX_CURR"), 
             standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"))
    
  df_geo <- read_csv(latdata) 
    
  df_site_long <- 
    df_site %>% 
    filter(funding_agency != "Dedup") %>% 
    group_by(orgunituid, fiscal_year, psnu, indicator, numeratordenom, sitename) %>% 
    summarize(across(.cols = starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    reshape_msd() %>% 
    clean_indicator() %>% 
    arrange(orgunituid, indicator, period)
  

# TX_PLVS Validity --------------------------------------------------------

  df_pvls_valid <- 
    df_site_long %>% 
    select(orgunituid, period, psnu, indicator, sitename, value) %>% 
    pivot_wider(names_from = indicator,
                values_from = value) %>% 
    mutate(TX_CURR_lag2 = lag(TX_CURR, n = 2), .by = c(orgunituid),
           tx_pvls_flag = ifelse(TX_PVLS_D > TX_CURR_lag2, 1, 0),
           pvls_txcurr_diff = TX_PVLS_D - TX_CURR_lag2) %>% 
    mutate(pvls_flag = ifelse(tx_pvls_flag == 1, 1, NA_integer_)) %>% 
    group_by(orgunituid) %>% 
    fill(pvls_flag, .direction = c("downup")) %>% 
    ungroup()
  
  df_site_pvls_invalid <- 
    df_pvls_valid %>% 
    group_by(orgunituid) %>% 
    mutate(across(.cols = c(TX_CURR:TX_PVLS_D), \(x) calc_pct_change(x), .names = "{.col}_pct_chg")) %>% 
    ungroup() %>% 
    mutate(pct_chg_flag = ifelse(TX_PVLS_D_pct_chg >= 1, 1, NA_integer_)) %>% 
    group_by(orgunituid) %>% 
    fill(pct_chg_flag, .direction = c("downup")) %>% 
    ungroup() %>% 
    left_join(df_geo) %>% 
    mutate(across(where(is.numeric), ~ na_if(., Inf)))

  write_csv(df_site_pvls_invalid, "Dataout/MWI_site_tx_pvls_validation.csv", na = "")


# VLC across time with offset bars ----------------------------------------

  df_vlc <- 
    df_site %>% 
    group_by(fiscal_year, indicator, numeratordenom) %>% 
    summarize(across(.cols = starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    reshape_msd() %>% 
    clean_indicator() %>% 
    arrange(indicator, period) %>% 
    select(-c(period_type, numeratordenom)) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    mutate(TX_CURR_lag2 = lag(TX_CURR, n = 2),
           VLC = TX_PVLS_D / TX_CURR_lag2)
  
  
  df_vlc %>% 
    filter(period > "FY22Q2") %>% 
    vlc_plot(font_size = 10) +
    labs(x = NULL, y = NULL, 
         title = "VIRAL LOAD COVERAGE ROSE NEARLY 20% FROM FY22Q3 TO FY23Q4",
         subtitle = "TX_CURR in light gray, TX_PVLS_D in light blue",
         caption = glue("{metadata$caption}"))
  si_save(glue("Graphics/MWI_{metadata$curr_pd}_VLC.svg"))
  
  # Now wrap this across the Districts  df_vlc <- 
  df_vlc_psnu <- 
    df_site %>% 
    group_by(fiscal_year, psnu, snu1, indicator, numeratordenom) %>% 
    summarize(across(.cols = starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    reshape_msd() %>% 
    clean_indicator() %>% 
    arrange(snu1, psnu, indicator, period ) %>% 
    select(-c(period_type, numeratordenom)) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    mutate(TX_CURR_lag2 = lag(TX_CURR, n = 2), .by = psnu,
           VLC = TX_PVLS_D / TX_CURR_lag2) %>% 
    clean_psnu()
  
  region <- df_vlc_psnu %>% distinct(snu1) %>% pull()
  
  vlc_plot_region <- function(.data, region, font_size = 7){
    .data %>% 
      filter(period > "FY22Q2", snu1 == {{region}}) %>% 
      mutate(psnu_order = fct_reorder(psnu, TX_PVLS_D, .desc = T)) %>% 
      vlc_plot(font_size = font_size) +
      facet_wrap(vars(psnu_order), scales = "free_y") +
      labs(x = NULL, y = NULL, 
           title = glue("{str_to_upper(region)}: VIRAL LOAD COVERAGE ROSE NEARLY 20% FROM FY22Q3 TO FY23Q4"),
           caption = glue("{metadata$caption}"))
    si_save(glue("Graphics/{region}_{metadata$curr_pd}_VLC.svg"))
  }
  
  purrr::map(region, ~vlc_plot_region(df_vlc_psnu, .x))
  
  
  df_vlc_psnu %>% 
    filter(period > "FY22Q2", snu1 == {{region}}) %>% 
    mutate(psnu_order = fct_reorder(psnu, TX_PVLS_D, .desc = T)) %>% 
    vlc_plot(font_size = 7) +
    facet_wrap(vars(psnu_order), scales = "free_y") +
    labs(x = NULL, y = NULL, 
         title = glue("{region}: VIRAL LOAD COVERAGE ROSE NEARLY 20% FROM FY22Q3 TO FY23Q4"),
         caption = glue("{metadata$caption}"))
  
  
  
  
  
# QOQ Deltas --------------------------------------------------------------

  df_dlta <- 
    df_site_long %>% 
    arrange(orgunituid, indicator, period) %>% 
    mutate(pct_chg = (value/lag(value, n = 1)) - 1, .by = c(orgunituid, indicator))
  
  df_dlta %>% distinct(orgunituid, sitename)
  
  # Create a flag for over a 100% change from FY23Q2 - FY23Q3
  df_dlta <- 
    df_dlta %>% 
    ungroup() %>% 
    mutate(over_100_flag = case_when(
      abs(pct_chg) > 1 & period == "FY23Q3" ~ 1, 
      TRUE ~ 0
    )) %>% 
    arrange(orgunituid, indicator, period) %>% 
    mutate(delta = (value - lag(value)), .by = c(orgunituid, indicator)) %>% 
    mutate(site_flag = ifelse(over_100_flag == 1, 1, NA_integer_)) %>% 
    group_by(orgunituid, indicator) %>% 
    fill(site_flag, .direction = c("downup")) %>% 
    ungroup()
  
  
  #  Spread out TX_CURR and see if two period lag is larger than TX_PVLS_D
  df_dlta %>% 
    select(orgunituid, period, psnu, indicator, sitename, value) %>% 
    pivot_wider(names_from = indicator,
                values_from = value) %>% 
    mutate(TX_CURR_lag2 = lag(TX_CURR, n = 2), .by = c(orgunituid))

  df_dlta %>% write_csv("MWI_FY23Q3_tx_pvls_anomalies.csv")
  

# Investigate occurences of numbers ============================================================================
  
  df_site %>% 
      filter(funding_agency != "Dedup", !is.na(qtr3)) %>% 
      count(qtr3, sort = T) %>% 
      top_n(10) %>% 
      ggplot(aes(x = qtr3, y = n)) +
      geom_col() +
      glitr::si_style() +
      scale_y_continuous(breaks = seq(0, 130, 10))
    
    df_site %>% 
      filter(funding_agency != "Dedup", !is.na(qtr3)) %>% 
      mutate(tmp = n(), .by = qtr3) %>% 
      mutate(rank = dense_rank(-tmp)) %>% 
      filter(rank <= 10) %>% 
      select(orgunituid, sitename, funding_agency, psnu, mech_code, mech_name, 
             contains("qtr"), indicator, standardizeddisaggregate, tmp) %>% 
      ggplot(aes(x = qtr3, group = orgunituid), fill = "white") +
      geom_histogram(width = 0.5) +
      geom_hline(yintercept = 1:125, color = "white", linewidth = 0.5) +
      glitr::si_style_xline()
    
# What about a series of faceted waffle graphs with the top 10 numbers    
    library(waffle)
    
   df_site %>% 
      filter(funding_agency != "Dedup", !is.na(qtr3)) %>% 
      count(qtr3, standardizeddisaggregate, sort = T) %>% 
      top_n(9) %>% prinf()
    
   waffle(parts = df_site %>% 
            filter(funding_agency != "Dedup", !is.na(qtr3)) %>% 
            count(qtr3, sort = T) %>% 
            top_n(5), size = 1) 
    
    
    
      count(qtr3, sort = T) %>% 
      top_n(15) %>% 
      ggplot(aes(x = qtr3, y = n)) +
      geom_col() +
      glitr::si_style() +
      scale_y_continuous(breaks = seq(0, 130, 10))
    
    
    
  # Create a list of sites that have 35 or 38
    df_issues <- 
      df_site %>% 
      filter(funding_agency != "Dedup",
             qtr3 %in% c(38, 35)) %>% 
      select(orgunituid, sitename, funding_agency, psnu, mech_code, mech_name, 
             contains("qtr"), indicator, standardizeddisaggregate) %>% 
      arrange(psnu, orgunituid, sitename) %>% 
      mutate(indicator = case_when(
        standardizeddisaggregate == "Total Numerator" ~ "TX_PVLS_N",
        TRUE ~ "TX_PVLS_D")
      ) %>% 
      select(-standardizeddisaggregate) %>% 
      mutate(orgid_count = n(), .by = orgunituid, 
             flag = ifelse(orgid_count == 2, "flag", "noflag")) 
 
    # 114 sites that are creating the inflation of TX_PVLS and thereby affecting
    # VL data
    df_problems <- 
      df_issues %>% 
      filter(flag == "flag")
    
    
    df_problems %>% 
      select(-c(qtr1, qtr2, qtr4)) %>% 
      pivot_wider(names_from = indicator, 
                  values_from = qtr3) %>% 
      mutate(Total_TX_PVLS_N = sum(TX_PVLS_N), 
             Total_TX_PVLS_D = sum(TX_PVLS_D))
          
      
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

