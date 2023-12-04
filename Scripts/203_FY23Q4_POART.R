# PROJECT: Q4 POART Visuals for FY23Q4
# PURPOSE: Munge and Analysis of PSNU x IM MSD for Malawi
# AUTHOR:  Tim Esssam | SI
# REF ID:  0405242c
# LICENSE: MIT
# DATE:   2023-11-30
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)
    library(gt)
    library(gtExtras)
    library(selfdestructin5)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    site_path <- return_latest(folderpath = merdata,
      pattern = "Site_IM.*Malawi")
    
    file_path <- return_latest(folderpath = merdata,
                               pattern = "PSNU_IM.*Malawi")
    
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "0405242c"
    
  # Functions  
  # Returns TX_CURR by default with no disaggregations  
  calc_trends <- function(.data, indicator = "TX_CURR", ...){
    df <- .data %>% 
      filter(indicator == {{indicator}}, 
             standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
      group_by(fiscal_year, indicator, ...) %>% 
      summarise(across(matches("targ|qtr"), \(x) sum(x, na.rm = T))) %>% 
      ungroup() %>% 
      reshape_msd(direction = "quarters") %>% 
      mutate(achv = results_cumulative / targets,
             qtr_flag = ifelse(period %in% c(metadata$curr_pd), 1, 0)
      ) 
    return(df)
  }
  
  plot_trends <- function(.data, results = results, text_size = 10, fill_color = scooter){
    
    text_size = text_size/.pt
    
    .data %>% 
      ggplot(aes(x = period)) +
      geom_col(aes(y = targets), fill = grey20k, 
               position = position_nudge(x = -0.15), width = width) +
      geom_col(aes(y = {{results}}), fill = fill_color, 
               width = width) +
      geom_text(aes(y = {{results}}, label = percent(achv, 1)), 
                size = text_size,
                family = "Source Sans Pro", 
                color = grey90k,
                vjust = -0.2) 
  }

# LOAD DATA ============================================================================  

  df_msd <- read_psd(file_path) 
      
  # What's causing issues in the new MSD?
  df_msd %>% names() %>% grep("targ", ., value = T)
  df_msd <- df_msd %>% select(-c(target_age_2024, target_modality_2024))  
    
  df_site <- read_psd(site_path)

# MUNGE ============================================================================
  
  # Pull TX overall
  df_tx <- calc_trends(df_msd, "TX_CURR")
  df_tx_new <- calc_trends(df_msd, "TX_NEW")    
  
  # Pull TX by Mechs
  
  
  # Pull TX by SNU1
  df_tx_snu <- calc_trends(df_msd, "TX_CURR", snu1)
  df_tx_new_snu <- calc_trends(df_msd, "TX_NEW", snu1)
  
  
  # Key Partners
  
  
# TX_CURR Summary ============================================================================

  width <- 0.5
  
  # Key treatment trends across time
  df_tx %>% 
    plot_trends() + 
    scale_y_continuous(labels = comma) +
    si_style_ygrid() +
    labs(x = NULL, y = "TX_CURR",
         title = "TX_CURR OVER THE LAST 12 QUARTERS",
         subtitle = "Grey bars represent TX_CURR targets; Achivement above results as a percentage",
         caption = glue("{metadata$caption}"))
  si_save("Images/MWI_TX_CURR_trends_national.png", scale = 1.25)
  
  # SNU1 TX_CURR
  df_tx_snu %>% 
    filter(str_detect(snu1, "Military", negate = T), 
           period > "FY21Q4") %>% 
    mutate(snu1_order = fct_reorder(snu1, results_cumulative, .desc = T)) %>% 
    plot_trends() + 
    facet_wrap(~snu1_order) +
    scale_y_continuous(labels = comma) +
    si_style_ygrid(facet_space = 0.5) +
    labs(x = NULL, y = "TX_CURR",
         title = "TX_CURR OVER THE LAST 8 QUARTERS BY REGION",
         subtitle = "Grey bars represent TX_CURR targets",
         caption = glue("{metadata$caption}"))
  si_save("Images/MWI_TX_CURR_trends_snu1.png", scale = 1.25)
  
  

# TX_NEW Summary ----------------------------------------------------------

  # Key TX_NEW Trends
  df_tx_new %>% 
    plot_trends(results = results_cumulative, fill_color = scooter_med) + 
    scale_y_continuous(labels = comma) +
    si_style_ygrid() +
    labs(x = NULL, y = "TX_NEW",
         title = "TX_NEW QUARTERLY CUMULATIVE ACHIEVEMENT OVER THE LAST 12 QUARTERS",
         subtitle = "Grey bars represent targets",
         caption = glue("{metadata$caption}"))
  si_save("Images/MWI_TX_NEW_trends_national.png", scale = 1.25)  
  
  df_tx_new_snu %>%
    filter(str_detect(snu1, "Military", negate = T), 
           period > "FY21Q4") %>%
    mutate(snu1_order = fct_reorder(snu1, results_cumulative, .desc = T)) %>% 
    plot_trends(results = results_cumulative, fill_color = scooter_med) + 
    scale_y_continuous(labels = comma) +
    si_style_ygrid(facet_space = 0.5) +
    labs(x = NULL, y = "TX_NEW",
         title = "TX_NEW QUARTERLY CUMULATIVE ACHIEVEMENT OVER THE LAST 8 QUARTERS",
         subtitle = "Grey bars represent targets",
         caption = glue("{metadata$caption}")) +
    facet_wrap(~snu1_order)
  si_save("Images/MWI_TX_NEW_REGION_trends_national.png", scale = 1.25)
  
  # Quarterly results trends (non-cumulative)
  df_tx_new %>%
    ggplot(aes(x = period, y = results)) +
    geom_col(fill = scooter_med, width = 0.65) +
    geom_text(aes(label = comma(results)),
              size = 12/.pt,
              family = "Source Sans Pro",
              color = grey90k,
              vjust = -0.2) +
    si_style_ygrid() +
    labs(x = NULL, y = "TX_NEW",
         title = "TX_NEW QUARTERLY RESULTS OVER THE LAST 12 QUARTERS",
         caption = glue("{metadata$caption}")) +
    theme(axis.text.y = element_blank())
  si_save("Images/MWI_TX_NEW_trends_by_quarter.png", scale = 1.25)  
  
  # Same as above but by Region
  df_tx_new_snu %>% 
    filter(str_detect(snu1, "Military", negate = T), 
           period > "FY21Q4") %>%
    mutate(snu1_order = fct_reorder(snu1, results_cumulative, .desc = T)) %>% 
    ggplot(aes(x = period, y = results)) +
    geom_col(fill = scooter_med, width = 0.65) +
    geom_text(aes(label = comma(results)),
              size = 12/.pt,
              family = "Source Sans Pro",
              color = grey90k,
              vjust = -0.2) +
    si_style_ygrid(facet_space = 0.5) +
    facet_wrap(~snu1_order) +
    labs(x = NULL, y = "TX_NEW",
         title = "TX_NEW QUARTERLY RESULTS OVER THE LAST 8 QUARTERS BY REGION",
         caption = glue("{metadata$caption}")) +
    theme(axis.text.y = element_blank())
  si_save("Images/MWI_TX_NEW_REGION_trends_by_quarter.png", scale = 1.25)  
  
# BY PARTNER  ============================================================================

  df_tx_mech <- calc_trends(df_msd %>% filter(fiscal_year == metadata$curr_fy), 
                            "TX_CURR", mech_code, mech_name)
  
  df_tx_new_mech <- calc_trends(df_msd %>% filter(fiscal_year == metadata$curr_fy), 
                                "TX_NEW", mech_code, mech_name)  

  # Viz TX_CURR
  df_tx_mech %>% 
    filter(period == metadata$curr_pd, mech_name != "Dedup") %>% 
    mutate(mech_order = fct_reorder(mech_name, results)) %>% 
    ggplot(aes(y = mech_order)) +
    geom_col(aes(x = targets), fill = grey20k, width = 0.75,
             position = position_nudge(y = 0.1)) +
    geom_col(aes(x = results), fill = scooter, width = 0.75) +
    geom_text(aes(x = results, label = percent(achv, 1)), 
              size = 10/.pt,
              family = "Source Sans Pro", 
              color = grey90k,
              hjust = -0.2) +
    scale_x_continuous(labels = comma, position = "top") +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 80)) +
    si_style_xgrid() +
    labs(x = "TX_CURR", y = NULL,
         title = "TX_CURR ANNUAL PERFORMANCE BY PARTNER FY23",
         subtitle = "Grey bars represent TX_CURR targets; Achivement next to results as a percentage",
         caption = glue("{metadata$caption}"))
  si_save("Images/MWI_TX_CURR_partner_performance.png", scale = 1.25)
  
  df_tx_new_mech %>% 
    filter(period == metadata$curr_pd, mech_name != "Dedup") %>% 
    mutate(mech_order = fct_reorder(mech_name, results_cumulative)) %>% 
    ggplot(aes(y = mech_order)) +
    geom_col(aes(x = targets), fill = grey20k, width = 0.55,
             position = position_nudge(y = 0.15)) +
    geom_col(aes(x = results_cumulative), fill = scooter_med, width = 0.55) +
    geom_text(aes(x = results_cumulative, label = percent(achv, 1)), 
              size = 10/.pt,
              family = "Source Sans Pro", 
              color = grey90k,
              hjust = -0.2) +
    scale_x_continuous(labels = comma, position = "top") +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 80)) +
    si_style_xgrid() +
    labs(x = "TX_NEW", y = NULL,
         title = "TX_NEW ANNUAL PERFORMANCE BY PARTNER FY23",
         subtitle = "Grey bars represent TX_NEW targets; Achivement next to results as a percentage",
         caption = glue("{metadata$caption}"))
  si_save("Images/MWI_TX_NEW_partner_performance.png", scale = 1.25)


# IIT SUMMARY -------------------------------------------------------------

  full_pds <- (min(df_site$fiscal_year) - 1) %>% 
    paste0("-10-01") %>% 
    as_date() %>% 
    seq.Date(convert_qtr_to_date(metadata$curr_pd), by = "3 months") %>% 
    convert_date_to_qtr()
  
  pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "") 
  
  df_iit <- df_site %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_RTT"), 
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus")) %>%
    group_by(fiscal_year, snu1, trendscoarse, facility, facilityuid, indicator) %>% 
    #group_by(fiscal_year, snu1, facility, facilityuid, indicator, mech_code, mech_name, psnu) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}") %>% 
    arrange(period) %>% 
    group_by(trendscoarse, facilityuid) %>% 
    mutate(tx_curr_lag1 = lag(tx_curr, n = 1)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  # IIT
  df_iit %>% 
    filter(#trendscoarse == "15+", 
      trendscoarse == "<15",
      str_detect(snu1, "Military", negate = T),
      period >= "FY22Q2") %>% 
    mutate(snu1 = fct_relevel(snu1, c("Southern Region", "Central Region", "Northern Region"))) %>% 
    filter(!is.na(snu1), period != min(period)) %>% 
    ggplot(aes(period, iit, size = tx_curr_lag1)) +
    geom_point(position = position_jitter(width = .2, seed = 42),
               na.rm = TRUE, color = scooter,
               alpha = .2) +
    geom_smooth(aes(weight = tx_curr_lag1, group = snu1),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                size = 1.5, color = golden_sand) +
    facet_wrap(~snu1) +
    scale_size(label = comma) +
    #scale_x_discrete(labels = full_pds) +
    scale_y_continuous(limits = c(0,.25),
                       label = percent_format(1),
                       oob = oob_squish) +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("Pediatric IIT ticked up slightly in {metadata$curr_pd}") %>% toupper,
         caption = glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 25%
                        Source: {metadata$source}")) +
    si_style() +
    theme(panel.spacing = unit(.5, "line"),
          plot.subtitle = element_markdown())
  si_save("Images/IIT_ped_increase_snu1.png", scale = 1.35)
  
  # Adults
  df_iit %>% 
    filter(trendscoarse == "15+",
      str_detect(snu1, "Military", negate = T),
      period >= "FY22Q2") %>% 
    mutate(snu1 = fct_relevel(snu1, c("Southern Region", "Central Region", "Northern Region"))) %>% 
    filter(!is.na(snu1), period != min(period)) %>% 
    ggplot(aes(period, iit, size = tx_curr_lag1)) +
    geom_point(position = position_jitter(width = .2, seed = 42),
               na.rm = TRUE, color = scooter,
               alpha = .2) +
    geom_smooth(aes(weight = tx_curr_lag1, group = snu1),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                size = 1.5, color = golden_sand) +
    facet_wrap(~snu1) +
    scale_size(label = comma) +
    #scale_x_discrete(labels = full_pds) +
    scale_y_continuous(limits = c(0,.25),
                       label = percent_format(1),
                       oob = oob_squish) +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("Adult IIT stayed flat in {metadata$curr_pd}") %>% toupper,
         caption = glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 25%
                        Source: {metadata$source}")) +
    si_style() +
    theme(panel.spacing = unit(.5, "line"),
          plot.subtitle = element_markdown())
  
  si_save("Images/IIT_adults_increase_snu1.png", scale = 1.35)
  
  

# SPARK IIT ---------------------------------------------------------------

  df_iit_snu1 <- df_site %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_RTT"), 
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus"),
           str_detect(snu1, "Military", negate = T)) %>%
    group_by(fiscal_year, snu1, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}") %>% 
    group_by(snu1) %>% 
    mutate(tx_curr_lag1 = lag(tx_curr, n = 1)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(period != min(period))
  
  df_iit_spark <- 
    df_iit_snu1 %>% 
    select(period, snu1, iit) %>% 
    arrange(snu1, period) %>% 
    group_by(snu1) %>% 
    summarize(spark_iit = list(iit), .groups = "drop") 
  
  df_iit_snu1 %>% 
    select(period, snu1, iit) %>%
    spread(period, iit) %>% 
    left_join(., df_iit_spark) %>% 
    gt() %>%
    gt_plt_sparkline(spark_iit, 
                     same_limit = , type = "shaded", 
                     fig_dim = c(10, 30),
                     palette = c(grey70k, grey90k, old_rose_light, scooter_med, grey10k),
                     label = F) %>% 
    fmt_percent(columns = where(is.numeric)) %>% 
    cols_label(snu1 = "",
               spark_iit = "",
               FY21Q2 = "Q2",
               FY21Q3 = "Q3",
               FY21Q4 = "Q4",
               FY22Q2 = "Q2",
               FY22Q3 = "Q3",
               FY22Q4 = "Q4",
               FY23Q2 = "Q2",
               FY23Q3 = "Q3",
               FY23Q4 = "Q4"
    ) %>% 
    tab_header(
      title = glue("INTERRUPTION IN TREATMENT SUMMARY BY REGION"),
    ) %>% 
    tab_spanner(
      label = "FY21",
      columns = 2:4
    ) %>% 
    tab_spanner(
      label = "FY22",
      columns = 5:8
    ) %>% 
    tab_spanner(
      label = "FY23",
      columns = 9:12
    ) %>% 
    
    tab_source_note(
      source_note = gt::md(glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW\n
                        Source: {metadata$source}"))) %>% 
    tab_options(
      source_notes.font.size = px(10),
      data_row.padding = px(1),
    ) %>% 
    gt_theme_nytimes() %>% 
    gtsave_extra("Images/MWI_iit_REGION.png")
  
# NET NEW -----------------------------------------------------------------

  df_net_new <- 
    df_msd %>% 
    filter(indicator %in% c("TX_NEW", "TX_NET_NEW", "TX_CURR"), 
           standardizeddisaggregate == "Age/Sex/HIVStatus", 
           fiscal_year %in% c(2021, 2022, 2023)) %>% 
    group_by(fiscal_year, indicator, psnu) %>% 
    summarise(across(matches("targ|qtr"), \(x) sum(x, na.rm = T))) %>% 
    ungroup() %>% 
    reshape_msd(direction = "quarters") %>% 
    mutate(results = case_when(
      indicator %in% c("TX_NEW", "TX_NET_NEW") ~ results_cumulative,
      TRUE ~ results
    )) %>% 
    select(-c(targets, results_cumulative)) %>% 
    pivot_wider(names_from = indicator, values_from = results)

  # Cont proxy TX_CURR / TX_CURR_lag4 + TX_NEW
  df_net_new_psnu <- 
    df_net_new %>% 
    filter(str_detect(psnu, "Military", negate = T)) %>% 
    arrange(psnu, period) %>% 
    mutate(cont_proxy = TX_CURR / (lag(TX_CURR, n = 4) + TX_NEW), .by = psnu) %>% 
    filter(period == "FY23Q4") %>% 
    clean_psnu() %>% 
    mutate(psnu_order = fct_reorder(psnu, TX_NET_NEW, .desc = T)) 
    
    df_net_new_psnu %>% 
    ggplot(aes(x = psnu_order)) +
    geom_col(aes(y = TX_NET_NEW, fill = ifelse(TX_NET_NEW > 0, scooter_med, old_rose)), width = 0.75) +
    si_style() +
    scale_fill_identity() +
    scale_y_continuous(labels = comma, limits = c(-4000, 4000)) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(x = NULL, y = "TX_NET_NEW",
         title = "TX_NET NEW AND CONTINUITY OF TREATMENT BY DISTRICT")

    si_save("Graphics/TX_NN_top.svg")
  
    df_net_new_psnu %>% 
      ggplot(aes(x = psnu_order)) +
      geom_col(aes(y = 1), width = 0.75, fill = "white", alpha = 0) +
    geom_point(aes(y = cont_proxy, fill = cont_proxy), size = 9, shape = 21) +
      geom_text(aes(y = cont_proxy, label = percent(cont_proxy, 1)), 
                size = 7/.pt) +
      scale_fill_viridis_c(direction = -1, option = "B") +
      si_style_ygrid() +
      theme(legend.position = "none") + 
      labs(x = NULL, y = "",
           caption = glue("{metadata$caption}"))  

    si_save("Graphics/TX_NN_bottom.svg")
  