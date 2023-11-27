# PROJECT: 
# PURPOSE: Munge and Analysis of
# AUTHOR: Tim Essam | SI
# REF ID:   3097b280
# LICENSE: MIT
# DATE: 2023-07-03
# NOTES: Tim Essam | SI

# LOCALS & SETUP ===============================================================

# Helpers

mk_ptr_tbl <- function(df, mech_id, cntry = "")  { 
  
  # Create a subdirectory under the Images folder if none remains -- useful
  # for staying organized with output
  dir.create("Images/Mechs")
  
  # Create the base data frame for the mechanism
  ip_mdb <- 
    df %>% 
    dplyr::filter(mech_code == mech_id) %>% 
    selfdestructin5::make_mdb_df() %>% 
    selfdestructin5::reshape_mdb_df(., metadata$curr_pd) 
  
  mech_name <-  
    df %>% 
    dplyr::filter(mech_code == mech_id) %>%
    dplyr::distinct(mech_name) %>% 
    dplyr::pull(mech_name)
  
  ip_mdb %>%   
    selfdestructin5::create_mdb(ou = cntry, 
                                type = "main", 
                                pd = metadata$curr_pd, 
                                msd_source = metadata$source,
                                legend = legend_chunk) %>% 
    gt::tab_header(
      title = glue::glue("{mech_name} PERFORMANCE SUMMARY")
    ) %>% 
    gt::gtsave(path = "Images/Mechs", filename = glue::glue("{metadata$curr_pd}_{mech_name}_mdb_main.png"))
  
  print(glue::glue("Created table for mechanism {mech_id}"))
}


# Shrink size of rows in GT tables  
shrink_rows <- function(gt_obj){
  gt_obj %>% 
    gt::tab_options(
      data_row.padding = px(1),
      row_group.padding = px(2),
      heading.padding = px(1)
    ) 
}  


# VLS / VLC
# Creates a wide viral load coverage data frame for use in basic plots
#' Create VL summary 
#'
#' @description Creates a viral load summary table for plotting
#' @param df 
#' @param ... additional grouping arguments to be passed
#'
#' @return a data frame with VLS and VLC calculated
#' @export 
#'
  create_vl_df <- function(df, ...) {
    df <- df %>%
      dplyr::filter(
        indicator %in% c("TX_CURR", "TX_PVLS"),
        standardizeddisaggregate %in% c(
          "Age/Sex/HIVStatus",
          "Age/Sex/Indication/HIVStatus",
          "Age Aggregated/Sex/HIVStatus"
        )
      ) %>%
      gophr::clean_indicator() %>%
      dplyr::group_by(indicator, fiscal_year, ...) %>%
      dplyr::summarise(dplyr::across(tidyselect::starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), 
                       .groups = "drop") %>%
      gophr::reshape_msd(include_type = FALSE) %>%
      tidyr::pivot_wider(
        names_from = indicator,
        names_glue = "{tolower(indicator)}"
      ) %>%
      dplyr::group_by(...) %>% 
      dplyr::mutate(
        tx_curr_lag2 = dplyr::lag(tx_curr, n = 2),
        vlc = tx_pvls_d / tx_curr_lag2,
        vls = tx_pvls / tx_pvls_d,
        vls_adj = tx_pvls / tx_curr_lag2
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange(..., period)
    return(df)
}  
