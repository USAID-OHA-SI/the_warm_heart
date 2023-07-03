# PROJECT: Explore Malawi Q2 data
# PURPOSE: Munge and Analysis of MWI MER data
# AUTHOR: Tim Essam | SI
# REF ID:   fa4c2fb8
# LICENSE: MIT
# DATE: 2023-07-03
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library(gtExtras)
    library(selfdestructin5)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"), "Archive")
    file_path <- return_latest(folderpath = merdata,
      pattern = "PSNU_IM.*Malawi")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "fa4c2fb8"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_msd <- read_psd(file_path) 
  df_pepfar <- df_msd %>% mutate(funding_agency = "PEPFAR")

# Summary Tables ============================================================================
  
    mdb_df   <- make_mdb_df(df_msd)
    mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd) 
      #mutate(agency = "PEPFAR") 
    
    # Create the treatment data frame needed for derived indicators
    mdb_df_tx    <- make_mdb_tx_df(df_msd)
    mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)
      #mutate(agency = "PEPFAR") 
  
# VIZ ============================================================================

  mdb_tbl %>% 
    # filter(indicator != "GEND_GBV") %>%
    create_mdb(ou = "Malawi", type = "main", metadata$curr_pd, metadata$source) %>%
      shrink_rows()  %>% 
      gtsave_extra(path = "Images", filename = glue::glue("MWI_PEPFAR_{metadata$curr_pd}_mdb_main.png"))  
    
    
    create_mdb(mdb_tbl_tx, ou = "Malawi", type = "treatment", metadata$curr_pd, metadata$source) %>% 
      bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
      embiggen() %>% 
      tab_options(
        data_row.padding = px(1),
        row_group.padding = px(2),
        heading.padding = px(1)
      ) %>% 
      gtsave_extra(., path = "Images", filename = glue::glue("{metadata$curr_pd}_MWI_PEPFAR_MMD_VL_MD.png"))   
      
      

#  IP SUMMARY TABLE -------------------------------------------------------

    mech_list <- df_msd %>% 
        filter(funding_agency == "USAID", fiscal_year == metadata$curr_fy)  %>% 
        distinct(mech_code, mech_name, prime_partner_name) %>% 
        pull(mech_code)
      
    map(mech_list, ~mk_ptr_tbl(df_msd, .x, cntry = "Malawi"))  
      

# Look at VLS and VLC ============================================================================

  df_vl <- create_vl_df(df_msd) %>% 
        filter(str_detect(funding_agency, "USAID|CDC")) %>% 
        clean_agency() %>% 
        filter(period %ni% c("FY21Q2", "FY21Q1"))
        
      
  #Create labels
  pd_label <- df_vl %>% distinct(period) %>% 
    mutate(label = ifelse(str_detect(period, "Q1|Q3"), period, "")) %>% 
    pull(label)
        

      top_ip <- 
        df_vl %>% 
        ggplot(aes(x = period, group = 1)) +
        geom_line(aes(y = vls), color = burnt_sienna) +
        geom_point(aes(y = vls), shape = 21, fill = burnt_sienna, size = 3,
                   color = "white") +
        geom_line(aes(y = vlc), color = denim) +
        geom_point(aes(y = vlc), shape = 21, fill = denim, size = 3,
                   color = "white") +
        geom_text(aes(y = vlc, label = percent(vlc, 1)), size = 9/.pt,
                  family = "Source Sans Pro", color = denim, 
                  vjust = -1) +
        geom_text(aes(y = vls, label = percent(vls, 1)), size = 9/.pt,
                  family = "Source Sans Pro", color = burnt_sienna, 
                  vjust = -1) +
        si_style_nolines(facet_space = 0.5) +
        facet_wrap(~funding_agency, nrow = 1) +
        theme(axis.text.y = element_blank(), 
              axis.text.x = element_blank()) +
        labs(x = NULL, y = NULL) +
        expand_limits(y = c(0.7,1.05)) 
      
      bottom_ip <- 
        df_vl %>% 
        ggplot(aes(x = period)) +
        geom_col(aes(y = tx_curr_lag2), fill = grey10k) +
        geom_col(aes(y = tx_pvls_d), fill = denim) +
        si_style_ygrid(facet_space = 0.5) +
        scale_y_continuous(labels = comma) +
        labs(x = NULL, y = NULL) +
        facet_wrap(~funding_agency, nrow = 1) +
        scale_x_discrete(labels = pd_label) +
        coord_cartesian(expand = F) +
        #get rid of facet labels
        theme(strip.text.x = element_blank()) +
        labs(caption = metadata$caption)
      
      top_ip / bottom_ip + plot_layout(heights = c(1, 3)) +
        plot_annotation(title = glue("VIRAL LOAD SUMMARY FOR {metadata$curr_fy} BY PARTNER")) &
        theme(plot.tag = element_text(family = "Source Sans Pro"))
      