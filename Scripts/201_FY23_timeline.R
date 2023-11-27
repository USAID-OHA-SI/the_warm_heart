# PROJECT: Timeline visualization
# PURPOSE: Munge and Analysis of key events in Malawi
# AUTHOR:  Tim Esssam | SI
# REF ID:  cad1ecce
# LICENSE: MIT
# DATE:   2023-11-27
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(googlesheets4)

  # SI specific paths/functions  
    load_secrets()
  
  # REF ID for plots
    ref_id <- "cad1ecce"
    
  # Objects
  gs_id <- "1AUTZT3Vdj3HalnCl_lpS_XTaqom9rDlAYLdAS3ZxllU"

# LOAD DATA ============================================================================  

  df_tl <- read_sheet(gs_id) %>% 
    mutate(across(c(start_date:end_date), ~as.Date(.x))) 
  
 tl_beg <- as.Date("2022-01-01")
 tl_end <- as.Date('2024-01-01')
  
# MUNGE ============================================================================
  
  df_tl %>% glimpse()  
 
# VIZ ============================================================================

  # Create a timeline of the major events that occurred.

  df_tl %>% 
    ggplot(aes(y = 1)) +
   annotate(geom = "rect", xmin = tl_beg, xmax = tl_end, ymin = 0, ymax = 2, 
            fill = grey10k) +
    geom_vline(xintercept = seq.Date(from = tl_beg, to = tl_end, by = "quarter"),
               color = "white") +

    geom_segment(aes(x = start_date, xend = end_date, yend = 1, color = event)) +
    geom_point(aes(x = start_date, color = event)) +
    geom_point(aes(x = end_date)) +
    ggrepel::geom_text_repel(aes(label = event, x = start_date)) +
   scale_y_continuous(lim = c(0,2)) +
   scale_x_date(date_breaks = "1 month", date_labels = "%b") +
   labs(x = NULL, y = NULL) +
   si_style_nolines() +
   theme(axis.text.y = element_blank(), 
         legend.position = "none") +
  coord_fixed(ratio = 30)
 si_save("Images/MWI_timeline_FY23.svg")

# SPINDOWN ============================================================================

