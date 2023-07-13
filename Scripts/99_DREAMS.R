# PROJECT: DREAMS Abstract
# PURPOSE: Munge and Analysis of
# AUTHOR:  Tim Esssam | SI
# REF ID:  57e6f7d7
# LICENSE: MIT
# DATE:   2023-07-12
# NOTES:   

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
    library(broom)
    library(estimatr)
    set.seed(42)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- "../../../Downloads/Facility AGYW PrEP uptake_v2.xlsx"
  
  # REF ID for plots
    ref_id <- "57e6f7d7"
    
# LOAD DATA ============================================================================  

  df <- readxl::read_xlsx(file_path, sheet = 2)

  df %>% count(psnu, Program) %>% arrange(Program)
  
# MUNGE ============================================================================
  
  #  What does the distribution of facility look like by DREAMS/NON-Dreams
  
  # What is the % of Facilities in each cohort
  janitor::tabyl(df, Program)
  
  df %>% count(Program, psnu, wt = PrEP_NEW)
  
  # Basic strip plot to see distribution of sites
  # Prompted ask for additional data to condition on site volume
  df %>% 
    ggplot(aes(x = PrEP_NEW, y = Program, color = Program)) +
    geom_point()
  
# VIZ ============================================================================

  # There are two sites that could be outliers
  # Want to be sure differences are not driven by these sites
  df_flt <- df %>% filter(PrEP_NEW < 700)
  
  # Create colors to use for DREAMS / NON-DREAMS sites / groups
  my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")[c(1, 2)]
  
  # Create a base ggplot upon which we will build
  g <- ggplot(df, aes(x = Program, y = PrEP_NEW, color = Program, fill = Program)) +
    scale_y_continuous() +
    scale_color_manual(values = my_pal, guide = "none") +
    scale_fill_manual(values = my_pal, guide = "none")
  
  # Grab means for the groups, we'll use this in the plots
  means <- aggregate(PrEP_NEW ~  Program, df, mean)
  
  
  # Create a combination density plot + boxplot + rainfall distribution cloud
  # to show the shape of the distribution for each group, as well as median etc
  g + 
    geom_boxplot(
      width = .15, fill = "white",
      size = 1, outlier.shape = NA
    ) +
    ggdist::stat_halfeye(
      adjust = .33, ## bandwidth
      width = .67,  ## remove slab interval
      position = position_nudge(x = .15)
    ) +
    gghalves::geom_half_point(
      side = "l", 
      range_scale = .3, 
      alpha = .5, size = 3
    ) +
      stat_summary(
        geom = "point",
        fun = mean,
        color = grey10k,
        size = 8
      ) +
    coord_flip() +
    geom_text(data = means, aes(label = round(PrEP_NEW, 0), y = PrEP_NEW), size = 3) +
    si_style_xgrid() +
    labs(title = "DREAMS SITES AVERAGED 128 CLIENTS ENROLLED ON PREP_NEW WHILE\nNON-DREAMS SITES AVERAGED 38",
         x = NULL) 
    si_save("Images/DREAMS_analysis_prep_new.png")
  
    
    
  # Conclusions
  # Two major outliers in the "treated group"
  # Can remove or keep in, for now we are removing to show what conditioning on TX_CURR looks like
  # Plots are touched up in Adobe Illustrator
    
  df_flt %>% 
    ggplot(aes(x = TX_CURR, y = PrEP_NEW, color = Program)) +
    geom_point() +
    stat_smooth(method = "loess", fill = grey10k, alpha = 0.25) +
    scale_color_manual(values = my_pal, guide = "none") +
    labs(title = "CONDITIONING ON TX_CURR, DREAMS SITES HAVE HIGHER PrEP_NEW INITIATIONS THAN NON-DREAMS SITES",
         subtitle = "Each dot represents a site plotted along the intersection of TX_CURR and PrEP_NEW") +
    si_style()
  
  si_save("Graphics/DREAMS_analysis_prep_new_filtered.svg")
  

# Basic Analysis ============================================================================

  # Basic table of DREAMS vs NON-DREAMS Results
  # likely not the appropriate test given endogenous placement of DREAMS sites
  # But, it's a starting point
  tt <- t.test(PrEP_NEW ~ Program, df_flt)
  tidy(tt)
  
  # Non-parametric test
  wt <- wilcox.test(PrEP_NEW ~ Program, df_flt)
  tidy(wt)
  
  # What if we do a simple regression and control for site volume.
  # After controlling for TX_CURR, going from a non-dreams to dreams site
  # increases PrEP_NEW enrollments by about 75. 
  
  df_reg_flt <- df_flt %>% mutate(treatment = ifelse(Program == "DREAMS", 1, 0))
  
  reg <- lm_robust(PrEP_NEW ~ TX_CURR + treatment,
                               clusters = psnu, data = df_reg_flt)
  tidy(reg)
  
  # Could look at correlation between DREAMS / Non-DREAMS and volume
  # Not sure this gives much other than showing that there is a higher
  # correlation in DREAMS sites
  
  df_flt %>% 
    group_by(treatment) %>% 
    summarise(corr = cor(PrEP_NEW, TX_CURR))
  

