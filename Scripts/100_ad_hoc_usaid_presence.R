# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  "push" countries review - agency presence 
# REF ID:   96c239cf 
# LICENSE:  MIT
# DATE:     2023-10-04
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(gagglr)
library(glue)
library(scales)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(sf)


# GLOBAL VARIABLES --------------------------------------------------------

ref_id <- "a31c1c38" #id for adorning to plots, making it easier to find on GH

get_metadata(type = "NAT_SUBNAT") #list of MSD metadata elements
metadata_subnat <- metadata

get_metadata(type = "PSNU_IM") #list of MSD metadata elements

shpdata <- file.path(glamr::si_path("path_vector"))

# IMPORT ------------------------------------------------------------------
cntry <- "Malawi"

df_msd <- si_path() %>% 
  return_latest("PSNU_IM") %>% 
  read_psd() %>% 
  filter(country == cntry)

df_subnat <- si_path() %>% 
  return_latest("NAT_SUBNAT") %>% 
  read_psd() %>% 
  filter(country == cntry)

# shp_tza_snu1 <-  si_path("path_vector") %>% 
#   return_latest("malawi_snu1.shp", recursive = TRUE) %>% 
#   st_read() %>% 
#   select(snu1uid = uid, geometry)


  terr <- gisr::get_raster(path = si_path("path_raster"))

# Map infrastructure
  spdf_pepfar <- gisr::get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
  mwi_geo <- purrr::map(c(3, 4, 6), ~spdf_pepfar %>% gisr::extract_boundaries(country = cntry, 
                                                                       level = .x))
  names(mwi_geo) <- list("adm0", "snu1", "psnu")


  basemap <- terrain_map(countries = mwi_geo$adm0,
                         adm0 = mwi_geo$adm0,
                         # adm1 = admin1,
                         mask = TRUE,
                         terr = terr)




# MUNGE -------------------------------------------------------------------

df_plhiv <- df_subnat %>% 
  filter(indicator == "PLHIV",
         fiscal_year == max(fiscal_year)) %>% 
  pluck_totals() %>% 
  count(fiscal_year, psnu, psnuuid, wt = targets, name = "plhiv") %>% 
  clean_psnu()

sf_plhiv <- full_join(mwi_geo$psnu, df_plhiv, by = c("uid" = "psnuuid"))

df_agencies <- df_msd %>% 
  filter(indicator == "TX_CURR",
         psnu != "_Military Malawi",
         fiscal_year == metadata$curr_fy, 
         mech_code != "84553") %>% 
  pluck_totals() %>% 
  clean_agency() %>% 
  count(psnu, psnuuid, funding_agency, mech_code, mech_name, wt = targets) %>% 
  filter(funding_agency != "DEDUP")
  # group_by(funding_agency, psnu, psnuuid) %>% 
  # mutate(share = n / sum(n)) %>% 
  # filter(share == max(share)) %>% 
  # ungroup() %>% 
  # select(psnu, psnuuid, funding_agency)


sf_agencies <- left_join(mwi_geo$psnu, df_agencies, by = c("uid" = "psnuuid"))


# VIZ ---------------------------------------------------------------------


m1 <- ggplot() +
  geom_sf(aes(fill = plhiv, geometry = geometry),
          data = sf_plhiv, alpha = .8,
          color = "gray", na.rm = TRUE) +
  # geom_sf_text(aes(label = psnu, geometry = geometry),
  #              data = sf_agencies %>% filter(funding_agency == "USAID"),
  #              family = "Source Sans Pro", color = grey10k) +
  scale_fill_viridis_c(label = label_number(scale_cut = cut_short_scale())) +
  labs(x = "", y = "", fill = "PLHIV (2024)",
       caption = "") +
  si_style_map() +
  si_legend_fill()



  basemap +  
  geom_sf(aes(geometry = geometry), 
          data = mwi_geo$adm0, fill = NA, alpha = 0.25) +
    geom_sf(aes(geometry = geometry), 
            data = mwi_geo$snu1, fill = NA, color = grey90k, alpha = 0.25) +
  geom_sf(aes(geometry = geometry), 
          data = mwi_geo$psnu, fill = NA, color = "white") +
  geom_sf(aes(fill = funding_agency, geometry = geometry),
          data = sf_agencies, alpha = .6,
          color = "white", na.rm = TRUE) +
  geom_sf(aes(geometry = geometry), 
          data = mwi_geo$adm0, fill = NA, color = grey60k) +
  scale_fill_manual(values = c("USAID" = denim,
                               "CDC" = scooter_light)) +
  facet_wrap(~funding_agency) +
  labs(x = "", y = "", title = "USAID & CDC SUPPORTED DISTRICTS IN FY23",
       subtitle = "Presence based on TX_CURR supported clients in FY23Q3 | mech code 84553 removed",
       caption = metadata$caption) +
  si_style_map() +
  theme(legend.position = "none")

si_save("Images/mwi_usaid_presence_omit_84553.png")

