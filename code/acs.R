library(tidyverse)
library(tidycensus)
library(tigris)
library(cwi)
library(sf)
library(sysfonts)
library(extrafont)
library(showtext)
#Make all of the ACS blockgroup-level variables in one go!
font_add_google(name = "Roboto Condensed")
showtext_auto(enable = T)
showtext_opts(dpi = 300)
bg_shps <- block_groups(state = "CT") %>% 
  filter(ALAND != 0) %>% 
  select(GEOID)

vari <- rio::import("data/acs_vars.xlsx") %>% 
  drop_na()
pop <- get_acs("block group", variables = "B01001_001", state = "CT") %>% 
  select(GEOID, pop = estimate)

vari %>% group_by(variable) %>% 
  group_split() %>% 
  walk(function(info){
    dta <- get_acs("block group", variables = info$variable, 
                   state = "CT") 
    
    if(info$percent) dta <- left_join(dta, pop, by = "GEOID") %>% mutate(estimate = estimate/pop)
    dta_shp <- dta %>% left_join(bg_shps, by = "GEOID") %>% 
      st_as_sf() %>% 
      ggplot() + 
      geom_sf(aes(fill = estimate), color = NA) + 
      labs(fill = info$short_desc, 
           title = paste0("Quick choropleth for ", info$short_desc)) + 
      scale_fill_viridis_c(option = "magma") + 
      theme_void() + 
      theme(text = element_text(family = "Roboto Condensed"))
    message(info$short_desc)
    ggsave(paste0("maps/acs/", str_remove(info$short_desc, "\\%"), ".png"), 
           plot = dta_shp, 
           width = 10, height = 7)
    
  })
