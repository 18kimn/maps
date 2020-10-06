library(tidyverse)
library(sf)
library(cowplot)
library(viridis)

#SETUP: Fonts
font_add_google("Heebo", family = "Heebo")
showtext_auto() 
showtext_opts(dpi  =300)
theme_src <- function(base_family = "Heebo", base_size = 4, ...) {
  camiller::theme_din(base_family = base_family, base_size = base_size, ...) +
    theme(plot.title.position = "plot",
          plot.caption.position = "panel",
          strip.text = element_text(face = "bold"),
          legend.title = element_text(size = rel(1)),
          legend.text = element_text(size = rel(0.75)),
          legend.key.width = unit(1.1, "lines"),
          legend.key.height = unit(0.8, "lines"))
}

theme_set(theme_src())
update_geom_defaults("text", list(family = "Heebo", fontface = "bold"))

finance <- rio::import("data/schools/sdf17_1a.sas7bdat") %>% 
  as_tibble() %>% 
  select(LEAID, CENSUSID, FIPST, TOTALEXP)
basic <- read_csv("data/schools/demo.csv", skip = 14)
shapes <- tigris::school_districts(state = "CT") %>% 
  filter(ALAND > 0)
shapes <- st_read("data/schools/shapes/schooldistrict_sy1819_tl19.shp") 

bins <- seq(0, 50000, by = 5000)
schools <- basic %>% 
  left_join(finance, by = c("NCES District ID" = "LEAID")) %>% 
  left_join(shapes, by = c("NCES District ID" = "GEOID"))  %>% 
  filter(TOTALEXP > 0 ) %>% 
  drop_na(TOTALEXP, `Students*`) %>% 
  mutate(exp_per = TOTALEXP / as.numeric(`Students*`),
         exp_per = unlist(map(exp_per, ~which.max(. < bins)))-1, 
         exp_per = factor(bins[exp_per], levels = bins) %>% 
           fct_relabel(~paste0("$", str_replace_all(., "000$", ",000"))), 
         exp_per = exp_per %>% 
           fct_relabel(~paste0(., " to ", levels(exp_per)[which(levels(exp_per) == .) +1]))) %>% 
  st_as_sf()

ct_outline <- tigris::block_groups(state = "CT") %>% 
  filter(ALAND > 0) %>% 
  st_union()

sd_finance <- ggplot(schools) + 
  geom_sf(aes(fill = exp_per)) + 
  geom_sf(data = ct_outline, fill = NA, size = 1) + 
  scale_fill_viridis_d(option = "magma", begin = .2) + 
  labs(fill = "Expenditure per student", 
       title = "Where are schools funded?", 
       subtitle = "Total public educaiton expenditure per student in the 2016-2017 academic year.",
       caption = "Data and shapefiles from the National Center for Education Statistics.") + 
  theme_void() + 
  theme(text = element_text(size = 36), 
        plot.title = element_text(face = "bold", family = "Heebo", size = rel(1.5)),
        plot.subtitle = element_text(face = "plain", size = rel(1.2)), 
        plot.caption = element_text(face = "plain")) 

  
ggsave("maps/sd_finance.png", sd_finance, width = 8, height = 5, dpi = 300)
