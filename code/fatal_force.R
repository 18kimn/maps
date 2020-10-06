library(tidyverse)
library(sf)
library(cwi)
library(ggmap)
library(ggrepel)
library(sysfonts)
library(extrafont)
library(showtext)

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

#SETUP: Date format

datesuffix <- function(dom) { switch(as.character(dom), 
                                     "1"="st", "2"="nd", 
                                     "3"="rd", "th") }
nicedate <- function(date){ 
  map(date, function(d){
    dlt <- as.POSIXlt(d)
    paste0(format(d, "%B %d"), datesuffix(dlt$mday), ", ", 
           format(d, "%Y")) 
  }) %>% unlist()
}
#from https://stackoverflow.com/questions/6292247/r-date-format-how-can-i-change-the-date-format-as-the-1st-june-2011

#Approximate the location(s) that is/are missing from the city centroid
town_centroids <- st_centroid(town_sf)

#Finally, clean the data:
murders <-  read_csv("https://github.com/washingtonpost/data-police-shootings/releases/download/v0.1/fatal-police-shootings-data.csv") %>% 
  filter(state == "CT") %>% 
  left_join(town_centroids, by = c("city" = "name")) %>% 
  mutate(longitude = ifelse(is.na(longitude), 
                            st_coordinates(geometry),
                            longitude),
         latitude = ifelse(is.na(latitude),
                           st_coordinates(geometry)[,2],
                           latitude), #interesting but useful that this syntax works
         lbl =str_to_upper(name),
         lbl2 = paste0(city, ". ", nicedate(date), ".")
         ) %>% 
  st_as_sf(coords = c("longitude", "latitude"), remove = F) %>% 
  st_set_crs(4326) 

#This method allows me to cut out the areas that are just water
state_outline <- map_dfr(c("09", "36", "25","44"), tigris::block_groups) %>% 
  filter(ALAND > 0) %>% 
  group_by(STATEFP) %>% 
  summarise(geometry = st_union(geometry))
  

background <- get_stamenmap(c(left = -73.82777,
                              bottom = 40.98014, 
                              right = -71.6,
                              top = 42.1),
                            maptype = "toner-background")
#Okay here we go
#' Given a Set of Points and Box sizes, find locations
#' Written by @zachp, updated by @slowkow
findboxes <- function(
  df, xcol, ycol,
  box_padding_x, box_padding_y,
  point_padding_x, point_padding_y,
  xlim, ylim,
  label_size = 3,
  force = 1e-7, maxiter = 20000
) {
  
  # x and y posiitons as a dataframe
  posdf <- df[c(xcol, ycol)]
  
  # returnd a df where columns are points
  boxdf <- apply(posdf, 1, function(row) {
    xval <- row[xcol]
    yval <- row[ycol]
    return(c(
      xval - box_padding_x / 2,
      yval - box_padding_y / 2,
      xval + box_padding_x / 2,
      yval + box_padding_y / 2
    ))
  })
  # columns are x1,y1,x2,y2
  boxmatrix <- as.matrix(t(boxdf))
  
  moved <- ggrepel:::repel_boxes(
    data_points = as.matrix(posdf),
    point_padding_x = point_padding_x,
    point_padding_y = point_padding_y,
    boxes = boxmatrix,
    xlim = xlim,
    ylim = ylim,
    hjust = 0.5,
    vjust = 0.5,
    force = force,
    maxiter = maxiter
  )
  
  finaldf <- cbind(posdf, moved)
  names(finaldf) <- c("x1", "y1", "x2", "y2")
  return(finaldf)
}

#
newlocs <- findboxes(st_drop_geometry(murders), 
                     xcol = "longitude", ycol = "latitude", 
                     box_padding_x = Reduce("-", rev(range(murders$longitude))) * 0.08,
                     box_padding_y = Reduce("-", rev(range(murders$latitude))) * 0.05,
                     point_padding_x = Reduce("-", rev(range(murders$longitude))) * 0.08,
                     point_padding_y = Reduce("-", rev(range(murders$latitude))) * 0.05,
                     force = 1e-5, xlim = c(-73.82777, -71.6), 
                     ylim =  c( 40.98014,42.1))


murders <- murders %>% select(-contains("(x|y)2")) %>% bind_cols(select(newlocs, x2, y2))

#First step is to get x and y locations for labels depending on where geom_label_repel 
(
murder_plot <- ggplot() + 
  geom_sf(data = state_outline, fill = "gray20", size = 1, inherit.aes = F) + 
  #geom_sf(data  = town_sf, fill = NA, inherit.aes = F) + 
  geom_sf(data = murders, color = "red", inherit.aes = F) + 
  geom_segment(data = murders, 
                 aes(x = longitude, y = latitude, xend = x2, yend= y2),
                 color = "red"
    ) + 
  geom_label(data = murders, 
                   aes(x = x2, y = y2, label = paste0("\n\n", lbl2)),
                   size = 1, color = "red", fontface = "plain", 
             fill = "gray70", 
                   inherit.aes= F) + 
  geom_text(data = murders, 
            aes(x = x2, y = y2 + .01, label = lbl),
            size = 1, color = "red", 
            fontface = "bold",
            inherit.aes = F
            ) + 
  annotate("text", x = -73.6, y = 41.5, label = rep(0:9, 10), 
           size = 1) +
  scale_x_continuous(limits = c(-73.82777, -71.6)) + 
  scale_y_continuous(limits = c( 40.98014,42.1)) + 
  theme_void() 
)

murder_plot$data

ggsave("maps/police_murders.png", plot = murder_plot)



ggplot(newlocs) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_point(aes(x1, y1), color = "black") + 
  geom_sf(data  = state_outline, fill = NA, inherit.aes = F) + 
  scale_x_continuous(limits = c(-73.82777, -71.6)) + 
  scale_y_continuous(limits = c( 40.98014,42.1)) 
  
  # geom_point(aes(x2, y2), color = "red")
NULL
