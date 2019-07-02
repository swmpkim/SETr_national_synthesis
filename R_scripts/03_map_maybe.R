library(dplyr)
library(here)
library(leaflet)

# read in the points for the map
in_path <- here::here("data", "intermediate", "locations.csv")
loc_dat <- read.csv(in_path, stringsAsFactors = FALSE) %>% 
        arrange(reserve) # make sure it's alphabetical because file lists for icons will be


# look for pie charts here
in_path <- here::here("R_output", "figures", "for_maps")

# file names of pie charts for different purposes
file_paths_0 <- dir(in_path, pattern = "dir_0.svg", full.names = TRUE)
file_paths_slr <- dir(in_path, pattern = "dir_slr.svg", full.names = TRUE)


# set up the color palette and labels for the legend
to_color <- c("#c00000", "#f4a582", "#fffacd", "#92c5de", "#2f5597") 
names(to_color) <- c("Lower, CIs don't overlap", "Lower, CIs overlap", 
                     "Not enough information",
                     "Higher, CIs overlap", "Higher, CIs don't overlap")


# make maps, using each icon for its associated location

# compared to 0
map_0 <- leaflet(loc_dat) %>% 
        addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>% 
        addMarkers(data = loc_dat,
                   lng = ~long,
                   lat = ~lat,
                   icon = ~icons(
                         iconUrl = file_paths_0,
                         iconHeight = 50,
                         iconWidth = 50
                   )) %>% 
        addLegend(colors = to_color,
                  labels = names(to_color),
                  position = "bottomleft",
                  opacity = 0.9,
                  title = "Compared to 0")
map_0


# compared to sea level rise
map_slr <- leaflet(loc_dat) %>% 
        addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>% 
        addMarkers(data = loc_dat,
                   lng = ~long,
                   lat = ~lat,
                   icon = ~icons(
                           iconUrl = file_paths_slr,
                           iconHeight = 50,
                           iconWidth = 50
                   )) %>% 
        addLegend(colors = to_color,
                  labels = names(to_color),
                  position = "bottomleft",
                  opacity = 0.9,
                  title = "Compared to long-term SLR")
map_slr
