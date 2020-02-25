library(dplyr)
library(here)
library(leaflet)
library(mapview)

# read in the points for the map
in_path <- here::here("data", "intermediate", "locations.csv")
loc_dat <- read.csv(in_path, stringsAsFactors = FALSE) %>% 
        arrange(reserve) # make sure it's alphabetical because file lists for icons will be


# regions: using same tribble from 05_tables etc.
# gtm isn't really the gulf but we seem to be the closest for mapping?
regions <- tribble(
        ~reserve, ~full_name, ~state, ~regions,
        "APA", "Apalachicola Bay", "FL", "gulf",
        "CBM", "Chesapeake Bay, MD", "MD", "midatl",
        "CBV", "Chesapeake Bay, VA", "VA", "midatl",
        "DEL", "Delaware", "DE", "midatl",
        "ELK", "Elkhorn Slough", "CA", "west",
        "GND", "Grand Bay", "MS", "gulf",
        "GRB", "Great Bay", "NH", "ne",
        "GTM", "Guana Tolomato Matanzas", "FL", "gulf",
        "MAR", "Mission Aransas", "TX", "gulf",
        "NAR", "Narragansett Bay", "RI", "ne",
        "NARUNH", "NAR - Univ of NH partner", "RI", "ne",
        "PDB", "Padilla Bay", "WA", "west",
        "SOS", "South Slough", "OR", "west",
        "WEL", "Wells", "ME", "ne",
        "WKB", "Weeks Bay", "AL", "gulf",
        "WQB", "Waquoit Bay", "MA", "ne"
)

loc_dat <- left_join(loc_dat, regions, by = "reserve")


# look for pie charts here
in_path <- here::here("R_output", "figures", "for_maps")

# file names of pie charts for different purposes
file_paths_0 <- dir(in_path, pattern = "dir_0.svg", full.names = TRUE)
file_paths_slr <- dir(in_path, pattern = "dir_slr.svg", full.names = TRUE)
file_paths_19yr <- dir(in_path, pattern = "dir_19yr.svg", full.names = TRUE)


# turn those into columns in the main data frame, so the correct icons get used during subsetting
loc_dat$file_paths_0 <- file_paths_0
loc_dat$file_paths_slr <- file_paths_slr
loc_dat$file_paths_19yr <- file_paths_19yr


# remove NAR-UNH since there's not enough data
# and it overwrites the NERR's pie chart
loc_dat <- loc_dat %>% 
        filter(reserve != "NARUNH")


# set up the color palette and labels for the legend
to_color <- c("#c00000", "#f4a582", "#fffacd", "#92c5de", "#2f5597") 
names(to_color) <- c("Lower, CIs don't overlap", "Lower, CIs overlap", 
                     "Not enough information",
                     "Higher, CIs overlap", "Higher, CIs don't overlap")


###### each region

subsets <- c("ne", "midatl", "gulf", "west")

for(i in seq_along(subsets)){
        to_map <- loc_dat[loc_dat$regions == subsets[i], ]
        
        map_0 <- leaflet(to_map) %>% 
                addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>% 
                addMarkers(data = to_map,
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
        file_out <- paste0("map0_", subsets[i], ".png")
        out_path <- here::here("R_output", "figures", "maps", file_out)
        mapshot(map_0, file = out_path)
        
        
        map_slr <- leaflet(to_map) %>% 
                addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>% 
                addMarkers(data = to_map,
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
        file_out <- paste0("mapSLR_", subsets[i], ".png")
        out_path <- here::here("R_output", "figures", "maps", file_out)
        mapshot(map_slr, file = out_path)
        
        
        
        map_19yr <- leaflet(to_map) %>% 
                addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>% 
                addMarkers(data = to_map,
                           lng = ~long,
                           lat = ~lat,
                           icon = ~icons(
                                   iconUrl = file_paths_19yr,
                                   iconHeight = 50,
                                   iconWidth = 50
                           )) %>% 
                addLegend(colors = to_color,
                          labels = names(to_color),
                          position = "bottomleft",
                          opacity = 0.9,
                          title = "Compared to 19-year water level change")
        file_out <- paste0("map19yr_", subsets[i], ".png")
        out_path <- here::here("R_output", "figures", "maps", file_out)
        mapshot(map_19yr, file = out_path)
}

#### national level

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
out_path <- here::here("R_output", "figures", "maps", "map0_all.png")
mapshot(map_0, file = out_path)

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
out_path <- here::here("R_output", "figures", "maps", "mapSLR_all.png")
mapshot(map_slr, file = out_path)



# compared to 19-year water level change
map_19yr <- leaflet(loc_dat) %>% 
        addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>% 
        addMarkers(data = loc_dat,
                   lng = ~long,
                   lat = ~lat,
                   icon = ~icons(
                           iconUrl = file_paths_19yr,
                           iconHeight = 50,
                           iconWidth = 50
                   )) %>% 
        addLegend(colors = to_color,
                  labels = names(to_color),
                  position = "bottomleft",
                  opacity = 0.9,
                  title = "Compared to 19-year water level change")
map_slr
out_path <- here::here("R_output", "figures", "maps", "map19yr_all.png")
mapshot(map_19yr, file = out_path)
