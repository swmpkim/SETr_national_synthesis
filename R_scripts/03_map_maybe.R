library(dplyr)
library(here)
library(leaflet)

# read in the points for the map
in_path <- here::here("data", "intermediate", "locations.csv")
loc_dat <- read.csv(in_path, stringsAsFactors = FALSE)


# read in the pie charts and turn them into icons
in_path <- here::here("R_output", "figures", "pie_charts")

# file names will look like this
files_0 <- dir(in_path, pattern = "dir_0.png")
files_slr <- dir(in_path, pattern = "dir_slr.png")

# loop through reserves to read in icons
reserves <- unique(loc_dat$reserve)

# set up the lists for the icons to go into
# icons_0 <- list()
# icons_slr <- list()

file_paths_0 <- paste0(in_path, "/", reserves, "_dir_0.png")
file_paths_slr <- paste0(in_path, "/", reserves, "_dir_slr.png")

# for(i in seq_along(reserves)){
#         res_in <- reserves[i]
#         res_0 <- paste0(res_in, "_dir_0.png")
#         res_slr <- paste0(res_in, "_dir_slr.png")
#         
#         
#         # read in 0 icon
#         if(res_0 %in% files_0){
#                 icon_path <- here::here(in_path, res_0)
#                 icons_0[[i]] <- makeIcon(iconUrl = icon_path,
#                                          iconWidth = 30,
#                                          iconHeight = 30)
#                 
#         } else {
#                 icons_0[[i]] <- "you don't have a pie chart for this reserve"
#         }
#         
#         
#         # read in slr icon
#         if(res_slr %in% files_slr){
#                 icon_path <- here::here(in_path, res_slr)
#                 icons_slr[[i]] <- makeIcon(iconUrl = icon_path,
#                                          iconWidth = 30,
#                                          iconHeight = 30)
#                 
#         } else {
#                 icons_slr[[i]] <- "you don't have a pie chart for this reserve"
#         }
#         
#         # name the piece of the list
#         names(icons_0)[[i]] <- reserves[i]
#         names(icons_slr)[[i]] <- reserves[i]
# }


# set up the color palette, for the legend
to_color <- c("#c00000", "#f4a582", "#f0f0f0", "#92c5de", "#2f5597")  
names(to_color) <- c("Lower, CIs don't overlap", "Lower, CIs overlap", 
                     "Not enough information",
                     "Higher, CIs overlap", "Higher, CIs don't overlap")

# make a map, using each icon for its associated location


##############################
# the help file for "icons" should be useful here
# also that general part of the manual:
# https://cran.r-project.org/web/packages/leaflet/leaflet.pdf
##############################


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
                  opacity = 0.8,
                  title = "Compared to 0")
map_0



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
                  opacity = 0.8,
                  title = "Compared to long-term SLR")
map_slr

