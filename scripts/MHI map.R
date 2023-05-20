# make color coded MHI map

# install and load libraries
PKG <- c('tidyverse','ggspatial','mapdata', 'marmap', 'sf','rnaturalearth', 'rnaturalearthdata')
#c("ggplot2", 'ggmap', 'maps','usmap', 'ggsn', 'usmap', "RColorBrewer")
# 'marmap' for visualizing marine data, including bathymetric contours
# 'ggrepel' could be useful for making sure labels don't run into each other
# but didnt need for this map

for (p in PKG) {
  
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

worldmap<-map_data('world')
HI <-subset(worldmap, subregion == 'Hawaii')
HI$group<-factor(HI$group)

# Transform data points into geographic objects
HIsf <- HI %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

HIpoly = st_sf(
  aggregate(
    HIsf$geometry,
    list(HIsf$group),
    function(g){
      st_cast(st_combine(g),"POLYGON")
    }
  ))

HIpoly$county<-c("Hawai'i",'Maui','Maui','Maui','Honolulu',"Kaua'i","Kaua'i")
HIpoly$county <- factor(HIpoly$county, levels = c("Kaua'i", "Honolulu", 'Maui', "Hawai'i"))

# Get bathymetry data
bathy <- getNOAA.bathy(-160.5, -154.5, 18.5, 22.5, res = 1, keep = TRUE)
ggbathy <- fortify(bathy)

##################################################
# everything above and below this works together to create a nice map
# in this block, I am essentially plotting the same map, but with the CCD outlines

# read in census data from tiger/line shapefiles
tracts <- st_read("data/tracts/tl_rd22_15_tract.shp")
head(tracts)
tracts_wgs<-st_transform(tracts,"+proj=longlat +datum=WGS84")
head(tracts_wgs)

ccd<-st_read("data/ccd/tl_2010_15_cousub10.shp")
head(ccd)
ccd_wgs<-st_transform(ccd,"+proj=longlat +datum=WGS84")
head(ccd_wgs)

# assign county names for nicer plotting
ccd_wgs <- ccd_wgs %>% mutate(County = case_when(
  COUNTYFP10 == '001' ~ "Hawai'i",
  COUNTYFP10 == '003' ~ "Honolulu",
  COUNTYFP10 == '005' ~ NA,
  COUNTYFP10 == '007' ~ "Kaua'i",
  COUNTYFP10 == '009' ~ 'Maui'
))

##################################################

###### below is in progress of trying to plot the MHI and bathy data together
# so far, the MHI data alone works fine using geom_polygon, but cant add the contours because they are on diff projections

# plot map
map_cont<- ggplot() +
  theme_bw() +
  geom_contour(
    data = ggbathy, aes(x = x, y = y, z = z), # plot the bathy data with long, lat, depth
    binwidth = 40, color = "grey90",linewidth = 0.3 #binwidth determines how far apart the contour lines are
    # 40 has really close lines, essentially works to shade darker closer to land
    ) +
#  geom_contour(
 #   data = ggbathy, aes(x = x, y = y, z = z),
  #  binwidth = 200, color = "grey80",linewidth = 0.3
  #) +
  geom_contour(
    data = ggbathy, aes(x = x, y = y, z = z),
    binwidth = 800, color = "grey65", linewidth = 0.4
    # give some darker outlines for layering
  )
   # set the map edge limits, expand F means nothing can go past these limits
  
map_cont

map1<-map_cont +
   geom_sf(data = ccd_wgs, aes(fill = County)) + # plot the ccd data
#   geom_sf(data = HIpoly, aes(group = Group.1, fill = county), color = "black") + # plot simple island outlines   
  coord_sf(xlim = c(-160.5, -154.6), ylim = c(18.7, 22.4), expand = FALSE) +
  # set the map edge limits, expand F means nothing can go past these limits
  scale_fill_manual(values = c("Kaua'i" = alpha("#E78AC3",0.7), "Honolulu" = alpha("#8DA0CB",0.7), "Maui" = alpha("#66C2A5",0.7), "Hawai'i" = alpha("#FC8D62",0.7))) +
#  scale_fill_manual(values = c("Kaua'i" = alpha("#77AADD",0.6), "Honolulu" = "#99DDFF", "Maui" = "#44BB99", "Hawai'i" = "#EEDD88")) +
#  scale_fill_manual(values = c("Kaua'i" = alpha("#BBCCEE",0.8), "Honolulu" = "#CCEEFF", "Maui" = "#CCDDAA", "Hawai'i" = "#EEEEBB")) +
  labs(title = "Main Hawaiian Islands", fill = "County",
       x='Longitude', y='Latitude')
map1

map2<-map1 +
  theme(plot.title = element_text(hjust = 0.5, size = 28),              
        axis.title=element_text(size=22,face="plain"), #adjust size of axis titles
        axis.text=element_text(size=16, color = 'black'), #adjust font size of axis tick labels
        legend.position = c(0.16, 0.27),  # manually adjust legend position
        legend.background = element_blank(), # need to set this otherwise it is opaque
        legend.box.background = element_rect(color = 'black', fill=alpha("white", 0.5)),
        # set legend border, fill, transparency
        legend.key = element_rect(fill = "transparent", colour = NA),# if not set, 
        legend.key.size = unit(0.8, 'cm'), # adjust size of ea fill box
        legend.spacing.y = unit(0.12, 'cm'), # space between fill boxes, need guides()
        legend.text=element_text(size=16), 
        legend.title=element_text(hjust = 0.5, size = 17),
        legend.margin = margin(5,12,8,10)) +
  guides(fill=guide_legend(byrow = T))
map2

map_scale <-map2 +
  # Add scale and North arrow
  ggspatial::annotation_scale(
    style = 'ticks',
    height = unit(-0.25, "cm"),
    tick_height = 0.6,
    line_width = 1,
    location = "tr",
    pad_x = unit(0.46, "in"), pad_y = unit(0.38, "in"),
    width_hint = 0.17,
    text_pad = unit(-1.82, "in"),
    line_col = "grey30"
  ) +
  ggspatial::annotation_scale(
    style = 'ticks',
    unit_category = 'imperial',
    height = unit(0.25, "cm"),
    tick_height = 0.6,
    line_width = 1,
    location = "tr",
    pad_x = unit(0.51, "in"), pad_y = unit(0.15, "in"),
    width_hint = 0.17,
    text_pad = unit(-1.82, "in"),
    line_col = "grey30"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.87, "in"), pad_y = unit(0.45, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )
  )

map_scale

ggsave('MHI map.png', 
       width =  10, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')
  
##########################################################################
#####################################
# old code using usmap because i thought mapdata didnt contain hawaii
# usmap moves hawaii so that it can be displayed next to contiguous us--wrong lat/long

# get hawaii map data with county boundaries from usmap package
hawaii_data<-us_map(regions = 'counties', include = 'Hawaii')

# filter for MHI (Kauai, Oahu, Maui, Hawaii)
county_data <- hawaii_data[hawaii_data$county %in% c("Kauai County", "Honolulu County", "Maui County", "Hawaii County"),]
unique(county_data$county)
usmap_crs()

# Transform data points into geographic objects
county_data1 <- county_data %>%
  st_as_sf(coords = c("x", "y"), crs = usmap_crs())

# Convert the Hawaii map data to the WGS 84 coordinate system
hawaii_wgs84 <- st_transform(county_data1, crs = 4326)
# this works, but the resulting lat long are off, they should be the same as the bathymetry lat long limits set below

# colnames gave issues so renamed
county_df<-county_data[,c(1,2,6,10)]
names(county_df)<-c('long','lat', 'group','county')

map<-
    plot_usmap(regions = 'counties', include = 'Hawaii') +
  #geom_polygon(data = county_df, aes(x = long, y = lat, group = group, fill = county), color = "black") +
  # geom_sf() +
  # geom_sf(data = hawaii_wgs84, aes(group = group, fill = county), color = "black") +
  # coord_equal() +
  theme_void() +
  scale_fill_manual(values = c("Kauai County" = "#E78AC3", "Honolulu County" = "#8DA0CB", "Maui County" = "#66C2A5", "Hawaii County" = "#FC8D62")) +
  labs(title = "Hawaii Islands", fill = "County") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) 
map


######################################
# test script found online with bathymetric contours
pays <- rnaturalearth::ne_countries(
  country = c("France", "Spain"),
  scale = "large", returnclass = "sf"
)
# Get bathymetry data
bathy <- getNOAA.bathy(-8, 0, 42, 50, res = 1, keep = TRUE)
ggbathy <- fortify(bathy)

# Base plot
pl <- ggplot(data = pays) +
  geom_contour(
    data = ggbathy, aes(x = x, y = y, z = z),
    binwidth = 200, color = "grey80", size = 0.3
  ) +
  
  geom_sf()+
  coord_sf(xlim = c(-6, 0), ylim = c(43, 48.5), expand = FALSE) +
  theme_bw()

pl
# Add scale and North arrow
pl +
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

# started to try for MHI, but cannot filter to state data, only country works so far
# using original code with data from usmap for now
us <- ne_countries(country = 'United States of America',scale = "medium", returnclass = "sf")

# Subset the data for Hawaii
hawaii <- us %>%
  filter(name == "Hawaii")

# Plot the map of Hawaii
plot(hawaii)
