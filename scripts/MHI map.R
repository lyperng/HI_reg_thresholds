# make color coded MHI map

##################################################
# install and load libraries
PKG <- c('tidyverse','ggspatial','mapdata', 'marmap', 'sf','tigris','rnaturalearth','rnaturalearthdata')
#c("ggplot2", 'ggmap', 'maps','usmap', 'ggsn', 'usmap', "RColorBrewer")
# 'marmap' for visualizing marine data, including bathymetric contours
# 'ggrepel' could be useful for making sure labels don't run into each other
# but didnt need for this map

for (p in PKG) {
  
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

##################################################
# Get bathymetry data
bathy <- getNOAA.bathy(-160.5, -154.5, 18.5, 22.5, res = 1, keep = TRUE)
ggbathy <- fortify(bathy)

##################################################
# get tiger shapefiles with the fishing community (CCD) outlines

# read in census data from downloaded tiger/line shapefiles
ccd<-st_read("data/ccd/tl_2010_15_cousub00.shp") # read in from download
head(ccd)
ccd_wgs<-st_transform(ccd,"+proj=longlat +datum=WGS84")
head(ccd_wgs)

# or get directly using  'tigris' package
HIccd<-county_subdivisions('HI')
head(HIccd)
HIccd_wgs<-st_transform(HIccd,"+proj=longlat +datum=WGS84")
head(HIccd_wgs)

HIccd_land<-erase_water(HIccd_wgs) # erase the portion of ccd that is water 'tigris' pkg

# read in water data
counties<-unique(HIccd_land$COUNTYFP) # what are the diff county codes?
water<-area_water('HI',counties) # get water area for all counties

# assign county names for nicer plotting
HIccd_land <- HIccd_land %>% mutate(County = case_when(
  COUNTYFP == '001' ~ "Hawai'i",
  COUNTYFP == '003' ~ "Honolulu",
  COUNTYFP == '005' ~ NA,
  COUNTYFP == '007' ~ "Kaua'i",
  COUNTYFP == '009' ~ 'Maui'
))

counordered <- c("Kaua'i", "Honolulu", "Maui", "Hawai'i")
HIccd_land$County<-factor(HIccd_land$County, levels = counordered)

##################################################

######## plot MHI map with fishing comms (CCDs) marked and bathy contours #######

### first plot inset world map
world <- ne_countries(scale='medium',returnclass = 'sf') # using rnaturalearthdata
class(world)
st_crs(world)
  
# Set the CRS to the Lambert azimuthal equal-area projection centered on 120 longitude
crs <- "+proj=laea +lat_0=52 +lon_0=-1200000 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
world <- st_transform(world, crs)

# Create a bounding box for Hawaii
hawaii_bbox <- st_bbox(c(xmin = -162, xmax = -153, ymax = 18, ymin = 23), crs = st_crs(4326))
hawaii_bbox <- st_as_sfc(hawaii_bbox)
hawaii_bbox <- st_sf(geometry = hawaii_bbox)

# Plot the world map
inset<-ggplot() +
  geom_sf(data = world) +
  geom_sf(data = hawaii_bbox, fill = NA, color = "red", linewidth = 0.7) +
  theme_bw() +
  coord_sf(crs = st_crs(world), 
           xlim = c(-3000000, 10000000), 
           ylim = c(-2000000, 7000000),expand = FALSE) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
inset

######## plot MHI map ########

### add contours
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

### add hawaii ccd and water layers
map1<-map_cont +
   geom_sf(data = HIccd_land, aes(fill = County)) + # plot the ccd data
   geom_sf(data = water, fill = alpha('#A6CEE3',0.3)) + # plot the water data
   geom_sf(data=HIccd_wgs, fill=alpha('grey',0)) + # add ccd outlines on the water too
#   geom_sf(data = HIpoly, aes(group = Group.1, fill = county), color = "black") + # plot simple island outlines   
  coord_sf(xlim = c(-160.5, -154.6), ylim = c(18.7, 22.4), expand = FALSE) +
  # set the map edge limits, expand F means nothing can go past these limits
  scale_fill_manual(values = c("Kaua'i" = alpha("#E78AC3",0.7), "Honolulu" = alpha("#8DA0CB",0.7), "Maui" = alpha("#66C2A5",0.7), "Hawai'i" = alpha("#FC8D62",0.7))) +
#  scale_fill_manual(values = c("Kaua'i" = alpha("#77AADD",0.6), "Honolulu" = "#99DDFF", "Maui" = "#44BB99", "Hawai'i" = "#EEDD88")) +
#  scale_fill_manual(values = c("Kaua'i" = alpha("#BBCCEE",0.8), "Honolulu" = "#CCEEFF", "Maui" = "#CCDDAA", "Hawai'i" = "#EEEEBB")) +
  labs(title = "Main Hawaiian Islands", fill = "County",
       x='Longitude', y='Latitude')
map1

### adjust layout and aesthetics
map2<-map1 +
  theme(plot.title = element_text(hjust = 0.5, size = 28),              
        axis.title=element_text(size=22,face="plain"), #adjust size of axis titles
        axis.text=element_text(size=16, color = 'black'), #adjust font size of axis tick labels
        legend.position = c(0.17, 0.48),  # manually adjust legend position
        legend.background = element_blank(), # need to set this otherwise it is opaque
        legend.box.background = element_rect(color = 'black', fill=alpha("white", 0.4)),
        # set legend border, fill, transparency
        legend.key = element_rect(fill = "transparent", colour = NA),# if not set, 
        legend.key.size = unit(0.8, 'cm'), # adjust size of ea fill box
        legend.spacing.y = unit(0.12, 'cm'), # space between fill boxes, need guides()
        legend.text=element_text(size=16), 
        legend.title=element_text(hjust = 0.5, size = 17),
        legend.margin = margin(5,12,8,10)) +
  guides(fill=guide_legend(byrow = T))
map2

### add scalebar and north arrow
map_scale <-map2 +
  ggspatial::annotation_scale(
    style = 'ticks',
    height = unit(-0.25, "cm"),
    tick_height = 0.6,
    line_width = 1,
    location = "tr",
    pad_x = unit(0.56, "in"), pad_y = unit(0.38, "in"),
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
    pad_x = unit(0.61, "in"), pad_y = unit(0.15, "in"),
    width_hint = 0.17,
    text_pad = unit(-1.82, "in"),
    line_col = "grey30"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.96, "in"), pad_y = unit(0.45, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )
  )  +
  annotation_custom(
    grob = ggplotGrob(inset),
    xmin = -160.3,
    xmax = -158.7,
    ymin = 18.85,
    ymax = 19.85
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
