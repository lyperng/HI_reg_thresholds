# make color coded MHI map

##################################################
# install and load libraries
PKG <- c('tidyverse','ggspatial', 'ggrepel','mapdata', 'marmap', 'sf','tigris',
         'rnaturalearth','rnaturalearthdata', 'scales', 'raster','RColorBrewer')
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
# Creates an object of class raster
r1 <- marmap::as.raster(bathy)
# Defines the target projection
wgsproj <- "+proj=longlat +datum=WGS84"
# Creates a new projected raster object
r2 <- projectRaster(r1, crs = wgsproj)
# Switches back to a bathy object
bathy.projected <- as.bathy(r2)
ggbathy <- fortify(bathy.projected)

##################################################
# get tiger shapefiles with the fishing community (CCD) outlines

# read in census data from downloaded tiger/line shapefiles
#ccd<-st_read("data/ccd/tl_2010_15_cousub00.shp") # read in from download
#head(ccd)
#ccd_wgs<-st_transform(ccd,"+proj=longlat +datum=WGS84")
#head(ccd_wgs)

options(tigris_use_cache = TRUE)

# or get directly using  'tigris' package
HIccd<-county_subdivisions('HI')
head(HIccd)
HIccd_wgs<-st_transform(HIccd,"+proj=longlat +datum=WGS84")
head(HIccd_wgs)

HIccd_land<-erase_water(HIccd_wgs) # erase the portion of ccd that is water 'tigris' pkg

# read in water data
counties<-unique(HIccd_land$COUNTYFP) # what are the diff county codes?
water<-area_water(state = 'HI', county =counties, refresh = TRUE) # get water area for all counties
water_wgs<-st_transform(water,"+proj=longlat +datum=WGS84")

# assign county names for nicer plotting
HIccd_land <- HIccd_land %>% mutate(County = case_when(
  COUNTYFP == '001' ~ "Hawai'i",
  COUNTYFP == '003' ~ "Honolulu",
  COUNTYFP == '005' ~ NA,
  COUNTYFP == '007' ~ "Kaua'i",
  COUNTYFP == '009' ~ 'Maui'
))

#counordered <- c("Kaua'i", "Honolulu", "Maui", "Hawai'i")
#HIccd_land$County<-factor(HIccd_land$County, levels = counordered)

####### section loading and organizing HI map data for index map
# unused in first MHI map

# get counties outlines
state<-counties(state = 'HI')
state_wgs<-st_transform(state,"+proj=longlat +datum=WGS84")

## outline of maui county looks weird since in goes inland to avoid kalawao

# merge maui and kalawao for a single outline, then layer kalawao on top as a cutout
# Select the two counties: Maui and Kalawao
target_counties <- state_wgs[state_wgs$NAME %in% c("Maui", "Kalawao"), ]

# Merge the selected counties into a single feature named "Maui"
maui_merged <- st_union(target_counties)

# Convert maui_merged to an "sf" object
maui_merged_sf <- st_sf(geometry = maui_merged, crs = st_crs(state_wgs))

#pull out kalawao in its own sf object
kalawao<-target_counties[!(target_counties$NAME %in% 'Maui'),] # remove maui

# insert the merged geometry in place of maui's original geometry
state_merged <- state_wgs
state_merged<-state_merged[!(state_merged$NAME %in% 'Kalawao'),] # remove kalawao from original df, or it gets plotted weird
state_merged$geometry[3]<- maui_merged_sf$geometry[1]

# assign county names for nicer plotting in index map
state_fin <- state_merged %>% mutate(County = case_when(
  COUNTYFP == '001' ~ "Hawai'i",
  COUNTYFP == '003' ~ "Honolulu",
  COUNTYFP == '007' ~ "Kaua'i",
  COUNTYFP == '009' ~ 'Maui'
))

# assign county names for nicer plotting in index map
HIccd_wgs <- HIccd_wgs %>% mutate(County = case_when(
  COUNTYFP == '001' ~ "Hawai'i",
  COUNTYFP == '003' ~ "Honolulu",
  COUNTYFP == '005' ~ NA,
  COUNTYFP == '007' ~ "Kaua'i",
  COUNTYFP == '009' ~ 'Maui'
))

#################################################



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
        axis.title = element_blank(),
        panel.grid.major = element_line(color = 'grey90'),
        panel.grid.minor = element_line(color = 'grey90'),
        plot.margin = unit(c(-0.2,-0.2,-0.2,-0.2), "cm"),)
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

#########################################################################
############### plot MHI map with soc-eco prod index scores #############
#########################################################################

### read in calculated indices over time
resfinal<-read_csv('data/DEA_indices.csv')

# aggregate by community means for plotting
temp.resfinal<-resfinal[,-c(1,3)] # delete County column--cannot apply arithmetic functions (aggregating by mean) to strings
inds_mean<-aggregate(.~Community, data = temp.resfinal, FUN = mean)

# read in community and matching diacriticals file
diacrit<-read.csv('data/diacriticals.csv', encoding = 'UTF-8')
names(diacrit)<-c('NAME','Community') # rename columns so they match with indices df
# name simple (non-diacriticals) comm names 'NAME', same as in HIccd spatial files

comm_inds<-plyr::join(inds_mean, diacrit, by='Community', type= 'left') # left join means use all rows in left df
map_inds<-comm_inds[,-1] # delete diacritical comm names column

#join to map data
HI_inds<-left_join(HIccd_land,map_inds, by = 'NAME')
class(HI_inds)

########## map the index values onto earlier MHI map
### add hawaii ccd and water layers
map_eqi<-map_cont +
  geom_sf(data = HI_inds, aes(fill = EQI)) + # plot the indices data (color gradient)
  geom_sf(data = water, fill = alpha('#A6CEE3',0.5), color = 'transparent') + # plot the water data
  geom_sf(data=HIccd_wgs, color = 'grey25', fill = 'transparent', linewidth = 0.3) + # add ccd outlines on the water too
  geom_sf(data=state_fin, aes(color = County), linewidth= 0.7, fill = 'transparent') + # add color coded county outlines
  # geom_sf(data=kalawao, color = 'grey25', fill = alpha('grey75', 0), linewidth = 0.4) + # layer kalawao cutout on top 
  coord_sf(xlim = c(-160.4, -154.6), ylim = c(18.7, 22.4), expand = FALSE) +
  # set the map edge limits, expand F means nothing can go past these limits
  scale_color_manual(values = c("Kaua'i" = alpha("#E78AC3",1), "Honolulu" = alpha("#8DA0CB",1), "Maui" = alpha("#66C2A5",1), "Hawai'i" = alpha("#FC8D62",1))) +
  scale_fill_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')), # gradientn allows assigning a palette instead of high and low color
                       guide = guide_colorbar(frame.colour = "black", frame.linewidth = 0.4)) + # put a border line around the colorbar
  labs(title = "Social-Ecological Index Values of Hawaiian Fishing Communities", # fill = "Social-Ecological Index",
       x='Longitude', y='Latitude') +
  
  # label communities with highest indices--top 6
  # 1 Spreckelsville
  # 2 Hana 
  # 3 N Kona
  # 4 Lihue
  # 5 Honolulu
  # 6 S Kona
  
  # Honolulu, South Kona, North Kona, Hana, Spreckelsville, Lihue (order listed in HI_inds)
  geom_sf_label(data = subset(HI_inds, rank(-EQI) <= 6), # label top 6
                aes(label = NAME),# by ccd NAME
                nudge_x = c(0.03, -0.1,-0.05,0.1, 0.05,0.12), # scoot the labels horizontally so they are not overlapping important features
                nudge_y = c(-0.135, 0,0,-0.02, 0.15,0), # scoot vertically
                size = 3, color = "black", fill = alpha('white',0.4), 
                fontface = "bold") +
  
  geom_sf_label(data = state_fin,
                aes(label = NAMELSAD),
                size = 5, color = "black", 
                fill =  alpha(c("#8DA0CB","#E78AC3", "#66C2A5", "#FC8D62"),0.5), # labels are in order listed in state_fin
                # so fill is in order of Honolulu, Kauai, Maui, Hawaii
                #          color =  alpha(c("#8DA0CB","#E78AC3", "#66C2A5", "#FC8D62"),0.8), # labels are in order listed in state_fin
                nudge_x = c(0.03,-0.17,-0.18,0.16),
                nudge_y = c(0.45,-0.5,0.65,0.85),
                label.padding = unit(0.4, "lines"), # Adjust padding around the label
                label.r = unit(0.1, "lines"), # adjust roundness of box, lower is less round
                fontface = "bold") 
map_eqi

### adjust layout and aesthetics
map_eqi2<-map_eqi +
  theme(plot.title = element_text(hjust = 0.5, size = 22),              
        axis.title=element_text(size=22,face="plain"), #adjust size of axis titles
        axis.text=element_text(size=16, color = 'black'), #adjust font size of axis tick labels
        #   panel.background = element_rect(fill = "transparent", colour = 'black', size = 1.3),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.position = c(0.355, 0.19),  # manually adjust legend position
        legend.background = element_blank(), # need to set this otherwise it is opaque
        #  legend.box.background = element_rect( fill=alpha("white", 0)),
        # set legend border, fill, transparency
        legend.key = element_rect( colour = 'black'),# if not set, 
        #      legend.key.size = unit(0.8, 'cm'), # adjust size of ea fill box
        #     legend.spacing.y = unit(0.12, 'cm'), # space between fill boxes, need guides()
        legend.text=element_text(size=17), 
        legend.title=element_blank(),
        #    legend.margin = margin(5,12,8,10)
  ) +
  guides( color = "none")
map_eqi2

### add scalebar and north arrow
map_scale <-map_eqi2 +
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
    pad_x = unit(1, "in"), pad_y = unit(0.46, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      line_col = "grey20",
      fill = c("white", "grey40"),
      text_size = 15
    )
    #    style = ggspatial::north_arrow_nautical(
    #     fill = c("grey40", "white"),
    #    line_col = "grey20"
    # )
  )  +
  annotation_custom(
    grob = ggplotGrob(inset),
    xmin = -160.1,
    xmax = -158.7,
    ymin = 18.88,
    ymax = 19.88
  )

map_scale

ggsave('figures/DEA/MHI soc_eco_prod map.png', 
       width =  10, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')
