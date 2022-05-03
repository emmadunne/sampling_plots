# ******************************************************
#
#   Sampling plots
#
#   E. M. Dunne (03-05-2022)
# ______________________________________________________
#
#   Palaeomap displaying species richness
#
# ******************************************************


## Install and then load the following packages:
library(tidyverse) # data manipulation and plotting
library(chronosphere) # for getting the maps - for citation use: citation("chronosphere") 
library(ggpubr) # for arranging the maps into panels


## Load a dataset of *cleaned* occurrences from the PBDB
occ_data <- read_csv("./data/PBDB_tetrapods_LateTriassic_03022020.csv")

## Create a frequency table to record how many occurences are in each locality
freq_table <- as.data.frame(table(occ_data[, c('collection_name','accepted_name')]$collection_name))
## Rename the columns for easier plotting later:
freq_table <- rename(freq_table, collection_name = Var1, richness = Freq)
## Arrange the table in descending order:
freq_table <- arrange(freq_table, richness)
## Take a look:
head(freq_table)

## Next, grab the useful information for each locality from occ_data:
locality_info <- dplyr::select(occ_data, collection_name, paleolat, paleolng) %>% distinct(collection_name, .keep_all = TRUE) %>% na.omit()

## Add this useful info to the frequency table to create the dataset we'll use for the map:
map_data <- left_join(freq_table, locality_info, by = "collection_name") %>% na.omit()
head(map_data) 


## Next, grab a map for your time interval of choice by using its age in Ma
## Here, for the Late Triassic, I've chosen 215Ma which is roughly the midpoint
palaeomap_LT <- reconstruct("plates", age = 215) # takes a little bit of time to run!

## Take a quick look:
par(mar = c(2, 2, 2, 2)) # set plot margins
plot(palaeomap_LT, col="gray") # we can make this look prettier later!
## Looks good!


## Set a theme so the maps are consistent (this is very minimal):
palaeomap_theme <- theme_minimal() + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
                                           axis.title.y=element_blank(), axis.text.y=element_blank(),
                                           axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                                           legend.title=element_blank())

## Build the map in ggplot:
palaeomap_plot <-  ggplot() +
  ## Grab your palaeomap:
  geom_polygon(data = palaeomap_LT, aes(x = long, y = lat, group = group), fill = "grey75", color = "grey75") +
  # Use the Mollweide projection:
  coord_map("mollweide") +
  # Add the localities:
  geom_point(data = map_data, aes(x = paleolng, y = paleolat, colour = richness), size = 5,  alpha = 0.8) +
  # Set the colour gradient using ONE of the following "scale_colour" functions:
  # 1. scale_colour_viridis for cotinuous data:
  scale_colour_viridis_c(option = "plasma") +
  # 2. Custom colour gradient:
  #scale_color_gradient2(low = "cyan3", mid = "darkorchid3", high = "violetred3", midpoint = 7, breaks = seq(1, 23, 5)) + 
  # Set the horizontal lines on the map:
  scale_y_continuous(breaks = seq(from = -90, to = 90, by = 30), limits = c(-90,90)) + 
  # Set the vertical lines on the map:
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 30), limits = c(-180,180)) +
  # Finally, add the theme from above:
  palaeomap_theme
palaeomap_plot # pull the map up in the plot window


## Save the plot file:
ggsave(plot = palaeomap_plot,
       width = 17, height = 12, dpi = 600, units = "cm",
       filename = "./plots/palaeomap_richness.pdf")


## Further edits to the plot can be done in Adobe Ilustrator etc.
