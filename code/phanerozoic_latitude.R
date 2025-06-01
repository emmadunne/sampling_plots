# ******************************************************
#
#   Sampling plots
#
#   E. M. Dunne (28-05-2025)
# ______________________________________________________
#
#   Plot showing latitude of PBDB collections 
#       across the Phanerozoic for terrestrial tetrapods
#
# ******************************************************


## This script uses occurrence data from the PBDB that has been thoroughly cleaned
## to remove trace terms, ootaxa, marine taxa, flying taxa, taxonomically indeterminate 
## occurrences, wastebasket taxa, and implausibly old/young occurrences following the
## procedure of Close et al. (2019). A new columns was also created for "mid_ma".
## Link to paper: https://www.nature.com/articles/s41559-019-0811-8
## Link to Dryad repoistory: https://datadryad.org/stash/dataset/doi:10.5061/dryad.3v0p84v


## Install and then load the following packages:
library(tidyverse) # data manipulation and plotting
install.packages("scales")
library(scales)



# Load and organise data --------------------------------------------------

## Load cleaned dataset from Close et al. (2019):
tetrapoda <- read_csv("./data/Close_2019_cleaned.csv")

## Take only the columns we need for the plot and remove any NAs
tetrapoda <- dplyr::select(tetrapoda, 
                           collection_name, collection_no, 
                           paleolat, paleolng, ma_mid) %>% distinct()
tetrapoda <- na.omit(tetrapoda) # omit NAs

## Make a vector of interval boundaries to mark on the plot using their ages in Ma
int_boundaries <- c(358.9, 298.9, 252.1, 237, 201.3, 145, 66, 23.03, 0)

## Create the plot in ggplot
lat_plot <- ggplot() +
  # Add a shaded rectangle to highlight a particular interval, in this case the Late Triassic based on its age in Ma:
  #geom_rect(aes(xmin = 201.3, xmax = 237, ymin=-90, ymax=90), alpha = 0.5, fill = "#DEB7FE") +
  # Add vertical lines for the interval boundaries using the vector from above:
  geom_vline(xintercept = int_boundaries, lty = 2, col = "grey90") +
  # Add a horizontal lone for the equator:
  geom_hline(yintercept = 0, colour = "grey10") +
  # Reverse the x-axis and choose which ages appear on it
  scale_x_reverse(limits = c(400, 0), breaks = int_boundaries, labels = function(x) ifelse(x == 0, "0", x)) + 
  # And the same for the y-axis (palaeolatitude):
  scale_y_continuous(labels = function(x) format(x, width = 5), limits = c(-90,90), breaks = seq(from = -90, to = 90, by = 15)) +
  # Set the theme to 'minimal' and make some adjustments to the legend etc.:
  theme_minimal() + theme(legend.position.inside = c(0.2, 0.85), legend.direction = "vertical", 
                          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
                          panel.grid.minor.y = element_blank(), axis.title = element_text(size = 14)) +
  # Add text to the axes:
  labs(x = "", y = "Palaeolatitude (º)") +
  # Finally, add the points for the occurrences: 
  geom_point(data = tetrapoda, aes(x = ma_mid, y = paleolat), alpha = 0.3, size = 2, colour = "#8D74A0")
lat_plot # call the plot to the plot window

ggsave("./plots/phanerozoic_latitude.pdf", lat_plot,  
       height = 13, width = 20, units = "cm")


## Geological timeline can be added later in Adobe Illustrator etc.
## Or you can try out the deeptime package: https://github.com/willgearty/deeptime

