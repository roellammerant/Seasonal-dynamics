#Libraries
library(raster)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(ggfx)

# 1. Load  Copernicus long term monthly temperature 1987-2023 data
sst_Lev <- stack("./cmems_mod_med_phy-temp_my_4.2km_P1M-m_1778495209674.nc")
check=stars::read_ncdf("./cmems_mod_med_phy-temp_my_4.2km_P1M-m_1778495209674.nc") %>% 
  as_tibble()
# ---------------------------------------------------------
# --- RASTER MATH: Calculating Long-Term Summer Average ---
# ---------------------------------------------------------

# Extract just the year from the layer names (e.g., "X1987.01.01" becomes "1987")
layer_years <- substr(names(sst_Lev), 2, 5)

# Calculate the maximum temperature per pixel for EACH year
# stackApply groups the layers by our 'layer_years' index and applies the 'max' function
annual_max_sst <- stackApply(sst_Lev, indices = layer_years, fun = max, na.rm = TRUE)

# Calculate the long-term mean of those annual maximums
# calc applies the 'mean' function across all the yearly max layers
long_term_summer_sst <- calc(annual_max_sst, fun = mean, na.rm = TRUE)

# Extrapolate data to fill coastal gaps to improve eugly edge effects
# Create a 3x3 moving window to calculate the mean of neighboring cells.
# NAonly = TRUE  ensures we only fill missing cells at the edge 
# and do not alter  actual valid temperature data.
long_term_summer_sst_filled <- focal(long_term_summer_sst, 
                                     w = matrix(1, nrow = 3, ncol = 3), 
                                     fun = mean, 
                                     NAonly = TRUE, 
                                     na.rm = TRUE)
# ---------------------------------------------------------
# --- PREPARING FOR ggploy --------------------------------
# ---------------------------------------------------------

# Convert the resulting filled raster layer to a dataframe for ggplot
sst_summer_df <- raster::as.data.frame(long_term_summer_sst_filled, xy = TRUE) %>%
  na.omit() 

# Change the variable name to 'sst'
colnames(sst_summer_df)[3] <- "sst"

# Load land shapefile
land <- ne_countries(scale = "large", returnclass = "sf")

# Set color scale
mycol <- c("darkblue", "cyan4", "chartreuse3", "yellow", "red", "red4")

# 1. Points dataframe without country names ---
sites_df <- data.frame(
  Location = c("IS", "CY_North", "CY_East", "CY_South", "GR_Crete"),
  Lat = c(31.875, 35.375, 34.975, 34.575, 35.175),
  Lon = c(34.625, 33.275, 34.125, 32.925, 24.375)
)

# 2. Country labels dataframe
# Coordinates are approximated to landmass centers within map bounds
countries_df <- data.frame(
  Country = c("Greece", "Israel", "Cyprus"),
  Lat = c(39.5, 31.2, 35.0),
  Lon = c(22.0, 35.2, 33.2)
)

# Set color scale
mycol <- c("darkblue", "cyan4", "chartreuse3", "yellow", "red", "red4")

# ---------------------------------------------------------
# --- PLOTTING --------------------------------------------
# ---------------------------------------------------------
map=ggplot() +
  # 1. Plot the summarized AND filled SST data
  geom_raster(data = sst_summer_df, aes(x = x, y = y, fill = sst), interpolate = TRUE) +
  
  # 2. Overlay the elegant land mask
  with_shadow(
    geom_sf(data = land, fill = "#E6E2DB", color = "#8A8782", linewidth = 0.2),
    sigma = 5,        # Blur radius of the shadow
    x_offset = 2,     # Shift right
    y_offset = -2,    # Shift down
    color = "black", 
    alpha = 0.4       # Transparency of the shadow
  ) +  
  
  # 3. Add the sampling points
  geom_point(data = sites_df, aes(x = Lon, y = Lat), 
             shape = 21, fill = "white", color = "black", size = 2.5, stroke = 1) +
  
  # 4. --- UPDATED: Add Country Labels with ggrepel ---
  geom_text_repel(data = countries_df, aes(x = Lon, y = Lat, label = Country),
                  size = 4.5, fontface = "bold", color = "#1C1F24", alpha = 0.9,
                  #min.segment.length = 0,    # Forces a line to always be drawn if the label is pushed
                  #box.padding = 1.75,         # Pushes the label further away from the center coordinate
                  #segment.color = "#1C1F24",  # Makes the leader line a subtle grey
                  seed = 42,
                  bg.color = "white", bg.r = 0.08,) +
  
  # 5. Add the specific locality labels (using ggrepel)
  geom_text_repel(data = sites_df, aes(x = Lon, y = Lat, label = Location),
                  size = 3.5, fontface = "bold", 
                  bg.color = "white", bg.r = 0.08,
                  min.segment.length = 0,    # Forces a line to always be drawn if the label is pushed
                  box.padding = 1,         # Pushes the label further away from the center coordinate
                  segment.color = "#1C1F24",  # Makes the leader line a subtle grey
                  seed = 42) + 
  
  # 6. Add Sea Label 
  annotate("text", x = 28.0, y = 33.5, label = "Eastern Mediterranean Sea", 
           color = "black", size = 5.5, fontface = "italic", alpha = 0.8) +
  
  # Set the bounding box for the Eastern Med
  coord_sf(xlim = c(19, 36.5), 
           ylim = c(30, 41), 
           expand = FALSE) +
  
  scale_fill_gradientn(colours = mycol, guide = "colourbar", name = "SST (°C)") +
  
  labs(x = "Longitude (°)", 
       y = "Latitude (°)") +
  
  theme(text = element_text(size = 11),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
        axis.line = element_blank(),
        
        # Legend Placement
        legend.position = c(0.97, 0.97), 
        legend.justification = c(1, 1), 
        
        # This removes the white box and outline:
        legend.background = element_blank(),
        
        # ensure no background behind the colorbar itself
        legend.key = element_blank(),
        
        legend.margin = margin(t = 4, r = 6, b = 4, l = 6))
map

# Save as a PDF (Vector format - best for the final manuscript submission)
ggsave(plot = map,"map.pdf", width = 8, height = 6, units = "in", bg = "white")

# Save as a high-resolution PNG (600 DPI - excellent for presentations and web viewing)
ggsave(plot = map,"map.png", width = 8, height = 6, units = "in", dpi = 300, bg = "white")

