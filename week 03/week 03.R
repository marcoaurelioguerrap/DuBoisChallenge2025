library(dplyr)
library(ggplot2)
library(purrr)
library(spdep)
library(showtext)
library(sf)
library(sysfonts)
font_add_google("Teko")
font_add_google("Caveat")
font_add_google("Vesper Libre")
showtext_auto()

# 1 - Load Data ----

data <- read.csv("week 03/data.csv",row.names = NULL )

georgia_county_1880 <- st_read("week 03/georgia-1880-county-shapefile/DuBoisChallenge - Georgia Counties w 1870 & 1880 data.shp")

# 2 - Joining Data table with shapefile ----

data$County1890 <- tolower(data$County1890)

georgia_county_1880$name <- tolower(georgia_county_1880$NHGISNAM) 

georgia_county_1880 <- left_join(georgia_county_1880, data, by = c("name" = "County1890"))

nb <- poly2nb(georgia_county_1880)

# Assign colors using a greedy graph coloring algorithm
assign_colors <- function(nb, colors) {
  n <- length(nb)
  color_assignment <- rep(NA, n)
  
  for (i in seq_len(n)) {
    # Get colors of neighbors
    neighbor_colors <- color_assignment[nb[[i]]]
    
    # Choose a color that is not used by neighbors
    available_colors <- setdiff(colors, neighbor_colors)
    
    # Assign the first available color
    color_assignment[i] <- sample(available_colors, 1)
  }
  
  return(color_assignment)
}

# 3 - Color scale ----

# color_scale <- c("#000000","#654321","#d2b48c","#ffd700","#ffc0cb","#dc143c","#4682b4","#7e6583")
color_scale <- c("#c52f44","#455d56","#e0d1be","#e3aa1c","#8a8992",
                 "#bc8576","#b9997c","#ada8ad","#dd9c95","#63847b")


set.seed(002)  # Ensure reproducibility
georgia_county_1880$random_color <- assign_colors(nb, color_scale)

# 4 - Plot data ----

crs <- "+proj=omerc +lonc=90 +lat_0=40 +gamma=185 +alpha=0"  

georgia_county_1880 %>%
  st_transform(crs) %>%
  mutate(centroid.x = map_dbl(geometry, ~ st_centroid(.x)[[1]]),
         centroid.y = map_dbl(geometry, ~ st_centroid(.x)[[2]])) %>% #View()
  # mutate(random_color = sample(color_scale, n(), replace = TRUE)) %>%
ggplot() +
  geom_sf( aes(fill = random_color), color = "black") +
  geom_text(aes(x = centroid.x, y = centroid.y, label = Acres.1899), family = "Caveat", size = 3) +
  geom_text(x = -259300, y = -11726005, label = "THE FIGURES INDICATE THE NUMBER OF\nACRES OWNED IN EACH COUNTRY IN 1899",
            size = 3,                   
            fontface = "plain",         
            family = "Arial",        
            check_overlap = TRUE ) +
  scale_fill_identity() +
  coord_sf(crs=crs) +
  labs(title = "LAND OWNED BY NEGROES IN GEORGIA, U.S.A 1870-1900.", 
       fill = "Land (acres)",
       x = NULL,
       y = NULL
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#e6d6c6ff"), 
    plot.title = element_text(
      family = "Teko",
      size = 24,
      hjust = 0.5,
      margin = margin(t = -10, b = 20) 
    ),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

ggsave("week 03/week 03.png",width = 700, height = 800, units = 'px',dpi=96)
