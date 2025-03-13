library(grid)
library(dplyr)
library(ggplot2)
# 1 - Load Data ----

data <- read.csv("week 06/data.csv",row.names = NULL)

data_text <- data.frame(
  x = c(-.04,-.04,-.04,
        .155,.49,.826),
  y = c(1,.595,.19,
        -.01,-.01,-.01),
  label = c("1880","1890","1898",
            "1880","1890","1898"),
  size =c(1,1,1,
          1,1,1)
)

# 2 - Format Data ----

data$x <- rep(1:(nrow(data)/2)/3,2)

a <- (1 - 0.19) / (1 - 0.333)
b <- 1 - a * 1

data$x <- a * data$x + b

data$norm_Owners <- data$Owners / max(data$Owners)
data$norm_Property.Value..Dollars. <- data$Property.Value..Dollars. / max(data$Property.Value..Dollars.)


# 3 - Functions ----
# Adding curly braces 
# Function to create a curly bracket
# OBS.: Not working as expected
create_smooth_brace <- function(x1, y1, x2, y2, width = NULL, n_points = 100) {
  # Default width if not provided
  if(is.null(width)) {
    width <- abs(y2 - y1) * 0.25
  }
  
  # Create a parameter t that goes from 0 to 1
  t <- seq(0, 1, length.out = n_points)
  
  # Create Bézier curve segments for a curly brace
  # These approximate the SVG path with proper curves
  brace <- data.frame(x = numeric(0), y = numeric(0))
  
  # First curve: bottom to first bend
  p0x <- 2; p0y <- 0  # Start point
  p1x <- 2; p1y <- 1  # Control point 1
  p2x <- 3; p2y <- 2  # Control point 2
  p3x <- 4; p3y <- 2  # End point
  
  x_vals <- (1-t)^3*p0x + 3*(1-t)^2*t*p1x + 3*(1-t)*t^2*p2x + t^3*p3x
  y_vals <- (1-t)^3*p0y + 3*(1-t)^2*t*p1y + 3*(1-t)*t^2*p2y + t^3*p3y
  brace <- rbind(brace, data.frame(x = x_vals, y = y_vals))
  
  # Straight line segment
  brace <- rbind(brace, data.frame(x = c(4, 15), y = c(2, 2)))
  
  # Second curve: to middle point
  p0x <- 15; p0y <- 2  # Start point
  p1x <- 16; p1y <- 2  # Control point 1
  p2x <- 17; p2y <- 3  # Control point 2
  p3x <- 17; p3y <- 4  # End point
  
  x_vals <- (1-t)^3*p0x + 3*(1-t)^2*t*p1x + 3*(1-t)*t^2*p2x + t^3*p3x
  y_vals <- (1-t)^3*p0y + 3*(1-t)^2*t*p1y + 3*(1-t)*t^2*p2y + t^3*p3y
  brace <- rbind(brace, data.frame(x = x_vals, y = y_vals))
  
  # Third curve: middle to second bend
  p0x <- 17; p0y <- 4  # Start point
  p1x <- 17; p1y <- 3  # Control point 1
  p2x <- 18; p2y <- 2  # Control point 2
  p3x <- 19; p3y <- 2  # End point
  
  x_vals <- (1-t)^3*p0x + 3*(1-t)^2*t*p1x + 3*(1-t)*t^2*p2x + t^3*p3x
  y_vals <- (1-t)^3*p0y + 3*(1-t)^2*t*p1y + 3*(1-t)*t^2*p2y + t^3*p3y
  brace <- rbind(brace, data.frame(x = x_vals, y = y_vals))
  
  # Straight line segment
  brace <- rbind(brace, data.frame(x = c(19, 30), y = c(2, 2)))
  
  # Fourth curve: to end
  p0x <- 30; p0y <- 2  # Start point
  p1x <- 31; p1y <- 2  # Control point 1
  p2x <- 32; p2y <- 1  # Control point 2
  p3x <- 32; p3y <- 0  # End point
  
  x_vals <- (1-t)^3*p0x + 3*(1-t)^2*t*p1x + 3*(1-t)*t^2*p2x + t^3*p3x
  y_vals <- (1-t)^3*p0y + 3*(1-t)^2*t*p1y + 3*(1-t)*t^2*p2y + t^3*p3y
  brace <- rbind(brace, data.frame(x = x_vals, y = y_vals))
  
  # Normalize coordinates (original width = 32, height = 4)
  brace$x <- brace$x / 32
  brace$y <- brace$y / 4
  
  # Transform to target position and size
  vertical <- abs(x2 - x1) < abs(y2 - y1)
  
  if (vertical) {
    # For vertical brace
    height <- abs(y2 - y1)
    if (y2 > y1) {
      # Pointing up
      result <- data.frame(
        x = x1 + brace$y * width,
        y = y1 + brace$x * height
      )
    } else {
      # Pointing down
      result <- data.frame(
        x = x1 + (1 - brace$y) * width,
        y = y1 + (1 - brace$x) * height
      )
    }
  } else {
    # For horizontal brace
    width_actual <- abs(x2 - x1)
    if (x2 > x1) {
      # Pointing right
      result <- data.frame(
        x = x1 + brace$x * width_actual,
        y = y1 + brace$y * width
      )
    } else {
      # Pointing left
      result <- data.frame(
        x = x1 + (1 - brace$x) * width_actual,
        y = y1 + brace$y * width
      )
    }
  }
  
  return(result)
}

# 4 - Creating Bars ----
# Note: I'm using geom_polygon so it is Easier to get
# the overlaying effect on the correct order
polygon_data <- data.frame()
bar_width <- 0.01
for (i in 1:nrow(data)) {
  city <- data$City[i]
  year <- data$Year[i]
  # Normalize the values to be between 0 and 1
  owners <- data$Owners[i] / max(data$Owners)
  x_pos <- rev(data$x)[i]
  
  # Add offset based on city for the "dodge" effect
  x_offset <- ifelse(city == "Atlanta", -bar_width, bar_width)
  
  # Create 4 points for each rectangle
  points <- data.frame(
    City = rep(city, 4),
    Year = rep(year, 4),
    Owners = rep(owners, 4),
    x = c(x_pos + x_offset - bar_width, x_pos + x_offset + bar_width, 
          x_pos + x_offset + bar_width, x_pos + x_offset - bar_width),
    y = c(0, 0, owners, owners),
    id = rep(i, 4)
  )
  
  polygon_data <- rbind(polygon_data, points)
}

polygon_data_vertical <- data.frame()

x_shift <- .825
for (i in 1:nrow(data)) {
  city <- data$City[i]
  year <- data$Year[i]
  # Normalize the values to be between 0 and 1
  Property.Value..Dollars. <- data$Property.Value..Dollars.[i] / max(data$Property.Value..Dollars.) 
  x_pos <- rev(data$x)[i]*x_shift
  
  # Add offset based on city for the "dodge" effect
  x_offset <- ifelse(city == "Atlanta", -bar_width, bar_width)
  
  # Create 4 points for each rectangle
  points <- data.frame(
    City = rep(city, 4),
    Year = rep(year, 4),
    Property.Value..Dollars. = rep(Property.Value..Dollars., 4),
    x = c(x_pos + x_offset - bar_width, x_pos + x_offset + bar_width, 
          x_pos + x_offset + bar_width, x_pos + x_offset - bar_width),
    y = c(0, 0, Property.Value..Dollars., Property.Value..Dollars.),
    id = rep(i, 4)
  )
  
  polygon_data_vertical <- rbind(polygon_data_vertical, points)
}

# Create the plot with geom_polygon
ggplot() +
  
  geom_polygon(data = polygon_data, 
               aes(x = y, y = x, fill = City, group = id),
               color = "black", size = 0.1) +
  geom_polygon(data = polygon_data_vertical,
               aes(x = rev(x), y = y*1.018, fill = City, group = id),
               color = "black", size = 0.1) +
  
  geom_polygon(data = polygon_data %>% filter(Year == '1898'), 
               aes(x = y, y = x, fill = City, group = id),
               color = "black", size = 0.1) +

  geom_polygon(data = polygon_data_vertical %>% filter(Year == '1880'),
               aes(x = rev(x)-.668, y = y*1.018, fill = City, group = id),
               color = "black", size = 0.1) +  
  
  geom_text(data = data, 
            aes(x = rev(norm_Owners)/2 ,
                y = x + ifelse("Atlanta" == City, bar_width, -bar_width),
                label = rev(Owners)),
            hjust = "left", vjust = 0.5, size = 2) +
  geom_text(data = data, 
            aes(y = (norm_Property.Value..Dollars.)/2 ,
                x = x* x_shift + ifelse("Atlanta" == City, bar_width, -bar_width),
                label = rev(scales::dollar_format()(Property.Value..Dollars.))),
            hjust = -.26, vjust = 0.5, size = 2,angle = 90) +
  
  # scale_fill_manual(values = c("#9aa0b6", "#f0b73b")) +
  scale_fill_manual(
    values = c("Savannah" = "#f0b73b", "Atlanta" = "#9aa0b6"),
    name = NULL,  # no legend title
    guide = guide_legend(
      direction = "horizontal",
      label.position = "left",  # text on the left, squares on the right
      label.hjust = 1
    )
  ) +
  theme_minimal() +
  labs(x = "Year", y = "Owners")  + 
  geom_path(data = create_smooth_brace(x1=-.020, y1=.155, x2=-.020, y2=.08), aes(x, y),linewidth = .1) +
  geom_path(data = create_smooth_brace(x1=-.020, y1=.155+.41, x2=-.020, y2=.1+.4), aes(x, y),linewidth = .1) +
  geom_path(data = create_smooth_brace(x1=-.020, y1=.155+.82, x2=-.020, y2=.1+.82), aes(x, y),linewidth = .1) +
  geom_path(data = create_smooth_brace(x1=-.10, y1=.95, x2=-.10, y2=0), aes(x*.5-.08, y-.805),linewidth = .1) +
  geom_path(data = create_smooth_brace(x1=.0, y1=0.80, x2=.84, y2=0.0), aes(x+.045, (.8-y)*.25),linewidth = .1) +
  geom_text(data = data_text, aes(x = x, y = y, label = label), size = 2.5) + 
  geom_text(aes(x=c(-.19,.5) , y=c(.59,-.09) , label = c("OWNERS.","PROPERTY"))) +
  labs(title = "NEGRO PROPERTY IN TWO CITIES\nOF GEORGIA") +
  theme(
    plot.background = element_rect(fill = "#e6d6c6ff"), 
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(
      family = "Teko",
      size = 24,
      hjust = 0.5,
      margin = margin(t = 20, b = 20) 
    ),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.position = "bottom",
    legend.direction = "horizontal"
    # legend.position = c(0.5, -0.1),     
    # legend.justification = c(0.5, -.5),
  )
  

ggsave("week 06/week 06.png",width = 960, height = 1200, units = 'px',dpi=96*1.15)
