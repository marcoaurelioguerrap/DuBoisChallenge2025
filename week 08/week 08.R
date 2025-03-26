library(dplyr)
library(tidyr)
library(ggplot2)

# 1 - Load Data -----
data <- read.csv("./week 08/data.csv") %>%
  mutate(rank = nrow(data) - row_number())

# 2 - Format data -----
polygon_data <- data %>%
  mutate(rank = row_number()) %>%
  group_by(Occupation)

bar_width <- 0.2
same_bar_dist <- 0.1


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


# with semi circle ----
create_stretched_semicircle <- function(point1, point2, n_points = 20, 
                                        x_stretch = 2, orientation = "below") {
  # point1 and point2 should be vectors of the form c(x, y)
  # x_stretch controls the stretching factor in the x direction
  # orientation can be "below" or "above" to determine the direction of the arc
  
  # Calculate the midpoint between the two points
  midpoint <- c(
    (point1[1] + point2[1]) / 2,
    (point1[2] + point2[2]) / 2
  )
  
  # Calculate the distance between the points
  width <- sqrt((point2[1] - point1[1])^2 + (point2[2] - point1[2])^2)
  
  # Calculate the height of the semi-circle
  radius <- width / 2
  
  # Calculate the angle of the line between the points
  angle <- atan2(point2[2] - point1[2], point2[1] - point1[1])
  
  # Generate points for the stretched semi-circle
  result <- tibble(x = numeric(n_points), y = numeric(n_points))
  
  for (i in 1:n_points) {
    # Angle parameter from 0 to pi
    t <- (i - 1) * pi / (n_points - 1)
    
    # Determine sign for orientation
    orientation_factor <- ifelse(orientation == "below", -1, 1)
    
    # Calculate point on a unit semi-circle, but stretch x by the factor
    circle_x <- cos(t) * radius * x_stretch  # Apply x_stretch here
    circle_y <- orientation_factor * sin(t) * radius
    
    # Rotate the point
    rotated_x <- circle_x * cos(angle) - circle_y * sin(angle)
    rotated_y <- circle_x * sin(angle) + circle_y * cos(angle)
    
    # Translate to midpoint
    result$x[i] <- midpoint[1] + rotated_x
    result$y[i] <- midpoint[2] + rotated_y
  }
  
  return(result)
}


bar_width <- 0.2
same_bar_dist <- 0.1
cut_value <- 63012

polygon_data <- data %>%
  mutate(rank = nrow(data) - row_number()) %>%
  group_by(Occupation) %>%
  # First get the basic information
  mutate(
    needs_wrap = Count > cut_value,
    wrap_points = purrr::map(needs_wrap, function(wrap) {
      if(wrap) {
        
        outer_curve_points <- create_semicircle(c(cut_value - 1500,-bar_width/2 - same_bar_dist - bar_width), 
                                                c(cut_value - 1500,bar_width/2),orientation="below",n_points=100,y_stretch = 4500)
        inner_curve_points <- create_semicircle(c(cut_value - 1500,-bar_width/2),
                                                c(cut_value - 1500,-bar_width/2 - same_bar_dist), 
                                                orientation="above",y_stretch = 4500)
        
        tibble(
          x = c(0, outer_curve_points$x ,
                Count-cut_value, Count-cut_value,
                inner_curve_points$x ,0),
          y_offset = c(bar_width/2, outer_curve_points$y,
                       -bar_width/2 - same_bar_dist - bar_width,-bar_width/2 - same_bar_dist,
                       inner_curve_points$y,-bar_width/2),
          point_order = 1:124
        )
      } else {
        # For regular counts, create a simple rectangle
        tibble(
          x = c(0, Count, Count, 0),
          y_offset = c(bar_width/2, bar_width/2, -bar_width/2, -bar_width/2),
          point_order = 1:4
        )
      }
    })
  ) %>%
  unnest(wrap_points) %>%
  mutate(
    y = rank + y_offset
  ) %>%
  arrange(Occupation, point_order) %>%
  select(Occupation, x, y)

shift_x_bracket_polygon <- 11000
bracket_polygon <- data[8:nrow(data),] %>% 
  summarise(rank = mean(rank),Count=sum(Count)) %>%
  mutate(
    wrap_points = list(tibble(
      x = c(shift_x_bracket_polygon, Count + shift_x_bracket_polygon, Count +shift_x_bracket_polygon, shift_x_bracket_polygon),
      y_offset = c(bar_width/2, bar_width/2, -bar_width/2, -bar_width/2),
      point_order = 1:4
    ))
  
  ) %>%
  unnest(wrap_points) %>%
  mutate(
    y = rank + y_offset
  )

# 4 - Plot -----
ggplot(polygon_data) +
  geom_polygon(aes(x = x, y = y, group = Occupation),fill = '#d93d51', color = "black",linewidth = .1) +
  geom_text(data = data,
            aes(x = -11000, y = rank, label = stringr::str_wrap(Occupation, 13) ),
            hjust = "center", size = 3,
            lineheight = .75) +
  geom_text(data = data,
            aes(x = -2800, y = rank, label = scales::comma(Count) ),
            hjust = "center", size = 3,
            lineheight = .75) +
  geom_path(data = create_smooth_brace(x1=500, y1=-1.5, x2=500, y2=14.5), aes(x + (x-500)*2400, y),linewidth = .1) +
  geom_text(aes(x=26000,y=11,label= "1890."),size=10,family = "Teko",) +
  geom_polygon(data = bracket_polygon, 
               aes(x = x, y = y),
               fill = '#d93d51', color = "black",linewidth = .1) +
  # scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Occupation Count",
    x = "Count",
    y = "Occupation"
  ) +
  labs(title = "OCCUPATIONS OF GEORGIA NEGROES",subtitle = "MALES OVER 10.") +
  theme(
    plot.background = element_rect(fill = "#e6d6c6ff"), 
    plot.title = element_text(
      family = "Teko",
      size = 24,
      hjust = 0.5,
      margin = margin(t = 20, b = 0) 
    ),
    plot.subtitle = element_text(
      family = "Teko",
      size = 12,
      hjust = 0.5,
      margin = margin(t = 0, b = 0) 
    ),
    plot.margin = margin(t = 00, r = 20, b = 0, l = 40),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
    # legend.position = c(0.5, -0.1),     
    # legend.justification = c(0.5, -.5),
  ) + coord_cartesian(clip="off")
  

ggsave("week 08/week 08.png",width = 960, height = 1200, units = 'px',dpi=96*1.02)
