library(dplyr)
library(ggplot2)
library(showtext)
library(sysfonts)
font_add_google("Teko")
font_add_google("Caveat")
font_add_google("Vesper Libre")
showtext_auto()

# 1 - Load Data ----

data <- read.csv("week 05/data.csv",row.names = NULL) %>% unique()

# 2 - Format data ----
data <- data %>% 
  mutate(Year = factor(Year, levels = rev(sort(unique(Year))))) %>%
  # mutate(Year = as.factor(Year)) %>%
  arrange(Year) 

# - ----
color_pallet <- c("#c41232", "#cfb7a0", "#e6ab0e", "#1e346e", "#b7927c", "#121212")


generate_stalactite <- function(x0, y0, width, height, n_points = 20, jitter_amount = 0.01) {
    # Generate left edge: from apex (x0,y0) to left base corner (x0 - width/2, y0 + height)
    left_edge <- data.frame(
      x = seq(x0, x0 - width/2, length.out = n_points),
      y = seq(y0, y0 + height, length.out = n_points)
    )
    if(n_points > 2) {
      left_edge$x[2:(n_points-1)] <- left_edge$x[2:(n_points-1)] +
        runif(n_points-2, -jitter_amount*width, jitter_amount*width)
      left_edge$y[2:(n_points-1)] <- left_edge$y[2:(n_points-1)] +
        runif(n_points-2, -jitter_amount*height, jitter_amount*height)
    }
    
    # Generate right edge: from apex (x0,y0) to right base corner (x0 + width/2, y0 + height)
    right_edge <- data.frame(
      x = seq(x0, x0 + width/2, length.out = n_points),
      y = seq(y0, y0 + height, length.out = n_points)
    )
    if(n_points > 2) {
      right_edge$x[2:(n_points-1)] <- right_edge$x[2:(n_points-1)] +
        runif(n_points-2, -jitter_amount*width, jitter_amount*width)
      right_edge$y[2:(n_points-1)] <- right_edge$y[2:(n_points-1)] +
        runif(n_points-2, -jitter_amount*height, jitter_amount*height)
    }
    
    # Build the polygon: 
    # Start at the apex, go down the left edge to the base, then along the base to the right,
    # then back up the right edge to the apex.
    polygon_df <- rbind(
      left_edge,
      right_edge[seq(n_points, 2), ]  # exclude the apex on right edge, since it's the same as left edge's first point
    )
    
    polygon_df
}
generate_stalactite <- function(x0, y0, width, height, n_points = 20, jitter_amount = 0.01, convexity = 0.1) {
  # Generate left edge: from apex (x0,y0) to left base corner (x0 - width/2, y0 + height)
  left_edge <- data.frame(
    x = seq(x0, x0 - width/2, length.out = n_points),
    y = seq(y0, y0 + height, length.out = n_points)
  )
  
  # Add convexity to left edge
  if(n_points > 2) {
    # Create a parabolic curve for convexity
    mid_point <- floor(n_points / 2)
    convex_offset <- seq(0, convexity * width, length.out = mid_point)
    convex_offset <- c(convex_offset, rev(convex_offset)[-1])
    
    left_edge$x[2:(n_points-1)] <- left_edge$x[2:(n_points-1)] + 
      convex_offset[2:(n_points-1)] +
      runif(n_points-2, -jitter_amount*width, jitter_amount*width)
    
    left_edge$y[2:(n_points-1)] <- left_edge$y[2:(n_points-1)] +
      runif(n_points-2, -jitter_amount*height, jitter_amount*height)
  }
  
  # Generate right edge: from apex (x0,y0) to right base corner (x0 + width/2, y0 + height)
  right_edge <- data.frame(
    x = seq(x0, x0 + width/2, length.out = n_points),
    y = seq(y0, y0 + height, length.out = n_points)
  )
  
  # Add convexity to right edge
  if(n_points > 2) {
    # Create a parabolic curve for convexity
    mid_point <- floor(n_points / 2)
    convex_offset <- seq(0, convexity * width, length.out = mid_point)
    convex_offset <- c(convex_offset, rev(convex_offset)[-1])
    
    right_edge$x[2:(n_points-1)] <- right_edge$x[2:(n_points-1)] - 
      convex_offset[2:(n_points-1)] +
      runif(n_points-2, -jitter_amount*width, jitter_amount*width)
    
    right_edge$y[2:(n_points-1)] <- right_edge$y[2:(n_points-1)] +
      runif(n_points-2, -jitter_amount*height, jitter_amount*height)
  }
  
  # Build the polygon: 
  # Start at the apex, go down the left edge to the base, then along the base to the right,
  # then back up the right edge to the apex.
  polygon_df <- rbind(
    left_edge,
    right_edge[seq(n_points, 2), ]  # exclude the apex on right edge, since it's the same as left edge's first point
  )
  
  polygon_df
}

# grey_stalactite <- data %>% filter(Year == 1875)

positions <- c( .6 , .75, .9 , 1.15, 1.39)
# stalactite_df <- generate_stalactite(x0 = .60,
#                                      y0 = df_stalactite$Valuation..Dollars./2,
#                                      width = .03, 
#                                      height = df_stalactite$Valuation..Dollars./2,
#                                      convexity = -0.2,
#                                      n_points = 50, jitter_amount = 0.04)
stalactite_list <- list()
i <- 1
df_stalactite <- data %>% filter(Year == 1875)

for (pos in positions){
  stalactite_list[[i]] <- list(data=generate_stalactite(x0 = pos,
                                                       y0 = df_stalactite$Valuation..Dollars./2,
                                                       width = .03, 
                                                       height = df_stalactite$Valuation..Dollars./2,
                                                       convexity = -0.2,
                                                       n_points = 50, jitter_amount = 0.04),
                                color = "#b7927c")
  
  i <- i + 1
}

df_stalactite <- data %>% filter(Year == 1880)

for (pos in positions[-4]){
  stalactite_list[[i]] <- list(data=generate_stalactite(x0 = pos,
                                       y0 = df_stalactite$Valuation..Dollars./2,
                                       width = .025, 
                                       height = df_stalactite$Valuation..Dollars./2,
                                       convexity = -0.2,
                                       n_points = 50, jitter_amount = 0.04),
                                color = "#1e346e")
  
  i <- i + 1
}

df_stalactite <- data %>% filter(Year == 1885)

for (pos in positions[-c(3,4)]){
  stalactite_list[[i]] <- list(data=generate_stalactite(x0 = pos,
                                                        y0 = df_stalactite$Valuation..Dollars./2*.8,
                                                        width = .0225, 
                                                        height = df_stalactite$Valuation..Dollars./2*1.2,
                                                        convexity = -0.2,
                                                        n_points = 50, jitter_amount = 0.04),
                               color = "#e6ab0e")
                               # color = "black")
  
  i <- i + 1
}

df_stalactite <- data %>% filter(Year == 1890)

for (pos in positions[-c(2,3,4)]){
  stalactite_list[[i]] <- list(data=generate_stalactite(x0 = pos,
                                                        y0 = df_stalactite$Valuation..Dollars./2*.8,
                                                        width = .0225, 
                                                        height = df_stalactite$Valuation..Dollars./2*1.2,
                                                        convexity = -0.2,
                                                        n_points = 50, jitter_amount = 0.04),
                               color = "#cfb7a0")
                               # color = "black")
  
  i <- i + 1
}

df_stalactite <- data %>% filter(Year == 1899)

for (pos in positions[-c(1,2,3,4)]){
  stalactite_list[[i]] <- list(data=generate_stalactite(x0 = pos,
                                                        y0 = df_stalactite$Valuation..Dollars./2*.8,
                                                        width = .0225, 
                                                        height = df_stalactite$Valuation..Dollars./2*1.2,
                                                        convexity = -0.2,
                                                        n_points = 50, jitter_amount = 0.04),
                               color = "#c41232")
                               # color = "black")
  
  i <- i + 1
}

gg <- ggplot(data,aes(x="", y=Valuation..Dollars., fill=Year)) +
  # geom_bar(stat="identity", width=1, color="white") +
  geom_bar(stat = "identity", position = "identity", alpha = 1, width = 1) +
  # coord_polar("x", start=0) +
  scale_fill_manual(values = color_pallet) +
  theme_void() + 
  theme(legend.position="none") 
  # geom_polygon(data = stalactite_df, aes(x = x, y = y),
  #              fill = "#b7927c") 


for (stalactite in stalactite_list){
  gg <- gg + geom_polygon(data = stalactite$data, aes(x = x, y = y),
                          fill = stalactite$color) 
}
gg + geom_text(data=data,
               aes(x = 1, y = Valuation..Dollars., label = Year,
                   color=c("black","black","black","white","black","white")),
               fontface = "plain",         
               family = "Arial",
               size = 3,vjust=-.15) +
  geom_text(data=data,
              aes(x = c(1.388,.603,.753,.903,1.143,1),
                  y = ifelse(Valuation..Dollars.==5393885,0,Valuation..Dollars./1.2),
                  angle = c(-52,52,0,-52,40,0),
                  hjust = c("right","center","right","right","center","center"),
                  # y = Valuation..Dollars.,
                  label = scales::dollar(Valuation..Dollars.),
                  color=c("black","black","black","white","black","white")),
              fontface = "plain",         
              family = "Arial",
              size = 3,vjust=-.15) +
  scale_color_manual(values = c("white" = "white", "black" = "black")) +
  coord_polar("x", start=0) +
  labs(title = "ASSESSED VALUATION OF ALL TAXABLE PROPERTY\nOWNED BY GEORGIA NEGROES")

# Function to generate a rough, inverted triangle (“stalactite”) polygon.
# The apex (the pointed tip) will be at (x0, y0), and the base of the triangle is at y0 + height.
generate_stalactite <- function(x0, y0, width, height, n_points = 20, jitter_amount = 0.01) {
  # Generate left edge: from apex (x0,y0) to left base corner (x0 - width/2, y0 + height)
  left_edge <- data.frame(
    x = seq(x0, x0 - width/2, length.out = n_points),
    y = seq(y0, y0 + height, length.out = n_points)
  )
  if(n_points > 2) {
    left_edge$x[2:(n_points-1)] <- left_edge$x[2:(n_points-1)] +
      runif(n_points-2, -jitter_amount*width, jitter_amount*width)
    left_edge$y[2:(n_points-1)] <- left_edge$y[2:(n_points-1)] +
      runif(n_points-2, -jitter_amount*height, jitter_amount*height)
  }
  
  # Generate right edge: from apex (x0,y0) to right base corner (x0 + width/2, y0 + height)
  right_edge <- data.frame(
    x = seq(x0, x0 + width/2, length.out = n_points),
    y = seq(y0, y0 + height, length.out = n_points)
  )
  if(n_points > 2) {
    right_edge$x[2:(n_points-1)] <- right_edge$x[2:(n_points-1)] +
      runif(n_points-2, -jitter_amount*width, jitter_amount*width)
    right_edge$y[2:(n_points-1)] <- right_edge$y[2:(n_points-1)] +
      runif(n_points-2, -jitter_amount*height, jitter_amount*height)
  }
  
  # Build the polygon: 
  # Start at the apex, go down the left edge to the base, then along the base to the right,
  # then back up the right edge to the apex.
  polygon_df <- rbind(
    left_edge,
    right_edge[seq(n_points, 2), ]  # exclude the apex on right edge, since it's the same as left edge's first point
  )
  
  polygon_df
}

