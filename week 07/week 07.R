library(dplyr)
library(ggplot2)

# 1 - Load data ----
data <- read.csv("./week 07/data.csv",row.names = NULL)

# 2 - Format data ----
# NOTE: if you use the linear transformation from column "Houshold.Value..Dollars." to std_values,
# the bars final position will not coincide with the original plot. 
data <- data %>%
  mutate(std_values = ((Houshold.Value..Dollars.)) / (max(Houshold.Value..Dollars.) ) )

# 3 - Functions ----
spiral_creator <- function(percentage=1, which_level,bar_width){
  start_t <- 8.6
  end_t <- 20.42
  # t <- seq(8.5, 20.42, by=0.01)
  # t <- seq(8.5, 20.42 - ((1-percentage)*(20.42-8.5)) , by=0.01)
  t <- seq(start_t + ((1-percentage)*(end_t-start_t)), end_t, by=0.001)
  correction  <- 1 + which_level*bar_width / t
  correction2 <- 1 + (which_level+1)*bar_width / t 
  x_outer <- t*cos(t) 
  y_outer <- t*sin(t)
  
  # Create an inner boundary with slightly smaller radius
  x_inner <- rev(t) * cos(rev(t))
  y_inner <- rev(t) * sin(rev(t))
  
  # Combine the outer and inner boundaries to form a closed polygon
  x <- c(x_outer * (correction2), x_inner  * rev(correction))
  y <- c(y_outer * (correction2), y_inner  * rev(correction))
  
  return(data.frame(x=x, y=y))
}

# 4 - Plot ----
ggplot() +
  # geom_polygon(fill="lightblue", color="black") +
  # geom_polygon(aes(x=x2,y=y2),fill="lightblue1", color="black") +
  geom_polygon(data=spiral_creator(data$std_values[1],6,.9),
               aes(x=x,y=y),fill="#eeb3ab", color="black",size =.1) +
  geom_polygon(data=spiral_creator(data$std_values[2],5,.9),
               aes(x=x,y=y),fill="#a2a7b5", color="black",size =.1) +
  geom_polygon(data=spiral_creator(data$std_values[3],4,.9),
               aes(x=x,y=y),fill="#c0a283", color="black",size =.1) +
  geom_polygon(data=spiral_creator(data$std_values[4],3,.9), 
               aes(x=x,y=y),fill="#f8b43d", color="black",size =.1) +
  geom_polygon(data=spiral_creator(data$std_values[5],2,.9),
               aes(x=x,y=y),fill="#d8cab8", color="black",size =.1) +
  geom_polygon(data=spiral_creator(data$std_values[6],1,.9),
               aes(x=x,y=y),fill="#e03a4f", color="black",size =.1) +
  
  geom_text(data=data %>% arrange(desc(Year)), 
            aes(x=-8,y=20.5+(1:6),label= paste0(Year,"---",scales::dollar_format()(Houshold.Value..Dollars.)) ),
            hjust = "left")+
    
  coord_equal() +
  labs(title = "ASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE\nOWNED BY GEORGIA NEGROES") +
  theme(
    plot.background = element_rect(fill = "#e6d6c6ff"), 
    plot.title = element_text(
      family = "Teko",
      size = 24,
      hjust = 0.5,
      margin = margin(t = 20, b = 20) 
    ),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
    # legend.position = c(0.5, -0.1),     
    # legend.justification = c(0.5, -.5),
  )


ggsave("week 07/week 07.png",width = 1000, height = 1200, units = 'px',dpi=96)



# ####
