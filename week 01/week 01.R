library(ggplot2)
library(ggimage)
library(showtext)
font_add_google("Teko")
showtext_auto()

# 1 - Load data -----

data <- read.csv("day 01/data.csv")

img <- "day 01/money_bag.png"

# 2 - Standardize Land value column -----
data$std.Land.Value..Dollars. <- (data$Land.Value..Dollars. - min(data$Land.Value..Dollars.)) / (max(data$Land.Value..Dollars.) - min(data$Land.Value..Dollars.))

# 3 - Plot -----
g <- ggplot() + 
  geom_image(
    aes(
      x = rep(1, 6),
      y = 6:1,
      size = I(data$std.Land.Value..Dollars./24 + .1)  # Normalize size values
    ),
    image = img
  ) + 
  geom_text(
    aes(
      x = rep(1, 6),
      y = 6:1,
      label = paste0("$",scales::comma(data$Land.Value..Dollars.))
    ),
    hjust = .5,
    vjust = 1.5,
    family = "Teko"
  )  +
  geom_text(
    aes(
      x = rep(1, 6),
      y = 6:1,
      label = data$Year
    ),
    hjust = .5,
    vjust = 6.5,
    family = "Teko",
  )  +
  scale_y_continuous(expand = c(.1, 0)) +
  theme_void() +
  labs(title = "Value of Land Owned by Georgia Negroes") +
  theme(
    plot.background = element_rect(fill = "#e6d6c6ff"),  # Light gray background
    plot.title = element_text(
      family = "Teko",
      size = 24,
      hjust = 0.5,
      margin = margin(t = 20, b = 20)  # Add some margin to the title
    ),
    # Add some padding around the plot
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )


ggsave("week 01/week 01.png",width = 700, height = 800, units = 'px')
