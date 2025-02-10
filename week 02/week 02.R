library(ggplot2)
library(showtext)
font_add_google("Teko")
showtext_auto()

# 1 - Load data ----

data <- read.csv("week 02/data.csv",row.names = NULL )

# 2 - Adding index to data

data$index <- nrow(data):1


# 3 - Plot data ----

ggplot(data, aes(x=index, y = Land)) +
  geom_col(
    fill = "#e02d47", 
    width = .5
  ) +
  geom_text(
    aes(
      x=index, y=0,
      label=Date
    ),
    family = "Teko",
    hjust= 1.1
  ) +
  geom_text(
    aes(
      x=index,
      y=Land/2,
      label=ifelse(Date %in% c(1874,1899), scales::comma(Land),"") 
    ),
    family = "Teko",
    size=6,
  ) +
  coord_flip() +
  labs(title = "ACRES OF LAND OWNED BY NEGROES\nIN GEORGIA.", 
       y = NULL,
       x = NULL
  ) +
  scale_y_continuous(labels = scales::comma) + 
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#e6d6c6ff"), 
    plot.title = element_text(
      family = "Teko",
      size = 24,
      hjust = 0.5,
      margin = margin(t = -10, b = -20) 
    ),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )
