library(dplyr)
library(ggplot2)

# 1 - Load Data ----
data <- read.csv("./week 09/data.csv")


# 2 - Plot ----

occupation_colors <- c(
  "Agriculture, Fisheries and Mining" = "#D13B40",
  "Manufacturing and Mechanical Industries" = "#4F68B0",
  "Domestic and Personal Service" = "#F9C430",

  "Trade and Transportation" = "#E6DED2",
  "Professions" = "#A89885"
)

# Create position and y-axis values for each group
negroes_data <- data %>%
  filter(Group == "Negroes") %>%
  arrange((match(Occupation, names(occupation_colors)))) %>%
  mutate(
    end_angle = cumsum(Percentage),
    start_angle = lag(end_angle, default = 0),
    mid_angle = (start_angle + end_angle) / 2,
    hjust = ifelse(mid_angle > 45, 1, 0),
    vjust = 0.5,
    y = 1
  )

whites_data <- data %>%
  filter(Group == "Whites") %>%
  arrange((match(Occupation, names(occupation_colors)))) %>%
  mutate(
    end_angle = cumsum(Percentage),
    start_angle = lag(end_angle, default = 0),
    mid_angle = (start_angle + end_angle) / 2,
    hjust = ifelse(mid_angle > 45, 1, 0),
    vjust = 0.5,
    y = -1
  )

# Combine data
plot_data <- rbind(negroes_data, whites_data)

# Create sector/fan plot
ggplot() +
  # Create fan sectors for each occupation group
  geom_rect(
    data = plot_data,
    # aes(xmin = start_angle, xmax = end_angle,
    #     ymin = ifelse(Group == "Negroes", 0, -2),
    #     ymax = ifelse(Group == "Negroes", 2, 0),
    aes(
      xmin = ifelse(Group == "Negroes", start_angle, start_angle + 150),
      xmax = ifelse(Group == "Negroes", end_angle, end_angle + 150),
      ymin = 0,
      ymax = 2,
      fill = Occupation
    ), color = 'black' , linewidth = .1
  ) +
  geom_text(
    data = plot_data %>% filter(Percentage >= 4),
    aes(
      x = ifelse(
        Group == "Negroes",
        (start_angle + end_angle) / 2,
        (start_angle + end_angle) / 2 + 150
      ),
      y = 1.75,
      label = paste0(Percentage, "%")
    ),
    size = 4
  ) +
  # scale_fill_manual(values = occupation_colors) +
  # Add group labels
  annotate("text", x = 50, y = 2.1, label = "NEGROES", size = 5) +
  annotate("text", x = 200, y = 2.1, label = "WHITES", size = 5) +

  # Add occupation legend - manual placement to match original
  annotate(
    "point",
    x = 280,
    y = 1.8,
    fill = occupation_colors["Agriculture, Fisheries and Mining"],
    size = 8,
    shape = 21,
    color = 'black',
    stroke = .2
  ) +
  annotate(
    "text",
    x = 280,
    y = 1.7,
    label = "AGRICULTURE, FISHERIES\nAND MINING",
    hjust = 0,
    size = 3
  ) +

  annotate(
    "point",
    x = 270,
    y = 1.8,
    fill = occupation_colors["Manufacturing and Mechanical Industries"],
    size = 8,
    shape = 21,
    color = 'black',
    stroke = .2
  ) +
  annotate(
    "text",
    x = 270,
    y = 1.7,
    label = "MANUFACTURING AND\nMECHANICAL INDUSTRIES",
    hjust = 0,
    size = 3
  ) +

  annotate(
    "point",
    x = 110,
    y = 1.8,
    fill = occupation_colors["Domestic and Personal Service"],
    size = 8,
    shape = 21,
    color = 'black',
    stroke = .2
  ) +
  annotate(
    "text",
    x = 110,
    y = 1.7,
    label = "DOMESTIC AND\nPERSONAL SERVICE",
    hjust = 1,
    size = 3
  ) +

  annotate(
    "point",
    x = 122,
    y = 1.8,
    fill = occupation_colors["Professions"],
    size = 8,
    shape = 21,
    color = 'black',
    stroke = .2
  ) +
  annotate(
    "text",
    x = 122,
    y = 1.7,
    label = "PROFESSIONS",
    hjust = 1,
    size = 3
  ) +

  annotate(
    "point",
    x = 135,
    y = 1.8,
    fill = occupation_colors["Trade and Transportation"],
    size = 8,
    shape = 21,
    color = 'black',
    stroke = .2
  ) +
  annotate(
    "text",
    x = 135,
    y = 1.7,
    label = "TRADE AND\nTRANSPORTATION",
    hjust = 1,
    size = 3
  ) +

  # Set the sector colors
  scale_fill_manual(values = occupation_colors) +
  # scale_x_continuous(limits = c(0, 100)) +
  # scale_y_continuous(limits = c(-6, 3)) +

  # Add title
  ggtitle("OCCUPATIONS OF NEGROES AND WHITES IN GEORGIA.") +

  # Theme customization
  theme_void() +
  theme(
    plot.title = element_text(
      family = "Teko",
      size = 24,
      hjust = 0.5,
      margin = margin(t = 20, b = 0)
    ),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0),
    # Remove margins

    plot.background = element_rect(fill = "#e6d6c6ff", color = NA)
  ) +
  xlim(c(0, 300)) +
  coord_polar(start = -1)

ggsave(
  "week 09/week 09.png",
  width = 960,
  height = 1200,
  units = 'px',
  dpi = 96 * 1.02
)
