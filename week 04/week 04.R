library(dplyr)
library(ggplot2)
library(showtext)
library(sysfonts)
font_add_google("Teko")
font_add_google("Caveat")
font_add_google("Vesper Libre")
showtext_auto()

# 1 - Load Data ----

data <- read.csv("week 04/data.csv",row.names = NULL) 

# 2 - Data transformation ----

spline_int <- as.data.frame(spline(data$Year, data$Property.Valuation))

annotations <- data.frame(
  x = c(1875, 1880,1892,1897,1872,1894),
  y = c(2300000, 4000000, 1500000,2300000,500000,400000),
  label = c("POLTICAL\n        UNREST.",
            "RISE OF\n        THE NEW\n                INDUSTRIALISM.",
            "LYNCHING.",
            "DISFRANCHSMENT\nAND\nPROSCAPTIVE\nLAWS.",
            "KU-KLUXISM",
            "FINANCIAL PANIC."),
  hjust = c("left","left","left","center","left","left"),
  angle = c(0,0,0,0,90,90)
  )

# 3 - Plot data ----
gg1 <- ggplot(data) +
  geom_point(aes(x=Year,y=Property.Valuation)) +
  geom_line(data = spline_int, aes(x = x, y = y),linewidth = 3, lineend = "square") +
  geom_line(data = spline_int,
            aes(x = x, y = y,color = ifelse((x <= 1874.8) | (x >= 1899.0) ,"white","transparent"),group=1),
            linewidth = 2.5, lineend = "square") +
  scale_color_manual(values = c("white" = "#dfcfbf", "transparent" = "transparent")) +
  scale_x_continuous(breaks = seq(1870, 1900, by = 5),
                     minor_breaks = seq(1870, 1900, by = 1),
                     expand=c(0,0)) +
  scale_y_continuous(breaks= seq(0, 4900000, by = 100000),
                     expand=c(0,0),limits = c(0,4900000)) +
  # geom_text(aes(x = 1877, y = 2300000, label = "Political\n        Unrest"), 
  geom_label(data = annotations,
            aes(x = x, y = y, label = label,hjust = hjust,angle=angle), 
            fill='#dfcfbf',
            # hjust = 'left',
            fontface = "plain",         
            # family = "Arial",        
            check_overlap = TRUE, size = 3,
            # label.r = unit(0.0, "lines"),
            label.size = 0.0,
            ) +
  # labs(title = "VALUATION OF TOWN AND CITY PROPERTY OWNED\nBY GEORGIA NEGROES.") + 
  theme(
    legend.position = "none",
    # Axis tick 
    panel.grid.major.x = element_line(color = "red", size = 0.1),
    panel.grid.minor.x = element_line(color = "red", size = 0.1),
    panel.grid.major.y = element_line(color = "red", size = 0.1),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(fill = NA, color="black"),
    panel.background = element_rect(fill="#dfcfbf"),
    plot.background = element_rect(fill="#dfcfbf",color="#dfcfbf"),
    plot.margin = margin(t = 0, r = 20, b = 20, l = 0),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
  



data_axis <- data.frame(
  x=rep(0,16),
  y=c(0,
      400000,600000,1000000,
      1400000,1600000,2000000,
      2400000,2600000,3000000,
      3400000,3600000,4000000,
      4400000,4600000,4800000),
  label=c("","$","$","1,000,00",
      "$","$","2,000,00",
      "$","$","3,000,00",
      "$","$","4,000,00",
      "$","$","DOLLARS")
)

lines_out_plot <- data.frame(
  x=c(rep(0,5),rep(10,5)),
  y=rep(seq(0,4900000, by = 1000000),2),
  group=c(rep(1,5),rep(1,5))
)[-1,]

gg2 <- ggplot(data_axis) +
  geom_text(aes(x=x,y=y,label=label)) +
  
  scale_y_continuous(breaks= seq(0,4900000, by = 100000),
                     expand=c(0,0),limits = c(0,4900000)) +
  theme(
    legend.position = "none",
    # Axis tick 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "red", size = 0.1),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(fill = NA, color="black"),
    panel.background = element_rect(fill="#dfcfbf"),
    plot.background = element_rect(fill="#dfcfbf",color = "#dfcfbf"),
    plot.margin = margin(t = 20, r = 0, b = 20, l = 20),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust=.5)
  ) 


gg3 <- ggplot() +
  geom_line(data=lines_out_plot,
            aes(x=x,y=y,group=y),color='red',linewidth=.1) +
  scale_y_continuous(breaks= seq(0,4900000, by = 100000),
                     expand=c(0,0),limits = c(0,4900000)) +
  theme(
    legend.position = "none",
    # Axis tick 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(fill = NA, color=NA),
    panel.background = element_rect(fill="#dfcfbf"),
    plot.background = element_rect(fill="#dfcfbf",color = "#dfcfbf"),
    plot.margin = margin(t = 0, r = 00, b = 20, l = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust=.5)
  ) 

# merge plots 
library(patchwork)

gg2 + gg3 + gg1 + 
  plot_annotation(title = "VALUATION OF TOWN AND CITY PROPERTY OWNED\nBY GEORGIA NEGROES.",
                  theme = theme(
                    plot.background = element_rect(fill = "#dfcfbf"),
                    plot.title = element_text(hjust=.5)
                  )) + 
  plot_layout(widths = c(1,1,5),design = NULL)


ggsave("week 04/week 04.png",width = 800, height = 1000, units = 'px',dpi=96*0.75)
