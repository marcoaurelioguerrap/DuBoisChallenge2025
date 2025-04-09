library(dplyr)
library(ggplot2)

# 1 - Load the data -----
data <- read.csv("./week 10/data.csv")

data$width <- 0.5

first_bar <- data.frame(Class="first_bar",
                        Actual.Average=0,
                        Rent=20,
                        Food=20,
                        Clothes=20,
                        Tax=20,
                        Other=20,
                        width=.2)

data <- rbind(first_bar,data)

# 

library(tidyr)
library(ggplot2)

data_long <- tidyr::pivot_longer(data, 
                          cols = c(Rent, Food, Clothes, Tax, Other),
                          names_to = "Category", 
                          values_to = "Percentage")  

# Convert Class to a factor with levels in the correct order
data_long$Class <- factor(data_long$Class, 
                          levels = rev(c("$100-200", "$200-300", "$300-400", "$400-500", 
                                     "$500-750", "$750-1000", "Over $1000")))

data_long$Category <- factor(data_long$Category, 
                          levels =  rev(c("Rent", "Food", "Clothes", "Tax", "Other")))

data_long <- data_long %>% 
  group_by(Class) %>%
  arrange(Class, desc(Category)) %>%
  mutate(pos = cumsum(Percentage) - 0.5 * Percentage)

data_long <- data_long %>% 
  mutate(labels=Category) %>%
  mutate(labels = ifelse(is.na(Class),as.character(labels),(paste0(Percentage,"%")))) %>%
  mutate(labels = ifelse(labels=="0%",NA,labels))

color <- rev(c("#11110f", "#755778", "#d09b87", "#a79f9e", "#c7b8a2"))

data_table <- data.frame(x=c(0.5,0.5,7.5,7.5),
                         y=c(5,-50,-50,5))

labels_col <- data.frame(y = c(-40,-15.5),x=7.8,label=c("CLASS.","ACTUAL AVERAGE."),size=3)

rent_line <- data.frame(y=c(20,
                            19,19,
                            22,22,
                            23,23,
                            18,18,
                            13,13,
                            0),
                        x=c(8,
                            7.25,6.75,
                            6.25,5.75,
                            5.25,4.75,
                            4.25,3.75,
                            3.25,2.75,
                            2.25))
food_line <- data.frame(y=c(40,
                            62,62,
                            69,69,
                            66,66,
                            55,55,
                            44,44,
                            37,37,
                            29,29),
                        x=c(7.9,
                            7.25,6.75,
                            6.25,5.75,
                            5.25,4.75,
                            4.25,3.75,
                            3.25,2.75,
                            2.25,1.75,
                            1.25,1.25))
clothes_line <- data.frame(y=c(60,
                            90,90,
                            92,92,
                            84,84,
                            70,70,
                            61,61,
                            56,56,
                            45,45),
                        x=c(7.9,
                            7.25,6.75,
                            6.25,5.75,
                            5.25,4.75,
                            4.25,3.75,
                            3.25,2.75,
                            2.25,1.75,
                            1.25,1.25))
tax_line <- data.frame(y=c(80,
                            99.9,99.9,
                            96,96,
                            88.5,88.5,
                            75.5,75.5,
                            66,66,
                            64,64,
                            49.5,49.5),
                        x=c(7.9,
                            7.25,6.75,
                            6.25,5.75,
                            5.25,4.75,
                            4.25,3.75,
                            3.25,2.75,
                            2.25,1.75,
                            1.25,1.25))


# Create the stacked bar chart
ggplot(data_long, aes(x = Class, y = Percentage)) +
  geom_bar(stat = "identity",aes(fill = Category,width = width)) +
  # geom_text(aes(y = pos, label = ifelse(Percentage==0,NA,Percentage)), 
  geom_text(aes(y = pos, label = labels,
                color= ifelse(Category == 'Rent',"white","black")), 
            # color = "white",
            size = 4) +
  geom_text(aes(x=Class,y=-20,label=ifelse(Actual.Average==0,NA,paste0("$",Actual.Average))),
            hjust='left')+
  geom_text(aes(x=Class,y=-45,label=Class),
            hjust='left')+
  geom_text(data = labels_col,aes(x,y,label=label)) +
  geom_path(data=data_table,aes(x=x,y=y),linewidth = .1) +
  geom_path(data=rent_line,aes(x=x,y=y),linewidth = .1) +
  geom_path(data=food_line,aes(x=x,y=y),linewidth = .1) +
  geom_path(data=clothes_line,aes(x=x,y=y),linewidth = .1) +
  geom_path(data=tax_line,aes(x=x,y=y),linewidth = .1) +
  labs(title = "Expense Distribution by Income Class",
       x = "Income Class",
       y = "Percentage") +
  scale_fill_manual(values = color) +
  scale_color_manual(values = c("black", "white")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(
          family = "Teko",
          size = 24,
          hjust = 0.5,
          margin = margin(t = 20, b = 0)
        ),
        plot.margin = margin(0, 0, 0, 0),
        # Remove margins
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#e6d6c6ff", color = NA),
        # plot.margin = margin(t = 20, r = 20, b = 20, l = 50),
        legend.position = "none" ) +
  # expand_limits(x = c(0, 10)) +
  # scale_fill_brewer(palette = "Set2") + 
  coord_flip(clip = 'off')
