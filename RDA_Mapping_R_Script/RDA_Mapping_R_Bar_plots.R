library(tidyverse)
library(grid)
library(shadowtext)
# load data ---------------------------------------------------------------------

setwd("C:/Users/fsdha/OneDrive - 11593765 Canada Association/Fares Drive/Works in progress/RDA/RDA Mapping/RDA_FF_mapping/RDA_Mapping_R_Data/RDA_Mapping_R_Data_input")
groups.path <- "C:/Users/fsdha/OneDrive - 11593765 Canada Association/Fares Drive/Works in progress/RDA/RDA Mapping/RDA_FF_mapping/RDA_Mapping_R_Data/RDA_Mapping_R_Data_input"

list <- list.files(path = groups.path)
list

#load data
data <- read.csv(list[1.], header = T)
topics <- read.csv(list[3.], header = T)

glimpse(data)
glimpse(topics)

data_for_analysis <- left_join(data, topics, by = "FF_TOPIC")
data_for_analysis_V1 <- data_for_analysis %>% select(Key, Count, Group_name) %>% 
  unique()



summary_table <- data_for_analysis_V1 %>%
  group_by(Key) %>% 
  summarise("n_groups" = sum(Count))

summary_table1 <- summary_table %>% as.tibble()

# PLOT --------------------------------------------------------------------


BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"


plt <-  ggplot(summary_table1) +
  geom_col(aes(n_groups, reorder(Key, -n_groups)), fill = BLUE, width = 0.6) 

plt <- plt + 
  scale_x_continuous(
    limits = c(0, 8.1),
    breaks = seq(0, 8, by = 1), 
    expand = c(0, 0), # The horizontal axis does not extend to either side
    position = "top"  # Labels are located on the top
  ) +
  # The vertical axis only extends upwards 
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 16)
  )
#Add labels
plt <- plt + 
  geom_shadowtext(
    data = subset(summary_table1, n_groups < 1),
    aes(n_groups, y = Key, label = Key),
    hjust = 0,
    nudge_x = 0.01,
    colour = RED,
    bg.colour = "white",
    bg.r = 0.2,
    family = "Econ Sans Cnd",
    size = 5.3,
    #fontface = "bold"
  ) +
  geom_text(
    data = subset(summary_table1, n_groups >= 1),
    aes(0, y = Key, label = Key),
    hjust = 0,
    vjust = 0.4,
    nudge_x = 0.01,
    colour = "white",
    family = "Ari",
    size = 5.3,
    #fontface = "bold"
  )
#Add annotations and final tweaks
plt <- plt +
  labs(
    title = "Funders Forums (FF) topics - keywords",
    subtitle = "Number of RDA WG-IG per FF keyword group"
  ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 22
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 20
    )
  )
plt


# Make room for annotations
plt <- plt + 
  theme(
    plot.margin = margin(0.05, 0, 0.04, 0.01, "npc")
  )

# Print the ggplot2 plot
plt

# Add horizontal line on top
# It goes from x = 0 (left) to x = 1 (right) on the very top of the chart (y = 1)
# You can think of 'gp' and 'gpar' as 'graphical parameters'.
# There we indicate the line color and width
grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = "firebrick4", lwd = 5)
)

# Add rectangle on top-left
# lwd = 0 means the rectangle does not have an outer line
# 'just' gives the horizontal and vertical justification
grid.rect(
  x = 0,
  y = 1,
  width = 0.05,
  height = 0.025,
  just = c("left", "top"),
  gp = gpar(fill = "green3", lwd = 0)
)

# We have two captions, so we use grid.text instead of 
# the caption provided by  ggplot2.
grid.text(
  "Notes: Keywords in red do not match any RDA group", 
  x = 0.01, 
  y = 0.005, 
  just = c("left", "bottom"),
  gp = gpar(
    col = RED,
    fontsize = 16,
    fontfamily = "Econ Sans Cnd"
  )
)
# grid.text(
#   "RDA-FF matching exercice", 
#   x = 0.005, 
#   y = 0.005, 
#   just = c("left", "bottom"),
#   gp = gpar(
#     col = GREY,
#     fontsize = 16,
#     fontfamily = "Milo TE W01"
#   )
# )

