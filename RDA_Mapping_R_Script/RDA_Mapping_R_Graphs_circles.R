library(tidyverse)
library(circlepackeR)   
library(hrbrthemes)
library(collapsibleTree) 
library(data.tree)
library(htmlwidgets)
library(RColorBrewer)

setwd("C:/Users/fsdha/Desktop/RDA_mapping/RDA_Mapping_R_analysis/RDA_Mapping_R_Data/RDA_Mapping_R_Data_input")
groups.path <- "C:/Users/fsdha/Desktop/RDA_mapping/RDA_Mapping_R_analysis/RDA_Mapping_R_Data/RDA_Mapping_R_Data_input"

list <- list.files(path = groups.path)
list

#Load the data
FF_TOPIC <- read.csv(list[3.], header = T) %>% select(-Keyword1, -Keyword2)
glimpse(FF_TOPIC)

setwd('C:/Users/fsdha/Desktop/RDA_mapping/RDA_Mapping_R_analysis/RDA_Mapping_R_Data/RDA_Mapping_R_Data_output')

matching_groups <- read.csv("RDA_matched_groups_topics_Input1_V1.csv", header = T) 
glimpse(matching_groups)

data <- inner_join(FF_TOPIC, matching_groups, by = c("FF_TOPIC" = "TOPIC")) %>% unique()
# view(data)

# sorted_FF_TOPIC <- data %>% group_by(FF_CLUSTER_THEME, SCOPE, FOCUS, FF_TOPIC, Group) %>% 
#   count()
sorted_FF_TOPIC <- data %>% group_by(FF_CLUSTER_THEME,FF_SCOPE,
                                     FF_FOCUS,
                                     FF_TOPIC, Group) %>% 
  count()

sorted_FF_TOPIC$pathString <- paste("FF_TOPIC",
                                  sorted_FF_TOPIC$FF_CLUSTER_THEME,
                                  sorted_FF_TOPIC$FF_TOPIC,
                                  sorted_FF_TOPIC$FF_SCOPE,
                                  sorted_FF_TOPIC$FF_FOCUS,
                                  sorted_FF_TOPIC$Group,
                                  sep = "/")

FF_TOPIC.arranged <- as.Node(sorted_FF_TOPIC)

p1 <- circlepackeR(FF_TOPIC.arranged,
             size = "n",
             color_min = "hsl(152,80%,80%)",
             color_max = "hsl(228, 30%, 40%)",
             width = 1920,
             height = 1080)

p1
# save the widget
# saveWidget(p1, file=paste0( getwd(), "/Hdendrogram_interactive.html"))

##########################################
glimpse(data)
data <- as.data.frame(data)

sorted_FF_TOPIC <- data


sorted_FF_TOPIC$pathString <- paste("FF_TOPIC",
                                  sorted_FF_TOPIC$FF_CLUSTER_THEME,
                                  sorted_FF_TOPIC$SCOPE,
                                  sorted_FF_TOPIC$FOCUS,
                                  sorted_FF_TOPIC$FF_TOPIC,
                                  sorted_FF_TOPIC$Group,
                                  sep = "/")

sorted_FF_TOPIC$pathString <- paste("FF_TOPIC",
                                  sorted_FF_TOPIC$FF_CLUSTER_THEME,
                                  sorted_FF_TOPIC$SCOPE,
                                  sorted_FF_TOPIC$FOCUS,
                                  sorted_FF_TOPIC$FF_TOPIC,
                                  sorted_FF_TOPIC$Group,
                                  sep = "/")


FF_TOPIC.arranged <- as.Node(sorted_FF_TOPIC)

collapsibleTree(FF_TOPIC.arranged,
                hierarchy_attribute = "level",
                root = FF_TOPIC.arranged$name,
                inputId = NULL,
                attribute = "leafCount",
                linkLength = NULL,
                fontSize = 15,
                tooltip = TRUE,
                tooltipHtml = NULL,
                nodeSize = "leafCount",
                collapsed = TRUE,
                zoomable = TRUE,
                fillByLevel = TRUE,
                width = 1920,
                height = 1080,
)
#####################################

data2 <- as.tibble(data)

# p2 <- data2 %>% group_by(FF_CLUSTER_THEME, SCOPE, FOCUS, FF_TOPIC, Group) %>% 
#   summarise("number of groups" = n()) %>% 
#   collapsibleTreeSummary(
#     hierarchy = c("FF_CLUSTER_THEME",
#                   "SCOPE",
#                   "FOCUS",
#                   "FF_TOPIC",
#                   "Group"),
#     root = "FF FF_TOPIC & groups",
#     attribute = "number of groups",
#     nodeSize = "leafCount",
#     fontSize = 25,
#     width = 3800,
#     height = 1200,
#     zoomable = TRUE,
#     collapsed = TRUE
#     
#   )

p2 <- data2 %>% group_by(FF_CLUSTER_THEME, FF_TOPIC,
                         FF_SCOPE, FF_FOCUS, Group) %>% 
  summarise("n" = n()) %>% 
  collapsibleTreeSummary(
    hierarchy = c("FF_CLUSTER_THEME",
                  "FF_SCOPE",
                  "FF_FOCUS",
                  "FF_TOPIC",
                  "Group"),
    root = "FF TOPIC & groups",
    attribute = "n",
    nodeSize = "leafCount",
    fontSize = 25,
    width = 4400,
    height = 1200,
    zoomable = TRUE,
    collapsed = TRUE
    
  )

p2

# save the widget

# saveWidget(p2, file=paste0( getwd(), "RDA_Data_for_visualization_V1_20220930.html"))
