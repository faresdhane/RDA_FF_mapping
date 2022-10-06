library(tidyverse)
library(tidytext)


# Prepare the data --------------------------------------------------------



setwd("D:/Documents/LDP/Productivity and Reproducibility/Git/RDA_FF_mapping/RDA_Mapping_R_Data/RDA_Mapping_R_Data_input")# set text as tibble
groups <- read.csv("RDA_Groups_R_V1_20220930.csv", header = T) %>% select(-Charter, - URL)

glimpse(groups)

# Tokenization and data tidying
group_words <- groups %>%  
  unnest_tokens(Text, words) %>% 
  count(Group, Text, sort = TRUE)

# Frequency
total_words <- group_words %>% 
  group_by(Group) %>% 
  summarize(total = sum(n))

group_words <- left_join(group_words, total_words)

book_tf_idf <- group_words %>%
  bind_tf_idf(Text, Group, n)



# delete words with tf-idf near zero = words that occur in many of the groups
book_tf_idf1 <- book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))


to.delete.words <- book_tf_idf1 %>% filter(tf_idf < 0.001, n > 4) %>% select(-tf, -idf)
view(to.delete.words)

to.delete.words <- to.delete.words %>% select(Text) %>% unique()

to.delete.words.string <- to.delete.words %>%
  mutate(string.length = str_length(to.delete.words$Text)) %>% 
  filter(string.length < 6) # select words with less than 6 letters
view(to.delete.words.string )

to.delete.words.string <- to.delete.words.string %>% filter(Text != "tools")



book_tf_idf2 <- book_tf_idf1 %>% arrange(Group, tf_idf) %>%
  group_by(Group) %>% top_n(10)

# write.csv(book_tf_idf2, "RDA_Groups_top5_words_Input1_V1.csv")



# Topics-Group merging ----------------------------------------------------


topics <- read.csv("RDA_Topics_R_V1_20220930.csv", header = T)

setwd("C:/Users/fsdha/Desktop/RDA_Mapping_R_analysis/RDA_Mapping_R_Data/RDA_Mapping_R_Data_output")
group2 <- read.csv("RDA_Groups_top5_words_Input1_V1.csv", header = T)

glimpse(topics)
glimpse(group2)

merge_1 <- inner_join(topics, group2, by = c("Keyword1" = "Text"))
merge_2 <- inner_join(topics, group2, by = c("Keyword2" = "Text"))

final_merge <- bind_rows(merge_1, merge_2) %>% unique() %>% select(FF_TOPIC,
                                                                   Group,
                                                                   Keyword1,
                                                                   Keyword2) %>% 
  drop_na()

glimpse(final_merge)

not_matching <- anti_join(topics, final_merge, by = "FF_TOPIC")

# write.csv(final_merge, "matching.csv")
# write.csv(not_matching, "not_matching.csv")


unmatched_groups <- anti_join(group2, final_merge, by = "Group")

# write.csv(unmatched_groups, "unmatched_groups.csv")

# Some examples - with graphs ---------------------------------------------


example1 <- book_tf_idf1 %>% filter(Group == "FAIR Digital Object Fabric IG")
example1 <- example1 %>% filter(Text != "ig")
example1 <- example1 %>% filter(tf_idf > 0)
# view(example1)

example2 <- book_tf_idf1 %>% filter(Group == "FAIR Digital Object Fabric IG")
example2 <- example2 %>% filter(tf_idf > 0)
# view(example2)

# Plot the words
example1 %>%
  group_by(Group) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(n, fct_reorder(Text, n), fill = Group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Group, ncol = 2, scales = "free") +
  labs(x = "The number of times a word has been used", y = NULL)+
  theme(axis.text = element_text(size =14),
        axis.title = element_text(size = 15))


example2 %>%
  group_by(Group) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(Text, tf_idf), fill = Group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Group, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

