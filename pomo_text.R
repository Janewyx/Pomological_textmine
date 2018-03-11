## Code for first text processing attempt for a seminar class blog post on geospatial projects in governments.
## Only sample texts from one blog post is displayed below for due to blog privacy. The code reads in pre-processed 
## .csv text documents extracted from all blog posts and visualisations are based on all results. 

# devtools::install_github("gadenbuie/ggpomological")

library(tidyverse) ## for cleaning data
library(tokenizers) ## for word extraction
library(reshape2) 
library(ggpomological) ## cool theme
library(magick) ## for saving image
library(extrafont)

loadfonts(device = "win")

## sample blog post for the topic "functions of geospatial analysis in governments worldwide"
functions <- paste("Locally, the Crown Land of British Columbia is owned by the provincial government. The government is responsible for managing land resources and collects tax on usages including but not limited to industrial, agricultural, and recreational activities. Geospatial analysis serves the purpose of resource management which integrates multiple landuse interests (economic, conservative, cultural etc). On a wider extent, recognising the wide applications of geospatial products, analyses help inform planning, decision making, and improvements of living conditions of a region. For example, meteorological stations use radar data for forecasting; public transportations are scheduled based on programmatically modelled demands; and visual air quality analyses inform industrial-related decision making processes.
                   Overall, geospatial analysis is essential for strategic planning and policy-making for governments around the world. This further illustrates the importance of open data and methods, workflow reproducibility and public involvement in government science.")

## extracting words from the paragraph
functions_text <- tokenize_words(functions)

## putting these words into a dataframe and doing a word frequency count
functions_df <- table(functions_text)
functions_df <- data_frame(word = names(functions_df), count = as.numeric(functions_df))
functions_df <- arrange(functions_df, desc(count))

## outputting and reading in all dataframes, with each representing a particular topic. 
## manual word picking was done in the .csv to take out meaningless terms including "I", "and" etc. 
# write.csv(functions_df, "functions.csv", row.names = FALSE)
functions_df <- read.csv("data/functions.csv")
improve_df <- read.csv("data/improve.csv")
transp_df <- read.csv("data/transparency.csv")

## combining dataframes by common terms among the three dataframes, and doing a total count
combined_2 <- inner_join(improve_df, transp_df, by = "word")
combined <- inner_join(combined_2, functions_df, by = "word")

combined <- combined %>% 
  mutate(sum = (count.x + count.y + count))

names(combined_2) <- c("word", "Improvement", "Transparency")
names(combined) <- c("word", "Improvement", "Transparency", "Functions", "Sum")

combined <- arrange(combined, desc(Sum))

## extracting the top six most frequently occurring terms for all topics
combined <- combined[1:6,1:4]

combined_long <- melt(combined, id = "word", variable.name = "Topic", value.name = "Count")

combined_long <- arrange(combined_long, desc(Count))

## dot plot for government improvement and transparency topics using ggpomological theme
dotplot <- ggplot(combined_2, aes(Improvement, Transparency)) +
  geom_jitter(alpha = 0.8, size = 2.5, width = 0.25, height = 0.25, colour = "#fd8f24") +
  geom_text(aes(label = word), check_overlap = TRUE, size = 5.5, vjust = 1.5, 
            family = "Homemade Apple", colour = "#6d452b") +
  scale_x_log10(breaks = c(1, 10), labels = c("10^0", "10^1")) +
  scale_y_log10(breaks = c(1, 10), labels = c("10^0", "10^1")) +
  theme_pomological_fancy(base_family = "Homemade Apple", base_size = 20)
plot(dotplot)

## plotting the six most frequently occurring terms for the three topics
stackplot <- ggplot(combined_long, aes(Topic, Count, fill = word)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  ylab("Frequency") +
  scale_fill_pomological() +
  theme_pomological_fancy(base_family = "Homemade Apple", base_size = 20) +
  theme(legend.title = element_blank()) 
plot(stackplot)

## outputting plots on a watercolour paper tile
 paint_pomological(dotplot, width = 800, height = 550) %>% 
   magick::image_write("transparency.png")
             
 paint_pomological(stackplot, width = 800, height = 550) %>% 
   magick::image_write("freq_terms.png")

