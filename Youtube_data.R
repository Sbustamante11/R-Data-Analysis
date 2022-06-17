# R project

# LOAD AND PREPARE DATA ####
# INSTALL PACKAGES #####
install.packages("psych")
install.packages("janitor")

# pacman, tidyverse packages ####
pacman::p_load(pacman, party, psych, rio, tidyverse)

library(janitor)


#open and read csv file
(df <- read_csv("youtube_channels.csv"))

# clean data columns ####
clean_df <- clean_names(df)
colnames(clean_df)

# BARPLOT ##

?plot
?barplot

barplot(clean_df$started)
barplot(clean_df$video_count)

# specify plot options ####
df %>%
  select(rank) %>%
  table()
  barplot(
    main = "Year of Channel Creation",
    sub = "(Source: ggplot:: started)",
    horiz = F,
    ylab = "Year", 
    xlab = "Name",
    xlim = c(0,2050),
    border = NA,
    col = "#CD0000",
    height = 100
)
  
barplot( table(clean_df$subscribers))

# displays count of channel types by category
barplot( table(clean_df$category))

# bar plot in ggplot 2
clean_df %>% ggplot(aes(x=started)) +
       geom_bar(aes(fill = started)) +
        #geom_density(fill = "blue")
       coord_flip() +
      ylab("Count of Content Creators")+
      xlab("Year Started")+
      ggtitle("Year of Channel Creation")



# SUMMARIZE DATA ####
# csv data is loaded into a data frame####
summary(clean_df)

# in depth statistics view ####
describe(clean_df)

# COLUMN VIEW ####
# returns all of our columns in data set ####
names(clean_df)

# place all of our columns into variable youtube.colnames ####
youtube.colnames <- names(clean_df)

# returns all subscriber numbers in descending order
clean_df$subscribers

# FILTER AND SORT DATA ####
# filter data by view count ###
# total views > 10 billion ###

clean_df %>%
  filter(video_views > 100000000000) %>%
  print()

# filter by subscriber numbers > 50 mil
clean_df %>%
  filter(subscribers > 50000000) %>%
  print()

# filter data by category
# display only gaming channels
clean_df %>%
  filter(category == "Gaming") %>%
  print()

# video count vector
vec0 <- clean_df$video_count
vec0

# year started vector
vec1 <- clean_df$started
vec1

# create get mode function ####
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- getmode(vec1)
print(result)

vec0[is.na(vec0)] <- 0
vec0

mean(vec0)

