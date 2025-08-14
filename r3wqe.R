library(devtools)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)

#Loading primary data
df_steam <- read.csv("C:/Users/maxkr/Downloads/Steam Trends 2023 by @evlko and @Sadari - Games Data.csv", header=TRUE)
   View(`df_steam`)
#cleaning 
df_steam_clean <- df_steam %>% select(
  Title, 
  Reviews.Total,
  Reviews.Score.Fancy, 
  Release.Date, 
  Launch.Price,
  Revenue.Estimated
)

#filtering
df_steam_filtered <- df_steam_clean %>% 
  filter(Reviews.Total > 0)

#Remove % from reviews
df_steam_filtered$Reviews.Score.Fancy <- gsub("\\%","",df_steam_filtered$Reviews.Score.Fancy)

#Make reviews numeric
df_steam_filtered$Reviews.Score.Fancy <- as.numeric(df_steam_filtered$Reviews.Score.Fancy)

#Remove $ from launch price
df_steam_filtered$Launch.Price <- gsub("\\$","",df_steam_filtered$Launch.Price)

#Replace comma with decimal
df_steam_filtered$Launch.Price <- gsub("\\,", ".", df_steam_filtered$Launch.Price)

#Make launch price numeric
df_steam_filtered$Launch.Price <- as.numeric(df_steam_filtered$Launch.Price)

#Load secondary data set
df_steam_2 <- read.delim("C:/Users/maxkr/OneDrive/Desktop/archive (1)/dataset_top20_cleaned.csv")
  View(`df_steam_2`)

#Rename columns to match primary
df_steam_2_renamed <- df_steam_2 %>% rename(
  Title = app_name,
  Reviews.Text = review_text,
  Review.Score = review_score,
  Review.Votes = review_votes
)

#Clean secondary
df_steam_2_clean <- df_steam_2_renamed %>% select(
  Title,
  Reviews.Text,
  Review.Score,
  Review.Votes
)
df_steam_3 <- df_steam_filtered
df_steam_3 <- df_steam_3 %>% arrange(, desc(Reviews.Total), desc(Reviews.Score.Fancy))
df_steam_3 <- head(df_steam_3, n= 100)
df_steam_3 <- df_steam_3 %>% arrange(desc(Reviews.Score.Fancy))
df_steam_3 <- head(df_steam_3, n= 10)

df_steam_4 <-df_steam_filtered %>% arrange(desc(Launch.Price))
df_steam_4 <- df_steam_filtered %>% 
  filter(Launch.Price <= 1)
df_steam_4 <- df_steam_4 %>% arrange( desc(Reviews.Total))
df_steam_4 <- head(df_steam_4, n= 100)
df_steam_4 <- df_steam_4 %>% arrange(desc(Reviews.Score.Fancy))
df_steam_4 <- head(df_steam_4, n= 10)

#Plot 1 
ggplot(df_steam_filtered, aes(y = Reviews.Score.Fancy, x = Launch.Price)) + 
  geom_smooth()+
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80 ,90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  labs(
    title = "Launch Price Vs. Review Percentage",
    x = "Launch Price",
    y = "Review Percentage",
    color = "Black"
  )
#Plot 2 
ggplot(df_steam_filtered, aes(y = Reviews.Score.Fancy, x = Launch.Price)) + 
  geom_point()+
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80 ,90, 100)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  labs(
    title = "Launch Price Vs. Review Percentage",
    x = "Launch Price",
    y = "Review Percentage",
    color = "Black"
  )

#Plot 3 
ggplot(df_steam_3, aes(x = Reviews.Total, y =  Launch.Price)) +
  geom_point()+
  geom_smooth()+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  labs(
    title = "Total Reviews Vs. Launch Price",
    x = "Total Reviews",
    y = "Launch Price",
    color = "Black"
  )

corr_data <- df_steam_filtered %>%
  select(
    Launch.Price,
    Reviews.Total,
    Reviews.Score.Fancy
  )
cor_matrix <- cor(corr_data)

cor_melted <- melt(cor_matrix)

#Plot 4 
ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white")+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_blank()
  ) 
