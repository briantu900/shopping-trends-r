install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
library(ggplot2)
library(dplyr)
library("tidyr")

df <- read.csv("C:/Users/brian/Downloads/shopping_trends_updated.csv")
df_head <- head(df)
df_head
colSums(is.na(df))
summary(df)

df_freq<- df %>%
  group_by(Frequency.of.Purchases) %>%
  summarise(count = n())

ggplot(df_freq, aes(x = Frequency.of.Purchases, y = count, fill = Frequency.of.Purchases)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +
  labs(title = "Number of Customers associated with Purchase Frequency",
       x = "",
       y = "No. of Customers",
       fill = 'Frequency of Purchases') +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5))+
  coord_flip()

df_review<- df %>%
  group_by(Category) %>%
  summarise(avg.review = mean(Review.Rating))
df_review

ggplot(df_review, aes(x=Category, y=avg.review, fill=Category))+
  geom_bar(stat='identity')+
  geom_text(aes(label = round((avg.review),2)), vjust = -0.5, color = "black")+
  labs(title = "Average Review Score of Category",
       x = "",
       y = "Review Score",
       fill = 'Categories') +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5))

df_purchase<- df %>%
  group_by(Category) %>%
  summarise(avg.Purchase.Amount.USD = mean(Purchase.Amount..USD.), total.Purchase.Amount.USD = sum(Purchase.Amount..USD.))

df_purchase$total.Purchase.Amount.USD <- df_purchase$total.Purchase.Amount.USD / 1000


ggplot(df_purchase, aes(x=Category, y='Purchase.Amount.USD.', fill=Category))+
  geom_bar(stat='identity')+
  geom_text(aes(label = paste('$',round((avg.Purchase.Amount.USD),2))), vjust = -0.5, color = "black")+
  labs(title = "Average Purchase Amount of Category",
       x = "",
       y = "Purchase Amount USD",
       fill = 'Categories') +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        axis.text.y = element_blank())

df_purchase_long <- tidyr::gather(df_purchase, key = "Statistic", value = "Value", -Category)

ggplot(df_purchase_long, aes(x = Category, y = Value, fill = Statistic)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.7)) +
  geom_text(aes(label = ifelse(Statistic == "sum", paste0("$", round(Value, 2), "k"), paste0("$", round(Value, 2)))), position = position_dodge(width = 0.7), vjust = 1.5) +
  facet_grid(Statistic ~ ., scales = "free_y", space = "free_y", switch = "y")+
  labs(title = "Mean and Sum of Purchase Amount of Category",
       x = "",
       y = "Purchase Amount USD",
       fill = "Category") +
  theme_minimal()