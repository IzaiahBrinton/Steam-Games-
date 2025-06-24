In this Project I am doing a Steam Games dataset I Found on Kaggle 
After I have saved my database I uploaded it into Rstudio 
![image](https://github.com/user-attachments/assets/1e183569-2299-4097-b18c-5c382792f983)

Once that has been uploaded I did code to clean the data 

# Check for missing values in the entire dataset
colSums(is.na(df))

# Convert 'release_date' to Date format
df$release_date <- as.Date(df$release_date, format = "%d %b, %Y")

# Convert 'supported_os' to a factor since it’s a category
df$supported_os <- as.factor(df$supported_os)

# Remove exact duplicate rows
df <- df %>% distinct()

# For numerical columns like 'price', 'rating', and 'estimated_downloads'
# we can use IQR to find outliers

#Remove extreme outliers in price
Q1 <- quantile(df$price, 0.25)
Q3 <- quantile(df$price, 0.75)
IQR_price <- Q3 - Q1

# Filter out prices outside 1.5*IQR
df <- df %>%
  filter(price >= (Q1 - 1.5 * IQR_price) & price <= (Q3 + 1.5 * IQR_price))

# Repeat similar logic for other variables as needed

# Trim whitespace and unify text case in 'developer' and 'user_defined_tags'
df$developer <- str_trim(df$developer)
df$user_defined_tags <- str_to_title(str_trim(df$user_defined_tags))

# Final Check
summary(df)
str(df)

I implemented this data into R studio to get data cleaned 

![image](https://github.com/user-attachments/assets/920db610-fdb3-4423-8fbb-77a6957e8a52)

We are going to now filter and arrange the data 

# Keep only games with more than 1000 reviews and a rating above 4
filtered_df <- df %>%
  filter(all_reviews_number > 1000, rating > 4)

  # Arrange the filtered games by rating (descending)
arranged_df <- filtered_df %>%
  arrange(desc(rating))

| Game Name                | Developer             | Rating | Reviews | Price |
| ------------------------ | --------------------- | ------ | ------- | ----- |
| Chef RPG                 | World 2 Studio        | 4.83   | 1,571   | 10.49 |
| Miss Neko 3              | Double W              | 4.80   | 4,161   | 2.99  |
| The Witcher 3: Wild Hunt | CD PROJEKT RED        | 4.72   | 761,599 | 29.99 |
| Baldur's Gate 3          | Larian Studios        | 4.61   | 669,120 | 34.99 |
| Windowkill               | torcado               | 4.61   | 2,798   | 2.99  |
| Conquest Dark            | Eldritch Sword Games  | 4.60   | 1,436   | 5.79  |
| Sultan's Game            | Double Cross          | 4.59   | 15,152  | 12.49 |
| Assemble with Care       | ustwo games           | 4.58   | 7,335   | 4.49  |
| Thief™ II: The Metal Age | Looking Glass Studios | 4.58   | 1,765   | 3.49  |
| Songs of Syx             | Gamatron AB           | 4.57   | 5,602   | 12.49 |

Next, I am going to mean with standard deviation to find the reviews rate with the price 
![image](https://github.com/user-attachments/assets/6d9861bd-1936-4129-a8b2-35532e85324f)
mean_reviews_like_rate <- mean(df$reviews_like_rate, na.rm = TRUE)
sd_reviews_like_rate <- sd(df$reviews_like_rate, na.rm = TRUE)

mean_price <- mean(df$price, na.rm = TRUE)
sd_price <- sd(df$price, na.rm = TRUE)

I am going to perform a T Test for high vs low price 

![image](https://github.com/user-attachments/assets/afe19d1e-4907-41eb-98e9-2675be8d3ac7)

# Step 3: Filter two groups based on price
high_price <- games %>%
  filter(price > 20) %>%
  pull(reviews_like_rate)

low_price <- games %>%
  filter(price <= 20) %>%
  pull(reviews_like_rate)

# Step 4: Perform a t-test
t_test_result <- t.test(high_price, low_price,
                        alternative = "greater", 
                        var.equal = FALSE)

Linear regression for games 
![image](https://github.com/user-attachments/assets/08264e51-4108-4e09-b36f-85d0c429cf2d)

This is the results 
![image](https://github.com/user-attachments/assets/0271c322-a854-4eab-b27f-753785e9e59d)

Purpose:
Most games in the dataset tend to get positive reviews, are priced affordably, and have a wide variety of download numbers.
Interestingly, games that cost more generally receive better reviews, and this relationship is statistically significant.
I ran a simple regression using price and download counts to predict review scores. While the model is statistically significant, it doesn’t predict reviews very well on its own, which suggests we’d need to include more factors to get a better prediction.

Here are 3 Visualizations:
![image](https://github.com/user-attachments/assets/ad58a53e-f6ca-40a7-a560-e98c49da5a61)
![image](https://github.com/user-attachments/assets/33b46392-b9a6-4af6-ab40-1429e01b5033)
![image](https://github.com/user-attachments/assets/dfbdfff3-1560-4117-9a03-0a13848f281f)

