# Leadership & Consulting Final Project

setwd('/Users/gavin-kunish/Desktop')

library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(scales)
library(cluster)


ecommerce <- read_excel('Ecommerce data.xlsx')

ecommerce_clean <- ecommerce[, 1:(ncol(ecommerce) - 3)]

# freq_cols <- grep("^F(?!$)", names(ecommerce_clean), value = TRUE, perl = TRUE)  # exclude just 'f'
# monetary_cols <- grep("^M", names(ecommerce_clean), value = TRUE)
# 
# total_freq <- colSums(ecommerce_clean[, freq_cols])
# total_monetary <- colSums(ecommerce_clean[, monetary_cols])
# 
# freq_df <- data.frame(genre = names(total_freq), total = total_freq) %>%
#   arrange(desc(total))
# 
# monetary_df <- data.frame(genre = names(total_monetary), total = total_monetary) %>%
#   arrange(desc(total))
# 
# ggplot(freq_df, aes(x = total, y = reorder(genre, total))) +
#   geom_col(fill = "steelblue") +
#   labs(title = "Total Books Purchased by Genre", x = "Total Count", y = "Genre") +
#   theme_minimal()
# 
# ggplot(monetary_df, aes(x = total, y = reorder(genre, total))) +
#   geom_col(fill = "darkgreen") +
#   labs(title = "Total Money Spent by Genre", x = "Total $ Spent", y = "Genre") +
#   theme_minimal()
# 
# 
# ggplot(ecommerce_clean, aes(x = f)) + 
#   geom_histogram(bins = 50, fill = "steelblue") + 
#   labs(title = "Frequency of Purchases (f)", x = "Total Orders", y = "Count of Customers")
# 
# max(ecommerce_clean$m)
# 
ecommerce_clean %>%
  filter(m > 5000) %>%
  arrange(desc(m)) %>%
  select(id, m, f, r, tof)

ecommerce_clean <- ecommerce_clean %>% filter(m <= 11000)
# ggplot(ecommerce_clean, aes(x = m)) + 
#   geom_histogram(bins = 50, fill = "darkgreen") + 
#   labs(title = "Total Spend (m)", x = "Dollars Spent", y = "Count of Customers")
# 
# ggplot(ecommerce_clean, aes(x = tof)) + 
#   geom_histogram(bins = 50, fill = "purple") + 
#   labs(title = "Time on File (tof)", x = "Days", y = "Count of Customers")
# 
# ggplot(ecommerce_clean, aes(x = r)) + 
#   geom_histogram(bins = 50, fill = "tomato") + 
#   labs(title = "Recency (r)", x = "Days Since Last Purchase", y = "Count of Customers")

f_cols <- grep("^F(?!$)", names(ecommerce_clean), perl = TRUE, value = TRUE)
m_cols <- grep("^M", names(ecommerce_clean), value = TRUE)

# cor_f <- cor(ecommerce_clean[, f_cols])
# cor_m <- cor(ecommerce_clean[, m_cols])
# 
# corrplot(cor_f, method = "color", type = "upper", tl.cex = 0.6, title = "Frequencies by Genre")
# corrplot(cor_m, method = "color", type = "upper", tl.cex = 0.6, title = "Spending by Genre")

ecommerce_clean$num_genres <- rowSums(ecommerce_clean[, f_cols] > 0)

# ggplot(ecommerce_clean, aes(x = num_genres)) +
#   geom_histogram(binwidth = 1, fill = "steelblue") +
#   labs(title = "Number of Genres Purchased", x = "Genres with ≥1 Purchase", y = "Count of Customers")

rfm_data <- ecommerce_clean[, c("r", "f", "m")]
rfm_scaled <- scale(rfm_data)

set.seed(75)

wss <- sapply(1:10, function(k) {
  kmeans(rfm_scaled, centers = k, nstart = 25)$tot.withinss
})
plot(1:10, wss, type = "b")

kmeans_rfm <- kmeans(rfm_scaled, centers = 5, nstart = 25)
ecommerce_clean$segment <- as.factor(kmeans_rfm$cluster)

centroids <- as.data.frame(kmeans_rfm$centers)
centroids$segment <- 1:nrow(centroids)

centroids


ecommerce_clean <- ecommerce_clean %>%
  mutate(segment_label = case_when(
    segment == 1 ~ "Passive Browsers",
    segment == 2 ~ "Loyal Buyers",
    segment == 3 ~ "Power Users",
    segment == 4 ~ "Casual Customers",
    segment == 5 ~ "Big Spenders",
    segment == 6 ~ "Dormant Users"
  ))

kmeans_rfm$tot.withinss

d <- dist(rfm_scaled)
sil <- silhouette(kmeans_rfm$cluster, d)
mean(sil[, 3])
plot(sil)


f_cols <- grep("^F(?!$)", names(ecommerce_clean), value = TRUE, perl = TRUE)
f_data <- ecommerce_clean[, f_cols]

genre_lookup <- setNames(f_cols, gsub("^F", "", f_cols))

ecommerce_clean$favorite_genre <- apply(f_data, 1, function(row) {
  fav <- which.max(row)
  if (length(fav) == 0 || row[fav] == 0) return(NA)
  return(genre_lookup[fav])
})


names(ecommerce_clean)

genre_labels <- c(
  "fiction1" = "Fiction",
  "classics3" = 'Classics',
  'cartoons5' = 'Cartoons',
  'legends6' = 'Legends',
  "philosophy7" = "Philosophy",
  'religion8' = 'Religion',
  'psychology9' = 'Psychology',
  'linguistics10' = 'Linguistics',
  "art12" = "Art",
  'music14' = 'Music',
  'fascimile17' = 'Facsimile Editions',
  "history19" = "History",
  'conthist20' = 'Contemperary History',
  'economy21' = 'Economy',
  'politics22' = 'Politics',
  'science23' = 'Science',
  'compsci26' = 'Computer Science',
  'railroads27' = 'Railroads',
  'maps30' = 'Maps',
  'travelguides31' = 'Travel Guides',
  'health35' = 'Health',
  'cooking36' = 'Cooking',
  'learning37' = 'Learning',
  'GamesRiddles38' = 'Games & Riddles',
  'sports39' = 'Sports',
  'hobby40' = 'Hobby',
  'nature41' = 'Nature',
  'encyclopaedia44' = 'Encyclopaedia',
  'videos50' = 'Videos',
  'nonbooks99' = 'Nonbooks'
)


ecommerce_clean <- ecommerce_clean %>%
  mutate(
    favorite_genre_clean = gsub("^F", "", favorite_genre),
    genre_label = genre_labels[favorite_genre_clean]
  )

ecommerce_clean$genre_label <- genre_labels[ecommerce_clean$favorite_genre_clean]

ecommerce_clean <- ecommerce_clean %>%
  mutate(
    discount = case_when(
      segment_label == "Power Users" ~ 0,
      segment_label == "Loyal Buyers" ~ 0,
      segment_label == "Big Spenders" ~ 0,
      segment_label == "Passive Browsers" ~ 0.10,
      segment_label == "Casual Customer" ~ 0.10,
      segment_label == "Dormant Users" ~ 0.20
    ),
    message = case_when(
      segment_label == "Power Users"       ~ paste0("Another bestseller in ", genre_label, " just arrived — we saved it for you."),
      segment_label == "Loyal Buyers"      ~ paste0("Looking for more in ", genre_label, "? Here's what's trending this week."),
      segment_label == "Big Spenders"      ~ paste0("Exclusive picks in ", genre_label, " we think you'll love — hand-picked for you."),
      segment_label == "Passive Browsers"  ~ paste0("See something you like in ", genre_label, "? Here's 10% off to help you decide."),
      segment_label == "Casual Customers"  ~ paste0("Catch up on your favorite ", genre_label, " books — now with 10% off."),
      segment_label == "Dormant Users"     ~ paste0("We miss you! Here's 20% off ", genre_label, " picks to bring you back.")
    )
  )


set.seed(456)

new_customers <- data.frame(
  id = 1:10,
  r = sample(1:2500, 10, replace = TRUE),
  f = sample(1:60, 10, replace = TRUE),
  m = round(runif(10, 10, 5000), 2)
)

new_scaled <- scale(
  new_customers[, c("r", "f", "m")],
  center = attr(rfm_scaled, "scaled:center"),
  scale = attr(rfm_scaled, "scaled:scale")
)

get_cluster <- function(row, centers) {
  dists <- apply(centers, 1, function(center) sum((row - center)^2))
  return(which.min(dists))
}

centroids_matrix <- as.matrix(kmeans_rfm$centers)
new_customers$segment <- apply(new_scaled, 1, get_cluster, centers = centroids_matrix)


segment_labels <- c(
  "1" = "Passive Browsers",
  "2" = "Loyal Buyers",
  "3" = "Power Users",
  "4" = "Casual Customers",
  "5" = "Big Spenders",
  "6" = "Dormant Users"
)

new_customers$segment_label <- segment_labels[as.character(new_customers$segment)]

set.seed(50)
genre_keys <- names(genre_labels)
new_customers$favorite_genre <- sample(genre_keys, 10, replace = TRUE)
new_customers$genre_label <- genre_labels[new_customers$favorite_genre]

new_customers <- new_customers %>%
  mutate(
    message = case_when(
      segment_label == "Power Users"       ~ paste0("Another bestseller in ", genre_label, " just arrived — we saved it for you."),
      segment_label == "Loyal Buyers"      ~ paste0("Looking for more in ", genre_label, "? Here's what's trending this week."),
      segment_label == "Big Spenders"      ~ paste0("Exclusive picks in ", genre_label, " we think you'll love — hand-picked for you."),
      segment_label == "Passive Browsers"  ~ paste0("See something you like in ", genre_label, "? Here's 10% off to help you decide."),
      segment_label == "Casual Customers"  ~ paste0("Catch up on your favorite ", genre_label, " books — now with 10% off."),
      segment_label == "Dormant Users"     ~ paste0("We miss you! Here's 20% off ", genre_label, " picks to bring you back.")
    )
  )

table(ecommerce_clean$segment_label)

segment_counts <- ecommerce_clean %>%
  count(segment_label)

segment_counts
