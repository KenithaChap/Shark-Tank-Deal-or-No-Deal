shark_data = read.csv("~/Desktop/Fall 2023/Stat Foundations of Data Analytics/Final Project/Dataset 3 — Shark tank pitches.csv")
us_cities = read.csv("~/Desktop/Fall 2023/Stat Foundations of Data Analytics/Final Project/uscities.csv")
attach(shark_data)
attach(us_cities)

###########################################################
################## 2023 Final Project #####################
######## Shark Tank Analytics: Deal or no Deal ############
###########################################################

#### Data Preprocessing ####
# Dropping the non predictive variables 
# Columns to drop in shark_data
columns_to_drop = c(
  "description", "website", "title",
  "episode.season", "plot_keywords", "shark1", 
  "shark2", "shark3", "shark4", "shark5", "entrepreneurs")

# Dropping columns
shark_data = shark_data[, !names(shark_data) %in% columns_to_drop]

# Change exchange for stake into percentage 
shark_data$exchangeForStake = shark_data$exchangeForStake/100

# Change valuation and asked for in thousands
shark_data$valuation = shark_data$valuation/1000
shark_data$askedFor = shark_data$askedFor/1000

# Change all True = 1, False = 0 
shark_data$deal = as.numeric(shark_data$deal) 
# Deal = 1, No Deal = 0
shark_data$Multiple.Entreprenuers = as.numeric(shark_data$Multiple.Entreprenuers) 
# More than one entrepreneur = 1, One entrepreneur = 0
View(shark_data)

#### Heatmap: Location vs Deal ####

# Columns to drop in us_cities
columns_to_drop_2 = c(
  "city_ascii", "state_name", "county_fips",
  "county_name", "population", "density", 
  "source", "military", "incorporated", "timezone", "ranking",
  "zips", "id")

# Drop columns 
us_cities = us_cities[, !names(us_cities) %in% columns_to_drop_2]

# Merge city, state 
us_cities$location = paste(us_cities$city, us_cities$state_id, sep = ", ")
us_cities = subset(us_cities, select = -c(city, state_id))
View(us_cities)

# Merge based on the city column to add longitude/lattitude
shark_data = merge(shark_data, us_cities, by = "location", all.x = TRUE)

# Create heatmap based off deals in each state 
#install.packages("leaflet")
library(leaflet)
library(dplyr)

# Create a sample data frame with latitude, longitude, and deal status
heatmap_df = data.frame(
  Longitude = shark_data$lng,
  Latitude = shark_data$lat,
  Deal = shark_data$deal
)
View(heatmap_df)

# Create a leaflet map with conditional coloring based on deal status
leaflet(heatmap_df) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    fillColor = ~ifelse(Deal, "green", "red"),
    color = "white",
    radius = 10,
    fillOpacity = 0.7,
    popup = ~paste("Deal Made: ", deal)
  )

# Most common locations
location_table = table(shark_data$location)
location_table = sort(location_table, decreasing = TRUE)
loc_table_df = as.data.frame(location_table)
print(loc_table_df)

# drop location column and use lat/long 
shark_data = subset(shark_data, select = -c(location))

#### Deal ####
table(deal)
deal = (244/(244+251))*100
deal
no_deal = 251/(244+251)*100
no_deal

# Summary of deals only 
shark_deals = shark_data[shark_data$deal == 1, ]
View(shark_deals)

summary(shark_deals)

# Summary of no deals
shark_no_deals = shark_data[shark_data$deal == 0, ]
View(shark_no_deals)

summary(shark_no_deals)

#### Categories ####
# Most common categories
category_table = table(shark_data$category)
category_table = sort(category_table, decreasing = TRUE)
cat_table_df = as.data.frame(category_table)
print(cat_table_df)

shark_data$category = as.character(shark_data$category)

# Hierarchical Clustering
#install.packages("cluster")
library(cluster)
category_data = shark_data[,c(1,3)]
View(category_data)

# Hierarchical Clustering 
set.seed(123)

# Convert 'category' to a factor for clustering
category_data$category = as.factor(category_data$category)

# Create a binary matrix for each category
binary_matrix = model.matrix(~ category - 1, data = category_data)

# Calculate the binary distance matrix using the dist function
dist_matrix = dist(binary_matrix, method = "binary")

# Perform hierarchical clustering 
hc = hclust(dist_matrix, method = "complete")

# Plot the dendrogram
plot(hc, main = "Hierarchical Clustering Dendrogram", xlab = "Categories")
clusters = cutree(hc, 54)

category_data$cluster = clusters

# Find probability of getting a deal per cluster 
prob_cat = category_data %>%
  group_by(cluster) %>%
  summarise(probability_of_deal = mean(deal))
prob_cat

require(ggplot2)
# Plot the results 
plot = ggplot(prob_cat, aes(x = factor(cluster), y = probability_of_deal)) +
  geom_bar(stat = "identity", fill = "turquoise") +
  labs(title = "Probability of Getting a Deal per Category",
       x = "Category",
       y = "Probability of Deal")
plot
# 47 (Fashion Accessories) and 52 (Maternity) have 0 deals made 

# Top/Low 5 probabilities 
category_data = merge(prob_cat, category_data, by = "cluster", all.x = TRUE)
View(category_data)

# Combine clusters for one clean list
unique_clusters = category_data %>%
  distinct(cluster, .keep_all = TRUE)

# Add probability to actual dataset 
probability = category_data[, c(2,4)]

shark_data = merge(probability, shark_data, by = "category", all.x = TRUE)
View(shark_data)
attach(shark_data)

# rename to category_probability
names(shark_data)[names(shark_data) == "probability_of_deal"] = "category_success"
attach(shark_data)
View(shark_data)

#### Amount Asked For ####
# Visualization 
summary(exchangeForStake)
summary(askedFor)
summary(valuation)
# Histogram
ggplot(shark_data, aes(x = askedFor)) +
  geom_histogram(binwidth = 100, fill = "pink", color = "black", alpha = 0.7) +
  labs(
    x = "Amount Asked For",
    y = "Frequency",
    title = "Histogram of Amounts Companys Asked For"
  ) +
  theme_minimal()

# Boxplot
ggplot(shark_data, aes(x = askedFor)) +
  geom_boxplot(fill = "pink") +
  labs(
    x = "Amount Asked For",
    y = "Frequency",
    title = "Boxplot of Amounts Companys Asked For"
  ) +
  theme_minimal()

# Logistic Regression
require(methods)

logit = glm(deal~askedFor, data = shark_data, family = "binomial")
summary(logit)
# P-value of ~0, very significant
# negatively correlated 

# predictions for context
values = data.frame(askedFor = c(10,258,5000))
predict(logit, values, type="response")

# plot
plot = ggplot(shark_data, aes(y=deal, x=askedFor))
scatter = geom_point()
line = geom_smooth(method="glm", formula=y~x, method.args = list(family = binomial), color = "magenta")
labels = labs(title = "Deal or No Deal and Amount Asked for", x = "Amount Asked For", y = "Deal")
theme = theme(axis.text.x = element_text(size = 15),  
          axis.text.y = element_text(size = 15),  
          plot.title = element_text(size = 20),
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20)) 
plot+scatter+line+labels+theme

#### Valuation ####
# Visualization
summary(valuation)
# Histogram
ggplot(shark_data, aes(x = valuation)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    x = "Valuation",
    y = "Frequency",
    title = "Histogram of Company Valuations"
  ) +
  theme_minimal()

# Boxplot
ggplot(shark_data, aes(x = valuation)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    x = "Valuation",
    y = "Frequency",
    title = "Boxplot of Company Valuations"
  ) +
  theme_minimal()

logit = glm(deal~valuation, data = shark_data, family = "binomial")
summary(logit)
# P-value of ~0, highly significant 

# plot
plot = ggplot(shark_data, aes(y=deal, x=valuation))
scatter = geom_point()
line = geom_smooth(method="glm", formula=y~x, method.args = list(family = binomial), color = "skyblue")
labels = labs(title = "Valuation Impact on Deals", x = "Valuation", y = "Deal")
theme = theme(axis.text.x = element_text(size = 15),  
              axis.text.y = element_text(size = 15),  
              plot.title = element_text(size = 20),
              axis.title.x = element_text(size = 20), 
              axis.title.y = element_text(size = 20)) 
plot+scatter+line+labels+theme

#### Stake ####
# Visualization
summary(exchangeForStake)
# Histogram
ggplot(shark_data, aes(x = exchangeForStake)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black", alpha = 0.7) +
  labs(
    x = "Exchange for Stake",
    y = "Frequency",
    title = "Histogram of Stake Exchanges"
  ) +
  theme_minimal()

# Boxplot
ggplot(shark_data, aes(x = exchangeForStake)) +
  geom_boxplot(fill = "orange") +
  labs(
    x = "Exchange for Stake",
    y = "Frequency",
    title = "Boxplot of Stake Exchanges"
  ) +
  theme_minimal()

logit = glm(deal~exchangeForStake, data = shark_data, family = "binomial")
summary(logit)
# P-value of ~0, highly significant 

# plot
plot = ggplot(shark_data, aes(y=deal, x=exchangeForStake))
scatter = geom_point()
line = geom_smooth(method="glm", formula=y~x, method.args = list(family = binomial), color = "orange")
labels = labs(title = "Exchange for Stake Impact on Deals", x = "Exchange for Stake", y = "Deal")
theme = theme(axis.text.x = element_text(size = 15),  
              axis.text.y = element_text(size = 15),  
              plot.title = element_text(size = 20),
              axis.title.x = element_text(size = 20), 
              axis.title.y = element_text(size = 20)) 
plot+scatter+line+labels+theme

#### Season ####
ggplot(shark_data, aes(x = season)) +
  geom_histogram(binwidth = 0.5, fill = "light green", color = "black", alpha = 0.7) +
  labs(
    x = "Season Number",
    y = "Frequency",
    title = "Histogram of Season Number"
  ) +
  theme_minimal()

logit = glm(deal~season, data = shark_data, family = "binomial")
summary(logit)
# P-value of 0.703, not significant 

# success rate per season
library(dplyr)

# Success rate per season 
grouped_data = shark_data %>%
  group_by(season)

success_per_season = grouped_data %>%
  summarise(mean_success_rate = mean(deal))
success_per_season

plot = ggplot(success_per_season, aes(x = factor(season), y = mean_success_rate)) +
  geom_bar(stat = "identity", fill = "light green", width = 0.5) +
  geom_text(aes(label = sprintf("%.2f", mean_success_rate)), vjust = -0.5, color = "black") +
  labs(title = "Average Success Rate Across Seasons",
       x = "Season",
       y = "Average Success Rate") +
  theme(axis.text.x = element_text(size = 15),  
        axis.text.y = element_text(size = 15),  
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20)) 
plot

# check for correlation
reg = lm(mean_success_rate~season, data = success_per_season)
summary(reg)
# not significant at all

#### Episode ####
ggplot(shark_data, aes(x = episode)) +
  geom_histogram(binwidth = 0.5, fill = "orange", color = "black", alpha = 0.7) +
  labs(
    x = "Episode Number",
    y = "Frequency",
    title = "Histogram of Episode Number"
  ) +
  theme_minimal()

logit = glm(deal~episode, data = shark_data, family = "binomial")
summary(logit)
# P-value of ~0, highly significant 

# Success rate per episode 
grouped_data2 = shark_data %>%
  group_by(episode)

success_per_episode = grouped_data2 %>%
  summarise(mean_success_rate = mean(deal))
success_per_episode

episode_ordered = success_per_episode[order(success_per_episode$mean_success_rate), decreasing = TRUE]
View(episode_ordered)

# check for correlation
reg = lm(mean_success_rate~episode, data = success_per_episode)
summary(reg)
# not strong enough

#### Multiple Entrepreneurs ####
ggplot(shark_data, aes(x = Multiple.Entreprenuers)) +
  geom_histogram(binwidth = 0.5, fill = "purple", color = "black", alpha = 0.7) +
  labs(
    x = "More than one Entrepreneur",
    y = "Frequency",
    title = "Histogram of Number of Entrepreneurs"
  ) +
  theme_minimal()
# 0 = Only one entrepreneur, 1 = more than one entrepreneur
table(Multiple.Entreprenuers)
# majority are sole entrepreneurs 

logit = glm(deal~Multiple.Entreprenuers, data = shark_data, family = "binomial")
summary(logit) 
# P-value of ~0, highly significant 

#### Multiple Logistic Regression ####
# First model
mlogit = glm(deal~askedFor+valuation+episode+season+exchangeForStake+
                Multiple.Entreprenuers, data = shark_data, family = "binomial")
summary(mlogit)

# Second model
mlogit2 = glm(deal~askedFor*valuation+episode+season+exchangeForStake+
               Multiple.Entreprenuers+category_success, data = shark_data, family = "binomial")
summary(mlogit2)

# Third model
mlogit3 = glm(deal~askedFor*valuation+exchangeForStake+Multiple.Entreprenuers+
                category_success, data = shark_data, family = "binomial")
summary(mlogit3)
# best model

# collinearity
vif(mlogit2)
vif(mlogit3)

# prediction with min, mean, max
values = data.frame(askedFor = c(10,236,5000),
                    valuation = c(40,1934.5,30000),
                    exchangeForStake = c(0.03,0.1756,1),
                    Multiple.Entreprenuers = c(0,1,1),
                    category_success = c(0,0.5199,1))

predict(mlogit3, values, type="response")

# Stargazer 
require("stargazer")
stargazer(mlogit3, type="html", dep.var.labels=c("Deal or No Deal"), 
          covariate.labels=c("Amount Asked For", "Valuation", "Stake", 
                             "More than one Entrepreneur",
                             "Category Deal Probability", "Amount Asked For*Valuation",
                             "Constant"), digits=2)

# Look for Interaction Terms 
# Linearity Assumption
linear = lm(askedFor~valuation, data = shark_data)
residualPlots(linear)
# Not Linear, P-value < 0.1

# Linearity Assumption
linear2 = lm(episode~season, data = shark_data)
residualPlots(linear2)
# Linear, P-value > 0.1

# Linearity Assumption
linear3 = lm(Multiple.Entreprenuers~exchangeForStake, data = shark_data)
residualPlots(linear3)
# Linear, P-value < 0.1

#### Regression Trees ####
#install.packages("tree")
#install.packages("rpart.plot")
library(tree)
library(rpart)
library(rpart.plot)

# Optimal Tree
mytree=rpart(deal~askedFor+valuation+Multiple.Entreprenuers+category_success,
             data = shark_data, control=rpart.control(cp=0.015))
rpart.plot(mytree)
summary(mytree)

printcp(mytree)
plotcp(mytree)
# out-of-sample performance 

# CP value minimizing error 
opt_cp = mytree$cptable[which.min(mytree$cptable[, "xerror"]), "CP"]
opt_cp
# just gives the same CP

# best tree for predictions
optimal_tree = rpart(deal~askedFor+valuation+Multiple.Entreprenuers+category_success, 
                     data = shark_data, control=rpart.control(cp=opt_cp))
printcp(optimal_tree)
rpart.plot(optimal_tree)

#### Random Forest ####
#install.packages("randomForest")
library(randomForest)
myforest = randomForest(deal~askedFor+valuation+Multiple.Entreprenuers+
                        category_success, ntree=250, data=shark_data, importance=TRUE, na.action = na.omit)
myforest
#MSE: Mean Squared Residuals = 0.1238314
# r-squared: 50.39%
# better model with episode and season, but overfits the model with a 99% r-squared

importance(myforest)
varImpPlot(myforest) 

# Out of bag performance 
newforest = randomForest(deal~askedFor+valuation+Multiple.Entreprenuers+
                         category_success,ntree=1000, data=shark_data, importance=TRUE,
                         na.action=na.omit, do.trace=50)
# lowest MSE as you add trees (ntree = 250)

#### Boosted Tree ####
library(gbm)
set.seed (1)
boosted = gbm(deal~askedFor+valuation+season+episode+exchangeForStake+
                Multiple.Entreprenuers+category_success, data = shark_data,
              distribution="gaussian", n.trees=10000, interaction.depth=4)
summary(boosted) 

predicted_score = predict(boosted, newdata = shark_data, n.trees=10000)
mean((predicted_score-deal)^2) 
# MSE way to high, but model to be developed (just to analyze variables)




  
  
  
