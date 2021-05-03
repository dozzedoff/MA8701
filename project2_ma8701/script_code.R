set.seed(1337)
library(fastDummies)
library(corrplot)
library(tidyverse)
library(randomForest)

# loading the dataset
ds_vgs_org <- as.data.frame(read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv"))


###########################################
####### Cleaning of the dataset ###########
###########################################

# Remove the covariates that are not going to be used

ds_vgs <-  subset(ds_vgs_org, select = -c(NA_Sales,EU_Sales, JP_Sales, Other_Sales))

# Need to open the dataset in excel and add the missing release dates


# Remove rows where both the critic values and user values are missing
ds_vgs_t <- subset(ds_vgs,!(is.na(ds_vgs$Critic_Count) & is.na(ds_vgs$User_Count)) )

write.csv(ds_vgs_t,"vgs_22_2016.csv")

# Updated dataset with missing year of release

ds_vgs_updt <- as.data.frame(read.csv("vgs_22_2016_new.csv", sep=";"))

ds_vgs_updt <- subset(ds_vgs_updt, ds_vgs_updt$Name != "Steins")


# Impute values for games that have critics score, but not user score, and the other way around
# Imputing values happens by taking the mean of corresponding values that exist for similar range of user/critic score, depending on which one is applicable
ds_vgs_updt$User_Score[ds_vgs_updt$User_Score=="tbd"] <- NA
ds_vgs_updt$User_Score[ds_vgs_updt$User_Score==""] <- NA
ds_vgs_updt$User_Score <- as.numeric(ds_vgs_updt$User_Score)
ds_vgs_imputed <- ds_vgs_updt



for(i in 1:length(ds_vgs_updt$Name)){
  if(is.na(ds_vgs_imputed$User_Score[i])){
    ds_vgs_imputed$User_Score[i] <- mean(as.numeric(ds_vgs_updt$User_Score[(na.omit(ds_vgs_updt$Critic_Score-5) <= ds_vgs_updt$Critic_Score[i] & ds_vgs_updt$Critic_Score[i]<=na.omit(ds_vgs_updt$Critic_Score+5))]), na.rm=TRUE)
    if(ds_vgs_imputed$User_Count[i]=="" | is.na(ds_vgs_imputed$User_Count[i])){
      ds_vgs_imputed$User_Count[i] <- mean(as.numeric(ds_vgs_updt$User_Count[((ds_vgs_updt$Critic_Score-5) <= ds_vgs_updt$Critic_Score[i] & ds_vgs_updt$Critic_Score[i]<=(ds_vgs_updt$Critic_Score+5))]), na.rm=TRUE)
    }
  }
  else if(ds_vgs_imputed$Critic_Score[i]=="" | is.na(ds_vgs_imputed$Critic_Score[i])){
    ds_vgs_imputed$Critic_Score[i] <- mean(as.numeric(ds_vgs_updt$Critic_Score[((as.numeric(ds_vgs_updt$User_Score)-5) <= ds_vgs_updt$User_Score[i] & ds_vgs_updt$User_Score[i]<=(as.numeric(ds_vgs_updt$User_Score)+5))]), na.rm=TRUE)
  if(ds_vgs_imputed$Critic_Count[i] == "" | is.na(ds_vgs_imputed$Critic_Count[i])){
    ds_vgs_imputed$Critic_Count[i] <- mean(as.numeric(ds_vgs_updt$Critic_Count[((as.numeric(ds_vgs_updt$User_Score)-5) <= ds_vgs_updt$User_Score[i] & ds_vgs_updt$User_Score[i]<=(as.numeric(ds_vgs_updt$User_Score)+5))]), na.rm=TRUE)
    }
  }
}


ds_vgs_imputed$Publisher[ds_vgs_imputed$Publisher == "N/A"] <- "Unknown"
ds_vgs_imputed$Developer[ds_vgs_imputed$Developer == ""] <- "Unknown"
ds_vgs_imputed$Rating[ds_vgs_imputed$Rating== "K-A"] <-"E"
ds_vgs_imputed$Rating[ds_vgs_imputed$Rating== "RP"] <-"Unknown"
ds_vgs_imputed$Rating[ds_vgs_imputed$Rating== ""] <-"Unknown"
ds_vgs_imputed$Rating[ds_vgs_imputed$Rating== "EC"] <-"E"


write.csv(ds_vgs_imputed, "vgs_22_2016_expl_analysis.csv")
#ds_vgs_imputed_with_name <- ds_vgs_imputed 
#ds_vgs_imputed <- subset(ds_vgs_imputed, select = -c(Name))


# Not dummy coding the genre and platform 
ds_vgs_imputed_no_dummy_genre_platform <- dummy_cols(ds_vgs_imputed, select_columns = c("Publisher", "Developer"))

ds_vgs_final_2 <- subset(ds_vgs_imputed_no_dummy_genre_platform, select = -c(X, Publisher, Developer))


ds_vgs_imputed_test <- dummy_cols(ds_vgs_imputed, 
                                  select_columns = c("Publisher","Developer","Platform","Genre"))

ds_vgs_final <- subset(ds_vgs_imputed_test, select = -c(X, Publisher, Developer, Platform, Genre))



# Exploratory analysis

# Correlation plot 
corrplot(cor(ds_vgs_imputed[6:10]))
ds_vgs_imputed_corr_genre <- dummy_cols(ds_vgs_imputed, select_columns = "Genre")
corrplot(cor(ds_vgs_imputed_corr_genre[c(6:10,13:24)]))

ds_vgs_imputed_corr_platform <- dummy_cols(ds_vgs_imputed, select_columns="Platform")

corrplot(cor(ds_vgs_imputed_corr_platform[c(6:10,13:29)]))


year_count <- count(ds_vgs_imputed, Year_of_Release)

# Released games each year
ggplot(data=year_count, aes(x=Year_of_Release,y=n)) + geom_line()


# platform and genre
plat_genre <- count(ds_vgs_imputed, Genre, Platform)


ggplot(plat_genre, aes(x=Genre, y=Platform, fill=n)) + geom_tile()

# Total number of sales per genre 
sum_sales_genre <- ds_vgs_imputed %>% group_by(Genre,Platform) %>% summarise(sum_gs = sum(Global_Sales))

ggplot(sum_sales_genre, aes(x=Platform, ))



# Mean of sales per genre
mean_sales_genre <- ds_vgs_imputed %>% group_by(Genre) %>% summarise(mean_gs = mean(Global_Sales))

# Median of sales per genre
median_sales_genre <- ds_vgs_imputed %>% group_by(Genre) %>% summarise(median_gs = median(Global_Sales))

# Sales per region of each genre or smtng like that
sum_sales_developer <- ds_vgs_expl_analysis %>% group_by(Developer) %>% summarise(sum_gs = sum(Global_Sales))
top_sales_developer <- top_n(arrange(sum_sales_developer,desc(sum_gs)),10)
ggplot(top_sales_developer, aes(x=Developer, y=sum_gs)) + geom_col() + ggtitle("Top ten developers with highest overall global sales") + theme(text = element_text(size=3))

sum_sales_publisher <- ds_vgs_expl_analysis %>% group_by(Publisher) %>% summarise(sum_gs = sum(Global_Sales))
top_sales_publisher <- top_n(arrange(sum_sales_publisher,desc(sum_gs)),10)
ggplot(top_sales_publisher, aes(x=Developer, y=sum_gs)) + geom_col() + ggtitle("Top ten publisher with highest overall global sales") + theme(text = element_text(size=3))


# Dummy variable coding 


write.csv(ds_vgs_final,"vgs_22_2016_cleaned_dummied_final.csv")

write.csv(ds_vgs_final_2,"vgs_22_2016_cleaned_dummied_final_2.csv")

ds_vgs_dont_joke <- as.data.frame(read.csv("vgs_22_2016_cleaned_dummied_final_2.csv"))

ds_vgs_dont_joke <- subset(ds_vgs_dont_joke, select = -c(X))
#set.seed(1337, kind = "Mersenne-Twister", normal.kind = "Inversion")
set.seed(1337)
data_set_size <- floor(nrow(ds_vgs_dont_joke))

indexes <- sample(1:data_set_size, size=data_set_size*0.8)

training_set <- ds_vgs_dont_joke[indexes,]
test_set <- ds_vgs_dont_joke[-indexes,]


#set.seed(1337, kind = "Mersenne-Twister", normal.kind = "Inversion")
set.seed(1337)

tree_sizes <- c(50,100,200,300,400,500)
tree_size_OOB <- lapply(tree_sizes, function(x){
  model = randomForest(Global_Sales ~ . -Name, data=training_set, ntree=x, mtry=floor((ncol(ds_vgs_dont_joke)-1)/3), importance = TRUE)
  oob_mse = tail(model$mse,1)
  return(oob_mse)

  })
tree_size_OOB

rf <- randomForest(Global_Sales ~ . -Name, data=training_set, ntree=500, mtry=floor((ncol(ds_vgs_dont_joke)-1)/3), importance = TRUE)

