set.seed(1337)
library(fastDummies)
library(corrplot)
library(tidyverse)



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


# Three extra rows with same name have showed up after imputing, removing these
ds_vgs_updt <- subset(ds_vgs_updt, ds_vgs_updt$Name != "Steins")


# Formating of missing values, setting everything to NA, to be imputed at the next step
ds_vgs_updt$User_Score[ds_vgs_updt$User_Score=="tbd"] <- NA
ds_vgs_updt$User_Score[ds_vgs_updt$User_Score==""] <- NA
ds_vgs_updt$User_Score <- as.numeric(ds_vgs_updt$User_Score)
ds_vgs_imputed <- ds_vgs_updt


# Impute values for games that have critics score, but not user score, and the other way around
# Imputing values happens by taking the mean of corresponding values that exist for similar range of user/critic score, depending on which one is applicable


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


# Changing category for unknown publishers and developers
ds_vgs_imputed$Publisher[ds_vgs_imputed$Publisher == "N/A"] <- "Unknown"
ds_vgs_imputed$Developer[ds_vgs_imputed$Developer == ""] <- "Unknown"

# Merging rating categories, according to current standards
ds_vgs_imputed$Rating[ds_vgs_imputed$Rating== "K-A"] <-"E"
ds_vgs_imputed$Rating[ds_vgs_imputed$Rating== "RP"] <-"Unknown"
ds_vgs_imputed$Rating[ds_vgs_imputed$Rating== ""] <-"Unknown"
ds_vgs_imputed$Rating[ds_vgs_imputed$Rating== "EC"] <-"E"


write.csv(ds_vgs_imputed, "vgs_22_2016_expl_analysis.csv")
#ds_vgs_imputed_with_name <- ds_vgs_imputed 
#ds_vgs_imputed <- subset(ds_vgs_imputed, select = -c(Name))


# Dummy coding, two different versions
ds_vgs_imputed_no_dummy_genre_platform <- dummy_cols(ds_vgs_imputed, select_columns = c("Publisher", "Developer"))

ds_vgs_final_2 <- subset(ds_vgs_imputed_no_dummy_genre_platform, select = -c(X, Publisher, Developer))


ds_vgs_imputed_test <- dummy_cols(ds_vgs_imputed, 
                                  select_columns = c("Publisher","Developer","Platform","Genre"))

ds_vgs_final <- subset(ds_vgs_imputed_test, select = -c(X, Publisher, Developer, Platform, Genre))
