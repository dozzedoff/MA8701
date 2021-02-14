# Necessary libraries
library(Matrix)
library(glmnet)

set.seed(3012)

# Load the file 


data_set <- read.table("gastroenterology_dataset/data.txt", header= FALSE, sep=",", dec =".", colClasses = "numeric")
df_gl <- as.data.frame(data_set)
df_gl <- as.data.frame(t(df_gl))

# hyperplasic(1)=benign, adenoma(3) + serated(2) =malignant 
# dataset sucks, have to decode 700 different covaraites 


#names(df_gl)[1] <- "lesion name"
names(df_gl)[1] <- "type_of_lesion"
names(df_gl)[2] <- "type_of_light_1_WL_2_NBL"
names(df_gl)[3:168] <- paste0("Textural_feature_AHT",1:166)
names(df_gl)[169:424] <- paste0("Rotational_invariant_LBP", 1:256)
names(df_gl)[425:440] <- paste0("Color_naming", 1:16)
names(df_gl)[441:453] <- paste0("Discriminative_color", 1:13)
names(df_gl)[454:460] <- paste0("Hue", 1:7)
names(df_gl)[461:467] <- paste0("Opponent", 1:7)
names(df_gl)[468:500] <- paste0("Color_gray-level_co-occurrence_matrix", 1:33)
names(df_gl)[501:600] <- paste0("shapeDNA", 1:100)
names(df_gl)[601:700] <- paste0("KPCA", 1:100)

test_ds <- df_gl



# Need to change to two classes for vairable: type of lesion
test_ds$type_of_lesion[which(test_ds$type_of_lesion==1)]<-0
test_ds$type_of_lesion[which(test_ds$type_of_lesion==3)]<-1
test_ds$type_of_lesion[which(test_ds$type_of_lesion==2)]<-1





glm_test = glm(type_of_lesion~.,data=test_ds, family="binomial")

#test_ds$type_of_light_1_WL_2_NBL<- as.factor(test_ds$type_of_light_1_WL_2_NBL)
#test_ds$type_of_lesion<- as.factor(test_ds$type_of_lesion)

xs <- model.matrix(type_of_lesion~.,data=test_ds)[,-1]
xss <- scale(xs)

ys <- test_ds[,1]

lassofit=glmnet(x=xs,y=ys,alpha=1,standardize=TRUE,family="binomial") # already standardized
plot(lassofit,xvar="lambda",label=TRUE)

cv.lasso=cv.glmnet(x=xs,y=ys,alpha=1,standardize=TRUE,family="binomial")
print(paste("The lamda giving the smallest CV error",cv.lasso$lambda.min))

plot(lassofit,xvar="lambda",label=TRUE)
abline(v=log(cv.lasso$lambda.1se))

# Lasso method
resmat=cbind(coef(lassofit,s=cv.lasso$lambda.1se))
colnames(resmat)=c("lasso logistic")
print(resmat)

# Choice of hyperparameters


#Goodness of fit part


