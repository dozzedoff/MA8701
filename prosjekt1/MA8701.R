# Necessary libraries
library(Matrix)
library(glmnet)
library(gglasso)

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

# We have a look at the different representatives of the "clusters" of variables
summary(test_ds[,c(1,2,3,169,425,441,454,461,468,501,601)])


# logistic regression does not make sense, it refuses to provide meaningful output, presumably 
# due to the large number of covariates

#test_ds$type_of_light_1_WL_2_NBL<- as.factor(test_ds$type_of_light_1_WL_2_NBL)
#test_ds$type_of_lesion<- as.factor(test_ds$type_of_lesion)

x_no_zero <- model.matrix(type_of_lesion~.,data=test_ds)[,-1]
x_no_zero_scale_1 <- scale(x_no_zero)
x_no_zero_scale_1[is.nan(x_no_zero_scale_1)] <- 0
#x_no_zero_new <- x_no_zero[,-(which(colSums(x_no_zero)==0))]
#stdz_xs <- scale(x_no_zero_new)

ys <- test_ds[,1]


#data_glm <- data.frame(ys,stdz_xs)
#colnames(data_glm)[1] <- "type_of_lesion"
#glm_test = glm(type_of_lesion~.,data=data_glm, family="binomial",control = list(maxit = 50))
x_no_zero_scale <- cbind(1,x_no_zero_scale_1)

index_s <- c(1,rep(2,166),rep(3,256),rep(3,16),rep(4,13), rep(5,7), rep(6,7), rep(7,33),rep(8,100),rep(9,100))
#lmax <- lambdamax(x=x_no_zero_scale, y=ys,index=index_s,penscale=sqrt, model = LogReg(), center = FALSE,
#                  standardize = FALSE)*0.5^(0:699)
#grplasso_test <- grplasso(x_no_zero_scale, ys, model = LogReg(), penscale=sqrt, center = FALSE, standardize = FALSE, lambda=lmax, index=index_s)
ys_gl <- ys
ys_gl[which(ys_gl==0)]<- -1


cv_testing_gglasso <- cv.gglasso(x=x_no_zero_scale_1, y=ys_gl, group=index_s, nfolds=5, loss="logit")
plot(cv_testing_gglasso)

group_lasso_coef <- coef(cv_testing_gglasso$gglasso.fit, s=cv_testing_gglasso$lambda.1se)
coef_value_gglasso <- group_lasso_coef[which(group_lasso_coef!=0)]
coef_name_gglasso <- row.names(group_lasso_coef)[which(group_lasso_coef!=0)]
# KPCA, color gray level co-occurence, textural feature, type of light




lassofit=glmnet(x=x_no_zero_scale_1,y=ys,alpha=1,standardize=FALSE,family="binomial") # already standardized
plot(lassofit,xvar="lambda",label=TRUE)

cv.lasso=cv.glmnet(x=x_no_zero_scale_1,y=ys,alpha=1,standardize=FALSE,family="binomial")
print(paste("The lamda giving the smallest CV error",cv.lasso$lambda.min))

plot(lassofit,xvar="lambda",label=TRUE)
abline(v=log(cv.lasso$lambda.1se))
plot(cv.lasso)

# Lasso method

lasso_coef <- coef(lassofit,s=cv.lasso$lambda.1se)
lasso_coef_print <- cbind(names(test_ds[lasso_coef@i+1]),lasso_coef@x)
lasso_coef_print[1,1] <- "Intercept"
lasso_coef_print

#resmat=cbind(coef(lassofit,s=cv.lasso$lambda.1se))
#colnames(resmat)=c("lasso logistic")


# Choice of hyperparameters


#Goodness of fit part


