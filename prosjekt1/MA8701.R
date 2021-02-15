# Necessary libraries
library(Matrix)
library(glmnet)
library(grplasso)
library(gglasso)

set.seed(3012)


############# Dataset cleaning ###############
# Loading the file and transposing it due to strange loading
data_set <- read.table("gastroenterology_dataset/data.txt", header= FALSE, sep=",", dec =".", colClasses = "numeric")
df_gl <- as.data.frame(data_set)
df_gl <- as.data.frame(t(df_gl))

# Original types:
# hyperplasic(1)=benign, adenoma(3) + serated(2) =malignant 
# Classes adenoma and serated have been merged to malignant class
# dataset sucks, have to decode 700 different covaraites 

# Rotationally invariant LBP - rot_invariant_LBP
# Color gray-level co-occurence matrix - col_gray_lvl_co-occurr_mx
# names(df_gl)[1] <- "lesion name"
names(df_gl)[1] <- "type_of_lesion"
names(df_gl)[2] <- "type_of_light_1_WL_2_NBL"
#names(df_gl)[3:168] <- paste0("Textural_feature_AHT",1:166)
names(df_gl)[3:168] <- paste0("TFAHT",1:166)
#names(df_gl)[169:424] <- paste0("Rot_invariant_LBP", 1:256)
names(df_gl)[169:424] <- paste0("RILBP", 1:256)
names(df_gl)[425:440] <- paste0("Color_naming", 1:16)
names(df_gl)[441:453] <- paste0("Discriminative_color", 1:13)
names(df_gl)[454:460] <- paste0("Hue", 1:7)
names(df_gl)[461:467] <- paste0("Opponent", 1:7)
#names(df_gl)[468:500] <- paste0("Col_gray_lvl_co-occurr_mx", 1:33)
names(df_gl)[468:500] <- paste0("CGlvl", 1:33)
names(df_gl)[501:600] <- paste0("shapeDNA", 1:100)
names(df_gl)[601:700] <- paste0("KPCA", 1:100)



# Set it to a new variable due to practical (testing) reasons
test_ds <- df_gl



# Need to change to two classes for vairable: type of lesion
test_ds$type_of_lesion[which(test_ds$type_of_lesion==1)]<-0
test_ds$type_of_lesion[which(test_ds$type_of_lesion==3)]<-1
test_ds$type_of_lesion[which(test_ds$type_of_lesion==2)]<-1

# We have a look at the different representatives of the "clusters" of variables
summary(test_ds[,c(1,2,3,169,425,441,454,461,468,501,601)])



#test_ds$type_of_light_1_WL_2_NBL<- as.factor(test_ds$type_of_light_1_WL_2_NBL)
#test_ds$type_of_lesion<- as.factor(test_ds$type_of_lesion)


# Making a model matrix, scaling and setting all nan-s to zero (as some columns are zero, and scaling
# that produces nan)
x_no_zero <- model.matrix(type_of_lesion~.,data=test_ds)[,-1]
x_no_zero_scale_1 <- scale(x_no_zero)
x_no_zero_scale_1[is.nan(x_no_zero_scale_1)] <- 0
#x_no_zero_new <- x_no_zero[,-(which(colSums(x_no_zero)==0))]
#stdz_xs <- scale(x_no_zero_new)

# response
ys <- test_ds[,1]

# logistic regression does not make sense, it refuses to provide meaningful output, presumably 
# due to the large number of covariates

#data_glm <- data.frame(ys,stdz_xs)
#colnames(data_glm)[1] <- "type_of_lesion"
#glm_test = glm(type_of_lesion~.,data=data_glm, family="binomial",control = list(maxit = 50))

# adding intercept for lasso from grplasso
x_no_zero_scale <- cbind(1,x_no_zero_scale_1)

# grouping of covariates for group lasso
index_s <- c(1,rep(2,166),rep(3,256),rep(3,16),rep(4,13), rep(5,7), rep(6,7), rep(7,33),rep(8,100),rep(9,100))

##### group lasso from grplasso #####
#lmax <- lambdamax(x=x_no_zero_scale, y=ys,index=index_s,penscale=sqrt, model = LogReg(), center = FALSE,
#                  standardize = FALSE)*0.5^(0:699)
#grplasso_test <- grplasso(x_no_zero_scale, ys, model = LogReg(), penscale=sqrt, center = FALSE, standardize = FALSE, lambda=lmax, index=index_s)

# needs the reponse to be on the form -1 and 1
ys_gl <- ys
ys_gl[which(ys_gl==0)]<- -1

set.seed(3012)
cv_testing_gglasso <- cv.gglasso(x=x_no_zero_scale_1, y=ys_gl, group=index_s, nfolds=5, loss="logit")
plot(cv_testing_gglasso)

group_lasso_coef <- coef(cv_testing_gglasso$gglasso.fit, s=cv_testing_gglasso$lambda.1se)
coef_value_gglasso <- group_lasso_coef[which(group_lasso_coef!=0)]
coef_name_gglasso <- row.names(group_lasso_coef)[which(group_lasso_coef!=0)]
# KPCA, color gray level co-occurence, textural feature, type of light
print_coef_gglasso <- rbind(coef_name_gglasso[2],coef_name_gglasso[3], coef_name_gglasso[169],coef_name_gglasso[202])
print_coef_gglasso

#### lasso ####

lassofit=glmnet(x=x_no_zero_scale_1,y=ys,alpha=1,standardize=FALSE,family="binomial") # already standardized
plot(lassofit,xvar="lambda",label=TRUE)
mtext("Lasso", font=2,side = 3, line = -2, outer = TRUE)



cv.lasso=cv.glmnet(x=x_no_zero_scale_1,y=ys,alpha=1,standardize=FALSE,family="binomial")
print(paste("The lamda giving the smallest CV error",cv.lasso$lambda.min))

plot(lassofit,xvar="lambda",label=TRUE)
abline(v=log(cv.lasso$lambda.1se))
abline(v=log(cv.lasso$lambda.min))
plot(cv.lasso)

# Lasso method results of coefficients picked, nice print-out

lasso_coef <- coef(lassofit,s=cv.lasso$lambda.1se)
lasso_coef_print <- cbind(names(test_ds[lasso_coef@i+1]),lasso_coef@x)
lasso_coef_print[1,1] <- "Intercept"
lasso_coef_print

#resmat=cbind(coef(lassofit,s=cv.lasso$lambda.1se))
#colnames(resmat)=c("lasso logistic")




# bootstrap
set.seed(3012)
B=1000
n=nrow(x_no_zero_scale_1)
p=ncol(x_no_zero_scale_1)
lasso_mx=matrix(ncol=p+1,nrow=B)
# no need or separate function for steps 1-6 since can use cv.glmnet
# and weight argument for giving the new bootstrapped data
for (b in 1:B)
{
  print(b)
  idx=sort(sample(1:n,replace=TRUE))
  wids=rep(0,n)
  for (i in 1:n)
    wids[i]=sum(idx==i)
  boot=cv.glmnet(x_no_zero_scale_1,ys,weights=wids)
  lasso_mx[b,]=as.vector(coef(boot)) #automatic lambda 1sd
}

investigate_mx <- lasso_mx[,which(lasso_coef != 0)]
colnames(investigate_mx)=c(lasso_coef_print[,1])

boxplot(investigate_mx)
boxplot(investigate_mx[,2:8])

lasso0perc=apply(abs(investigate_mx)<.Machine$double.eps,2,mean)
barplot(lasso0perc)




