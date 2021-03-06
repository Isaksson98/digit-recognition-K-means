library(kknn)
library(caret)

data = read.csv('optdigits.csv', header=FALSE)

###STEP 1 - partition into train/valid/test###
n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 

id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
valid=data[id2,]

id3=setdiff(id1,id2)
test=data[id3,] 

###STEP 2###
#Remove the label element 65 on each row
test_data=test[, 1:64]
train_data=train[, 1:64]
#train_labels=train[, 65]
train_labels=as.factor(train$V65)
test_labels=as.factor(test$V65)
valid_labels=as.factor(valid$V65)

kknn_model_train = kknn(formula=train_labels~., test=train, train=train, k=30, kernel="rectangular")
kknn_model_test = kknn(formula=train_labels~., test=test, train=train, k=30, kernel="rectangular")

fit_train <- fitted(kknn_model_train)
conf_matrix_train = table(fit_train, train_labels)

fit_test <- fitted(kknn_model_test)
conf_matrix_test = table(fit_test, test_labels)

conf_matrix_train
conf_matrix_test
#cfm <- confusionMatrix(fit_train, test_labels)
#cfm

missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

missclass_rate_test = missclass(fit_test, test_labels)
missclass_rate_train = missclass(fit_train, train_labels)

missclass_rate_train
missclass_rate_test

#num4 = mean(fit_train != test_labels)
#num4
###STEP 3###


#probs = kknn_model_train$prob

#Row 7, 8 & 15 is low
#Row 49, 50 is high
num1 = 8 # Row to display
imge=c(test[num1,1:64])
#d <- matrix(imge, nrow = 8, byrow = TRUE)
y = matrix(as.numeric(imge),nrow= 8)
#y = matrix(rnorm(8*8))
y=t(y)
y
heatmap(y, Colv=NA, Rowv=NA)
#What it's suppose to be
test[num1,]$V65
#7 is 3
#8 is 9
#15 is 3
#49&50 is 8
###STEP 4###
m <- vector("integer", 30)
s <- vector("integer", 30)

for(k in 1:30){
  kknn_model_loop_m = kknn(formula=train_labels~., test=train, train=train, k=k, kernel="rectangular")
  fit_train_loop_m <- fitted(kknn_model_loop_m)
  m[k] <- missclass(fit_train_loop_m, train_labels)

  kknn_model_loop_s = kknn(formula=train_labels~., test=valid, train=train, k=k, kernel="rectangular")
  fit_train_loop_s <- fitted(kknn_model_loop_s)
  s[k] <- missclass(fit_train_loop_s, valid_labels)
}
plot( s, type='l', col='orange', main='Missclassification',
     xlab='k', ylab='Missclassification error')
lines( m, col='blue')

m
s

###STEP 5###

#kknn_model_loop_m$prob

epsilon=1.0e-15

H_valid <- vector("integer", 30)

#Looping same again for step 5 question
for(k_i in 1:30){
  kknn_model_loop_m2 = kknn(formula=train_labels~., test=valid, train=train, k=k_i, kernel="rectangular")
  
  fit_train_loop_m2 <- fitted(kknn_model_loop_m2)
  
  for(i in 1:nrow(kknn_model_loop_m2$prob)){
    p1 = max(kknn_model_loop_m2$prob[i,valid[i,65]+1])
    H_valid[k_i] = H_valid[k_i] -log(p1+epsilon)
  }
  
}

plot( H_valid, type='l', col='green', main='Empirical loss',
     xlab='k', ylab='Empirical loss')

H_valid

#Cross entropy is more suitable because it factors in the 
# probability for each guess. say 90% sure instead of 70% is better








