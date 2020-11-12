library(kknn)

data = read.csv('optdigits.csv', header=FALSE)

###STEP 1 - partition into train/valid/test###
n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.4)) 
train=data[id,] 

id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.3)) 
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

kknn_model_train = kknn(formula=train_labels~., test=test, train=train, k=30, kernel="rectangular")
kknn_model_test = kknn(formula=test_labels~., test=test, train=test, k=30, kernel="rectangular")

fit_train <- fitted(kknn_model_train)
conf_matrix_train = table(fit_train, test_labels)

fit_test <- fitted(kknn_model_test)
conf_matrix_test = table(fit_test, test_labels)

conf_matrix_train
conf_matrix_test



missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

#missclass(conf_matrix_train, conf_matrix_test)
missclass_rate_train = 1-sum(diag(conf_matrix_train)/sum(conf_matrix_train))
missclass_rate_test = 1-sum(diag(conf_matrix_test)/sum(conf_matrix_test))


###STEP 3###

list_of_8 = kknn_model_train$prob[,9]

num = sort(list_of_8, decreasing = FALSE)

num1 = 14
imge=train_data[num1,]
d <- matrix(imge, nrow = 8, byrow = TRUE)
y = matrix(as.numeric(d), nrow = 8)

heatmap(y, Colv=NA, Rowv=NA)
imge2=train[num1,]
imge2$V65
