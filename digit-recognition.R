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
test_labels=test[, 65]

kknn_model = kknn(formula=train_labels~., test=test, train=train, k=3, kernel="rectangular")
kknn_model$prob
fit <- fitted(kknn_model)
conf_matrix = table(test_labels, fit)
conf_matrix
#confusionmatrix_kknn = table(test_labels,kknn_model[["fitted.values"]]) 
#confusionmatrix_kknn


#missclass=function(X,X1){n=length(X)return(1-sum(diag(table(X,X1)))/n)}