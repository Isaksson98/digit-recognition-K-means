data = read.csv('parkinsons.csv')
data_scaled = scale(data)

training_quota = 0.6
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n * training_quota))
train=data[id,]
test=data[-id,]
