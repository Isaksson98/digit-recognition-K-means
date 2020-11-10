data = read.csv('optdigits.csv', header=TRUE)

spec = c(train = .5, test = .25, validate = .25)

g = sample(cut(
  seq(nrow(df)), 
  nrow(df)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(df, g)
train = data[id,]
test = data[-id,]
validation = data[-id,]
