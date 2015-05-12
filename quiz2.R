library(datasets)
data(iris)
?iris

# There will be an object called 'iris' in your workspace. 
# In this dataset, what is the mean of 'Sepal.Length' for 
# the species virginica? 

   s<-split(iris,iris$Species)
   answer<- apply(s$virginica[,1:4],2,mean)



library(datasets)
data(mtcars)




x<-matrix(rnorm(200),20,10)

apply(x,1,quantile,probs=c(0.25,0.75))

a<-array(rnorm(2*2*10),c(2,2,10))
apply(a,c(1,2),mean)

# or
rowMeans(a,dims=2)
# colMeans(a,dims=2)
# 
# !is.vector
# !is.object
# !is.character
# !is.matrix
# !is.list
# !is.numeric
# !is.array

x<-list(a=matrix(1:4,2,2),b=matrix(1:6,3,2))
lapply(x,function(y) y[,1])
sapply(x,function(y) y[,1])




