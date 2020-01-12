A<-matrix(1,2,3)
B<-as.data.frame(A)

set.seed(1)
n<-100
x<-runif(n,0,1)
y<-rnorm(n,0,1)
A<-matrix(c(x,y),n,2,byrow = F)
B<-as.data.frame(A)
colnames(B)<-c("Uniform","Gauss")
B[13,1]
B$Uniform[13]
plot(B$Uniform,B$Gauss)


summary(B$Gauss)


plot.ts(y)
hist(x,200)

hist(x)
hist(y)