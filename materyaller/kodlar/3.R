veri=matrix(rnorm(100000000),100000,100)
str(veri)

hist(veri[,1])

write.csv(veri,'veri.csv',row.names=F)

baslangic=Sys.time()
veri=read.csv('veri.csv')
bitis=Sys.time()
print(bitis-baslangic)

#data.table
install.packages('data.table')
require(data.table)

n=1500
r=1/3
vek=rexp(n,r)
hist(vek)
boxplot(vek)


n=1000
x=runif(n,-1,1)
y=runif(n,-1,1)

plot(x,y)
n=100000
vmat=matrix(runif(2*n,-1,1),n,2)
plot(vmat[,1],vmat[,2])
n=100000
vmat=matrix(runif(2*n,-1,1),n,2)
binvek=sqrt(vmat[,1]^2+vmat[,2]^2)<=1
#sum(binvek)
ind=which(binvek)
#plot(vmat[ind,1],vmat[ind,2])

plot(vmat[,1],vmat[,2],main='Kare ve Daire')
points(vmat[ind,1],vmat[ind,2],col='red')
tahminipi=4*sum(binvek)/n
print(tahminipi)
print(pi)