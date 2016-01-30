---
layout: post
title: Birinci Gün Ders Kodları
date: 2016-01-30 20:30:00 +02:00
---

Aşağıdaki kodlar bugün derste işlenen konuları içeriyor.

Birincı kısım

```{r}
a=49
str(a)
a='abc'
a="a"
a[2]=51
a[10]='k'

b=a[1:5]
b
d=as.numeric(b)
d
e=a[-10]
e
ind=-c(1:5)
ind
a=a[ind]

x=c(1,5,3,6,7)

ornekdata=data.frame(Yas=c(1:5),
          X=c('a','b','c','d','e'))


write.csv(ornekdata,'ornek.csv')

path='C:/mustafa/Presentation/AB2016/Gün1'
setwd(path)

ornekdata[,2:3]

yeni=5*(1:5)

ornekdata[c(1,3),]

```

İkinci kısım

```{r}
carptopla <- function(x,y){
  toplam=x+y
  carpim=x*y
  print(x)
  #return(c(toplam,carpim)) 
  return(list(Toplam=toplam,
              Carpim=carpim))
}

a=carptopla(3,7)
a$Toplam
help(read.table)
```

Üçüncü kısım

```{r}
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
```