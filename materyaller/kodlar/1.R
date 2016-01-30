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
