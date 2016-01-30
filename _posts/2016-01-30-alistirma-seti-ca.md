---
layout: post
title: Alıştırma Seti Cevap Anahtarı
date: 2016-01-30 20:00:00 +02:00
---

Aşağıdaki kodlar bugünkü alıştırma setinin cevap anahtarı olarak düşünülebilir.

```{r}

#store plots in pdf, will save in Documents folder if you are on windows
#and do not specify a path for the file
#text results will be printed on R terminal window

pdf("plots.pdf",width=7,height=5)
### Exercise 1 ###
###            ###
q1rand=rexp(1500,1/3)
par(mfrow=c(1,2))
hist(q1rand,main='Histogram for Exercise 1')
boxplot(q1rand,main='Boxplot for Exercise 1')

### Exercise 2 ###
###            ###
#(part a)
nofrandom=100
q2rand=runif(nofrandom,3,10)
percentage=100*sum(q2rand>5)/nofrandom
sampmean=mean(q2rand)
sampmedian=median(q2rand)
print(c(percentage,sampmean,sampmedian))

#(part b)
#population 
prob=1-punif(5,3,10)
popmean=0.5*(3+10)
popmedian=popmean #symmetry
print(c(100*prob,popmean,popmedian))

#(part c)
nofrandom=1000
q2rand=runif(nofrandom,3,10)
percentage=100*sum(q2rand>5)/nofrandom
sampmean=mean(q2rand)
sampmedian=median(q2rand)
print(c(percentage,sampmean,sampmedian))
print(c(100*prob,popmean,popmedian))

### Exercise 3 ###
###            ###
#(part a)
nofrandom=2000
q4rand=rnorm(nofrandom,8,5)
#(part b)
nofgreater=sum(q4rand>=9)
print(nofgreater)
#(part c)
sampmean=mean(q4rand)
sampstdev=sd(q4rand)
print(c(sampmean,sampstdev))
#(part d)
#population 
percentile25=qnorm(0.25,8,5)
percentile75=qnorm(0.75,8,5)
print(c(percentile25,percentile75))
#(part e)
orderedsample=sort(q4rand)
samplepercentile25=(orderedsample[500]+orderedsample[501])/2
samplepercentile75=(orderedsample[1500]+orderedsample[1501])/2
print(c(samplepercentile25,samplepercentile75))
#(part f)
print(c(pnorm(0.789),pnorm(-0.543))) #default values imply standard normal, check help!

### Exercise 3 ###
###            ###
#(part a)
nofdim=c(2,3)
nofrandom=1000
fraction=array(0,nofdim)
par(mfrow=c(1,1))
for(k in nofdim){
	q3rand=runif(k*nofrandom,-1,1)
	q3rand=matrix(q3rand,nrow=nofrandom) #matrix form
	distances=apply(q3rand,1,function(x) { sqrt(sum(x^2))}) #apply to each row
	fraction[k]=sum(distances<1)/nofrandom
}
plot(c(1:nofdim),fraction,type='l',lty=2,lwd=2,col=2,xlab='# of dimensions',ylab='fraction',main='Exercise 3')
points(c(1:nofdim),fraction,pch=2,cex=1)

#(part b)
#fraction[2] is the ratio of the areas of circle with radius of 1 and the square with side length of 2
#fraction[3] is the ratio of the volumes of circle with radius of 1 and the cube with side length of 2
approximatedpi2D=fraction[2]*4/1
approximatedpi3D=fraction[3]*8/(4/3)
print(c(pi,approximatedpi2D,approximatedpi3D))

### Exercise 5 ###
###            ###
#(part a and b)
nlevel=c(2,5,10,30,50,100)
nofsampling=1000
par(mfrow=c(2,3))
for(i in nlevel){
	q5rand=rpois(i*nofsampling,3)
	q5rand=matrix(q5rand,nrow=nofsampling) #matrix form
	means=apply(q5rand,1,mean)
	hist(means,main=paste('n=',i))
} #as n gets larger the distribution of mean is closer to normal distribution

#(part c)
par(mfrow=c(1,1))
qqnorm(means, ylab="Standardized Scores", xlab="Normal Scores", main="Exercise 5 Part c") 
qqline(means)
dev.off()

```

