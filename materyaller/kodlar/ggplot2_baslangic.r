##ggplot2 ornekleri

options(stringsAsFactors=FALSE) #data.frame yapısında characterleri factor değil character şeklinde tanımla
options(dplyr.width = Inf) #dplyr tablolarının genişliğini tam göster
options(scipen = 7) #Ondalık verilerde bu derinliği kullan

# install.packages("dplyr")
# install.packages("ggplot2")

library(dplyr)
library(ggplot2)


#once on fonksiyonlari yukleyelim

#Veri setimizi olusturalim
sinif<-sinif_olustur() %>% tbl_df

#Bu bos grafik cikaracaktir
ggplot(data=sinif)

#Matematik_1 ve Matematik_2'de notlarin dagilimi
ggplot(data=sinif) + geom_point(aes(x=Matematik_1,y=Matematik_2))
#Boyle de yazilir
ggplot(data=sinif,aes(x=Matematik_1,y=Matematik_2)) + geom_point()


#Matematik_1 ve Matematik_2'de notlarin dagilimi kiz erkek ayrimi
ggplot(data=sinif) + geom_point(aes(x=Matematik_1,y=Matematik_2,color=cinsiyet))

ggplot(data=sinif) + geom_point(aes(x=Matematik_1,y=Matematik_2,color=cinsiyet,shape=sube))

ggplot(data=sinif) + geom_point(aes(x=Matematik_1,y=Matematik_2,color=cinsiyet,shape=sube), size=4)

ggplot(data=sinif) + geom_point(aes(x=Matematik_1,y=Matematik_2,color=cinsiyet,shape=sube,alpha=Tarih_1), size=4)

ggplot(data=sinif %>% filter(Matematik_1>=50 & Matematik_2 >= 50)) + geom_point(aes(x=Matematik_1,y=Matematik_2,color=cinsiyet,shape=sube), size=4)

ggplot(data=sinif) + geom_histogram(aes(Matematik_1),binwidth=1)

ozet1<-sinif %>% group_by(sube,cinsiyet) %>% summarise(Matematik_1=mean(Matematik_1)) %>% ungroup %>% mutate(sube_cinsiyet=paste0(sube,"_",cinsiyet))

ggplot(data=ozet1,aes(x=sube_cinsiyet,y=Matematik_1)) + geom_bar(stat="identity",aes(fill=sube))



df<-data.frame(gun=1:50,
				deger=cumsum(runif(50,0,1)),
				deger2=cumsum(runif(50,0,1))) %>% tbl_df


ggplot(data=df,aes(x=gun)) + geom_line(aes(y=deger),color="red") + geom_line(aes(y=deger2),color="blue")








