#Akademik Bilişim 2016 - R ile Veri Analizi Kursu
#Eğitmenler: Mustafa Baydoğan ve Berk Orbay

#dplyr ve ggplot2 örnekleri

#Aşağıdaki satırları anlamanız önemli değil
rm(list=ls(all=TRUE)) #Ortamda bulunan bütün değişkenleri yok et. 
gc() #Hafızayı temizle (garbage collection)
options(stringsAsFactors=FALSE) #data.frame yapısında characterleri factor değil character şeklinde tanımla
options(repos="https://cran.rstudio.com/") #R paketlerini indirirken bu depoyu kullan
options(dplyr.width = Inf) #dplyr tablolarının genişliğini tam göster
options(scipen = 7) #Ondalık verilerde bu derinliği kullan

if(exists(".Random.seed")) #Eğer bir rastgelelik tohumu varsa sil
	rm(.Random.seed, envir=globalenv())

#Buradan aşağısı dplyr 
if(!("tidyr" %in% rownames(installed.packages()))) #Eğer paket yüklenmediyse paketi yükle
	install.packages("tidyr")

if(!("plyr" %in% rownames(installed.packages()))) 
	install.packages("plyr")
#dplyr
if(!("dplyr" %in% rownames(installed.packages()))) 
	install.packages("dplyr")
#ggplot2
if(!("ggplot2" %in% rownames(installed.packages())))
	install.packages("ggplot2")

if(!("reshape2" %in% rownames(installed.packages())))
	install.packages("reshape2")
#xlsx dosyalarını yazdırmak için
if(!("xlsx" %in% rownames(installed.packages())))
	install.packages("xlsx")
#xlsx dosyalarını okutmak için
if(!("readxl" %in% rownames(installed.packages())))
	install.packages("readxl")

#paketleri çalıştır
library(tidyr) 
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(xlsx)
library(readxl)

#####
### Yardimci fonksiyonlari calistir
source()
#####

#Veri setimizi olusturalim
sinif<-sinif_olustur() %>% tbl_df

# print(sinif)
#      isim      soyad cinsiyet  sube Kodlama_1 Kodlama_2 Matematik_1 Matematik_2 Tarih_1 Tarih_2
#     (chr)      (chr)    (chr) (chr)     (int)     (int)       (int)       (int)   (int)   (int)
# 1    Öcal      Polat    Erkek    9A        43        75          83          39      42      96
# 2   Ökten      Özbey    Erkek    9A        58        11          22          65      77      16
# 3   Önder   Ekşioğlu    Erkek    9A        83        96          26          83      34      74
# 4  Mavisu      Kıraç      Kız    9A        22        14          30          44      63      94
# 5   Sanat     Paksüt    Erkek    9A       100        87          84          23      63      96
# 6   Özlem Tekelioğlu      Kız    9A        72        15          46          71      98      30
# 7   Petek    Çamdalı      Kız    9A        57        25          18          55      13      24
# 8   Serra    Ayverdi      Kız    9A        38        31          29          42      86      75
# 9  Tonguç      Akyüz    Erkek    9A        88        52          29          61      44      88
# 10  Ilgar   Nebioğlu    Erkek    9A        51        98          73          90      68      92
# ..    ...        ...      ...   ...       ...       ...         ...         ...     ...     ...

###
#select komutu ile ilgili alistirmalar
#select sutunlari secer
###

#isim soyad ve subeyi secelim
sinif %>%
	select(isim,soyad,sube)

#isim soyad ve sube haric hepsini secelim
sinif %>%
	select(-isim,-soyad,-sube)

#kodlama dersi ile tarih dersi arasini secelim
sinif %>%
	select(Kodlama_1:Tarih_2)

#isim soyad ve sube ile sadece birinci sinavlari secelim
sinif %>%
	select(isim,soyad,sube,contains("_1"))

#soyadi soyisim yapalim
sinif %>%
	select(soyisim=soyad)

###
#rename komutu ile ilgili alistirmalar
#rename sutun ismini degistirir
###
#soyadi soyisim yapalim
sinif %>%
	rename(soyisim=soyad)

#soyadi "soy isim" olarak yazalim
sinif %>%
	rename(soy isim=soyad) #hata verecek

#dogrusu
sinif %>%
	dplyr::rename(`soy isim`=soyad)

####
#filter komutu ile ilgili alistirmalar
#filter satirlarin arasindan secim yaptirir
####

#erkek ogrencileri secelim
sinif %>% 
	filter(cinsiyet=="Erkek")

#9A sinifinin kiz ogrencilerini secelim
sinif %>% 
	filter(cinsiyet=="Kız" & sube=="9A")

sinif %>% 
	filter(cinsiyet=="Kız", sube=="9A")

#Kodlamadan 1. sinavda 50 ve uzerinde alanlari secelim
sinif %>%
	filter(Kodlama_1>=50)

#Matematik 1 veya Matematik 2 sinavindan 50 ve uzeri alanlari gosterelim
sinif %>% 
	filter(Matematik_1 >= 50 | Matematik_2 >= 50)

#İsmi Buğra veya Nurgül olanları gösterelim
sinif %>% 
	filter(isim %in% c("Buğra","Nurgül"))

#Kodlamadan 1. sinavda 50 ve uzerinde alanlarin isim, soyad, Kodlama 1 notunun hepsini gosterelim
sinif %>%
	filter(Kodlama_1>=50) %>%
	select(isim,soyad,Kodlama_1) %>%
	print(n=Inf)

#Soyadinda 'oglu' olan herkesi gosterelim
sinif %>% 
	filter(grepl("oğlu",soyad))

#ilk 5 satiri alalim
sinif %>%
	slice(1:5)

#sadece 5. satiri alalim
sinif %>%
	slice(5)

####
#arrange komutu ile ilgili alistirmalar
#arrange satirlari siralar
####

#Isim sirasina gore dizdirelim
sinif %>%
	arrange(isim)

#Tarih 1 notlarina gore buyukten kucuge dizdirelim
sinif %>%
	arrange(desc(Tarih_1)) %>%
	select(isim,soyad,Tarih_1)

#Her subeyi kendi icinde Tarih 1 notlarina gore buyukten kucuge dizdirelim
sinif %>%
	select(isim,soyad,sube,Tarih_1) %>%
	arrange(sube,desc(Tarih_1)) %>%
	print(n=Inf)

####
#mutate komutu ile ilgili alıştırmalar
#mutate sütunların üzerinde işlem yaptırtır
####

#Tarih 1 notundan herkesten 5 puan duselim
sinif %>%
	mutate(Tarih_1=Tarih_1-5)

#Tarih 1 notundan herkesten 5 puan duselim ama yeni sutunda
sinif %>%
	mutate(Tarih_1_yeni=Tarih_1-5) %>% 
	select(isim,soyad,Tarih_1,Tarih_1_yeni)

#Matematik 1'den 50nin altinda olan herkesin notunu 15 arttiralim
sinif %>%
	select(isim,soyad,sube,Matematik_1) %>%
	mutate(Matematik_1_yeni=ifelse(Matematik_1<=50,Matematik_1 + 15,Matematik_1))

#Peki kimsenin hakkini yedik mi? Notu 65 altinda olanlara bakalim 
sinif %>%
	select(isim,soyad,sube,Matematik_1) %>%
	mutate(Matematik_1_yeni=ifelse(Matematik_1<=50,Matematik_1 + 15,Matematik_1)) %>%
	filter(Matematik_1_yeni<=65) %>%
	arrange(desc(Matematik_1))

#Notu 50nin altinda olan herkes not arttirma ile en fazla 50 olsun adalet gelsin
sinif %>%
	select(isim,soyad,sube,Matematik_1) %>%
	mutate(Matematik_1_yeni=ifelse(Matematik_1<=50,pmin(Matematik_1 + 15,50),Matematik_1)) %>%
	filter(Matematik_1_yeni<=65) %>%
	arrange(desc(Matematik_1)) %>%
	print(n=Inf)

#Kodlama'da iki sinavin ortalamasini alalim
sinif %>%
	mutate(Kodlama_ort=(Kodlama_1+Kodlama_2)/2)

#Kodlama'da iki sinavin agirlikli ortalamasini alalim
sinif %>%
	mutate(Kodlama_ort=(Kodlama_1*0.3+Kodlama_2*0.7))

#Kodlama dersinden 40 ustu alanlari gecirelim
sinif %>%
	mutate(Kodlama_ort=(Kodlama_1*0.3+Kodlama_2*0.7),
		Kodlama_sonuc=ifelse(Kodlama_ort>=40,"Geçti","Kaldı"))  %>%
	select(-cinsiyet,-sube)

#Kodlama dersinden 40 ustu alanlari gecirelim
sinif %>%
	mutate(Kodlama_ort=(Kodlama_1*0.3+Kodlama_2*0.7)) %>%
	mutate(Kodlama_sonuc=ifelse(Kodlama_ort>=40,"Geçti","Kaldı")) %>%
	select(-cinsiyet,-sube)

#Kodlama dersinin not ortalamalarini tek basina alalim 
#Transmute komutu
sinif %>%
	transmute(Kodlama_ort=(Kodlama_1*0.3+Kodlama_2*0.7))

#Transmute komutu ve histogram cizdirme
sinif %>%
	transmute(Kodlama_ort=(Kodlama_1*0.3+Kodlama_2*0.7)) %>% unlist %>% hist()

###
#summarise ve group_by komutlari
#summarise komutu ile pivot tablolari yapabiliriz
###

#Tarih 1 ve Tarih 2 sinavlarinin ortalamasini alalim
sinif %>%
	summarise(Tarih_1_ort=round(mean(Tarih_1),5),Tarih_2_ort=round(mean(Tarih_2),5))

#Tarih 1 ve Tarih 2 sinavlarinin subelere gore ortalamasini alalim
sinif %>% 
	group_by(sube) %>%
	summarise(Tarih_1_ort=round(mean(Tarih_1),2),Tarih_2_ort=round(mean(Tarih_2),2))

#Tarih 1 ve Tarih 2 sinavlarinin subelere ve cinsiyete gore ortalamasini alalim
sinif %>% 
	group_by(sube,cinsiyet) %>%
	summarise(Tarih_1_ort=round(mean(Tarih_1),2),Tarih_2_ort=round(mean(Tarih_2),2))

#Sinif mevcutlarina bakalim
sinif %>%
	group_by(sube) %>%
	summarise(sinif_mevcudu=n())

#Siniflardaki kiz erkek dagilimlarina bakalim
sinif %>%
	group_by(sube,cinsiyet) %>%
	summarise(sinif_mevcudu=n())

#Matematik sinavlarinin sube ve cinsiyet bazinda en dusuk ve en yuksek skorlarina bakalim
sinif %>% 
	group_by(sube,cinsiyet) %>%
	summarise(Matematik_1_min=min(Matematik_1),Matematik_1_max=max(Matematik_1),Matematik_2_min=min(Matematik_2),Matematik_2_max=max(Matematik_2))


#sinifta Kodlama_1den 30'un uzerinde alan ve Kodlama_2'de 40'in uzerinde alan kiz ogrencilerin not ortalamasi

sinif %>% 
	filter(cinsiyet=="Kız",Kodlama_1 > 30 & Kodlama_2 > 40 ) %>% 
	summarise(kodlama_1_ort=mean(Kodlama_1),kodlama_2_ort=mean(Kodlama_2))


#Matematikten 1. sinavda 30 ile 50 veya 2. sinavda 50 ile 80 arasi alanlarin ortalamasi 65in uzerinde olanlari gecti olmayanlari kaldi diye belirtelim.





