---
layout: post
title: Alıştırma Seti
date: 2016-01-30 12:00:00 +02:00
---

Teorik dağılımlar ile örneklemler üzerinden istatistiklerin ilişkileri

###Alıştırma 1
a)	1500 uzunluğunda parametresi  <script type="math/tex">\lambda = 1/3</script> olan bir üssel (exponential) dağılımdan 1500 uzunluğunda bir vektör yaratalım.

b)	Bu vektörün çubuk grafiğini (histogram) ve kutu grafiğini (boxplot) çizelim.

###Alıştırma 2

a) 3 ile 10 arasında tekdüze (uniform) dağılımdan gelen 100 tane sayı yaratalım ve bu sayıların özet (summary) istatistiklerini hesaplayalım. 

b) Oluşturduğunuz vektörde 5’ten büyük sayılar tüm sayıların yüzde kaçını oluşturur?

c) 3 ile 10 arasında tekdüze dağılımın teorik olarak 5’ten büyük olma olasılığı nedir? a şıkkında bulunan sayı ile bu olasılığı karşılatırın. 

d) a şıkkında hesapladığımız yüzdeyi 100 yerine 1000 tane rastgele üretilmiş sayı üzerinden hesaplayalım ve sonuçları karşılaştıralım. Ne gibi bir sonuca varıyorsunuz?

###Alıştırma 3
a)	Ortalaması 8, varyansı 25 olan bir Normal dağılımdan 2000 tane rastgele sayı üretelim.

b)	Oluşturduğunuz vektörün yüzde kaçı 9’dan büyük ya da eşittir?.  

c)	Ürettiğiniz 2000 sayının örneklem ortalaması ve standart sapması nedir? Bu değerleri dağılımın ortalaması ve standart sapması ile karşılaştırın.

d)	Ortalaması 8, varyansı 25 olan bir Normal dağılımın teorik yirmeştebirlik (25th percentile) ve yetmişbeştebirlik (75th percentile) değerleri nedir?

e)	Oluşturduğunuz 2000 sayının özet istatiklerini çıkaralım. Bu değerlerden yirmeştebirlik ve yetmişbeştebirlikleri teorik değerler ile karşılaştıralım.

Döngüler, koşullar ve bunların benzetim (simülasyon) amaçlı kullanımı

###Alıştırma 4

Bu alıştırmada veri analizinde veri boyutunun büyümesiyle veriyi açıklamanın zorluğunun simülasyonunu yapacağız. Bunu öncelikle bir karenin alanını bir dairenin alanı ile karşılaştırarak yapacağız. Daha sonra ise aynı karşılaştırmayı bir küp ve küre ile tekrarlayacağız. Küp ele alındığında 3 boyuta geçtiğimiz için bu karşılatırmayı hacimler cinsinden yapacağız.

a) Bir karenin alanine iki boyutta yaratacağımız rastgele sayılar ile ifade edelim. Diyelim ki rastgele -1 ile 1 arası tekdüze dağılımdan gelen 1000 rastgele sayı üretelim. Bunlar x kordinatımızı ifade etsin. Aynı işlemi bird aha tekrarlayıp 1000 uzunluğunda bir vektör daha elde edelim. Bunlar da y kordinatımızı ifade etsin. Bu noktaları x-y kordinatları şeklinde plot fonksiyonunu kullanarak çizelim. Gözleminiz nedir? Bir karenin alanını 1000 tane sayı ile ifade etmiş olduk mu?

b) Oluşturduğunuz noktalardan kaç tanesinin (0,0) noktasına olan Öklit uzaklığı bir ya da birden küçüktür. Bu noktaları bulun, kareyi simüle eden noktalar içinden bu koşulu sağlayanları kırmızı ile boyayın (önceki grafiğin üzerinde). Koşulu sağlayan noktaların sayısına k diyelim.

c) Bu noktaların oranının (yani k/1000) dairenin alanının karenin alanına oranına eşit olması gerekir. Diyelim ki daire formülünü biliyoruz  <script type="math/tex">A = \pi r^2</script> ama gerçek \pi  değerini bilmiyoruz. Noktalarının oranını \pi  değerinin tahmin etmek için kullanabiliriz. Tahmini \pi  değerini hesaplayınız ve gerçek \pi  değeri ile kıyaslayınız. Daire alanı <script type="math/tex">A=\pi r^2</script> ve kare alanı <script type="math/tex">A=x^2</script>, r dairenin yarıçapını, x Karenin bir kenarının uzunluğunu temsil etmekte.

d) Yukardaki işlemi aynı şekilde 1000 tane daha rastgele sayı üreterek yapalım. Bu da z kordinatımızı ifade etsin. Dolayısıyla şu anda bir küpün hacmini 1000 tane sayıyla ifade etmiş olduk. Benzer şekilde (0,0,0) noktasına uzaklığı bire eşit ya da küçük olan noktalar bir küreyi temsil eder. Benzer <script type="math/tex">\pi</script>  hesabını kürenin alanının küpün alanına oranını kullanarak yapalım. Küre hacmi <script type="math/tex">V=4/3 \pi r^3</script> ve küp hacmi <script type="math/tex">V=x^3</script>, <script type="math/tex">r</script> kürenin yarıçapını, <script type="math/tex">x</script> küpün bir kenarının uzunluğunu temsil etmekte


Hesapladığınız değerleri karşılaştırdığınızda örneklem sayısı sabit tutulduğu düşünülürse 2 boyutlu (değişkenli) bir veriden 3 boyutlu (değişkenli)  bir veriye çıktığınızda tahminlerinizin kötüleştiğini göstericektir. Bu veri analizi ya veri madenciliğinde boyut büyüklüğünün laneti (curse of dimensionality) diye bilinir.

###Alıştırma 5
Bu alıştırmada matrixler üzerinde çeşitli operasyonları denemek temel amaçtır. Bunu merkezi limit teoremini ampirik olarak göstererek yapacağız. Merkezi limit teoremini hatırlayacak olursak bize şöyle der:
Birbirinden bağımsız ve aynı olasılık dağılımı ile dağılan sayıların (independently and identically distributed) n tane sayının ortalaması Normal dağılıma yakınsar. Bu genellikle n 30’dan büyük iken geçerlidir.

Bunun için 1000 tane örneklem yaratacağız. Her örneklem n tane gözlem içeriyor diyelim. Dağılımımız Poisson olsun. 

a)	n=2, 5, 10, 30, 50 ve 100 olduğu durumlarda hesapladığımız 1000 ortalamanın çubuk grafiğini (histogram) çizelim.

b)	n arttıkça ne gözlemliyorsunuz?


