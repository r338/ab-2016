#Akademik Bilisim 2016 - R ile Veri Analizi Kursu
#Egitmenler: Mustafa Baydogan ve Berk Orbay

#dplyr ve ggplot2 yardimci fonksiyonlari


isim_getir<-function(isim_soyad="isim",cinsiyet="Erkek",n){

	if(isim_soyad=="isim" & cinsiyet=="Erkek"){
		return(sample(c("Acar","Afsar","Afsin","Ahsen","Akin","Alaz","Alemdar","Ali","Alican","Alihan","Alinur","Alize","Alp","Alpay","Altan","Amil","Andaç","Angin","Anil","Aras","Arda","Ardiç","Armagan","Aruz","Ata","Atabek","Ataç","Ataol","Ates","Atik","Atil","Atilgan","Atilla","Atom","Ayaz","Aybars","Ayberk","Aydemir","Ayerdem","Aykon","Aykut","Aytaç","Aytek","Aytug","Ayvaz","Babur","Baha","Bahadir","Balkan","Balkar","Balkir","Baran","Barbaros","Baris","Basar","Batu","Batur","Baykal","Bayulken","Berat","Bereket","Berk","Berkay","Berke","Besim","Betim","Bilgin","Birkan","Bora","Bugra","Bulut","Buragan","Burak","Burçak","Burkhan","Can","Cem","Cenk","Cesur","Ceyhan","Comert","Cumhur","Cuneyt","Çagan","Çagatay","Çagdas","Çaglar","Çaglayan","Çagri","Çakabey","Çakir","Çelik","Çetin","Çevik","Çinar","Çivgin","Daghan","Deger","Demir","Deniz","Denizhan","Derin","Derman","Destan","Devrim","Dinç","Dirim","Dogaç","Doruk","Duman","Duru","Durul","Dunya","Ecevit","Ediz","Efe","Eflatun","Efsun","Ege","Egemen","Ekim","Ekin","Elçi","Elgin","Elhan","Emir","Emrah","Emre","Emri","Engin","Enginsu","Enis","Eran","Erdem","Eren","Ergin","Erguvan","Erim","Erk","Erkin","Erksin","Ertunç","Ertunga","Eser","Etkin","Evren","Evrensel","Eylem","Ferhan","Ferhat","Feyezan","Feyyaz","Feza","Firat","Furkan","Giray","Gokada","Gokalp","Gokberk","Gokhan","Gokmen","Gokova","Goksel","Goksenin","Gokturk","Gonenç","Gorkem","Gurur","Gun","Gunes","Gur","Guven","Guvenç","Hakan","Haliç","Haluk","Harun","Hasmet","Hayat","Hincal","Hisar","Hitit","Hur","Hurriyet","Iklim","Ilbay","Ilgar","Ilgi","Ilham","Ilhan","Inan","Inanç","Irak","Isfendiyar","Iskender","Istemihan","Izgu","Kaan","Kahraman","Kamer","Kanat","Kandemir","Kartal","Kaya","Kayihan","Kaynak","Kerem","Kerim","Kiliç","Kivanç","Kivilcim","Koral","Koray","Korhan","Korkut","Koksal","Kubilay","Kuday","Kudret","Kurthan","Kurtulus","Kurultay","Kutan","Kutlu","Kutlukhan","Kursad","Lacin","Ladin","Ledun","Levent","Lider","Lirik","Marti","Mecnun","Melih","Melik","Mengu","Meriç","Mert","Mete","Mevzun","Miralay","Murat","Mutlu","Nedim","Odak","Odkan","Ogun","Ogrun","Ogul","Oktar","Olay","Olcayto","Olgu","Olgun","Onat","Ongun","Onur","Orçun","Orkun","Otag","Ova","Oytun","Ozan","Öcal","Ögut","Ökten","Ömer","Ömur","Önal","Öncel","Öncu","Önder","Öner","Öney","Önsel","Ören","Örsan","Özgun","Özgur","Pamir","Pars","Pelit","Poyraz","Reha","Revan","Ruzgar","Saganak","Sanat","Sancak","Sarp","Sarper","Satvet","Savas","Saygin","Selim","Selman","Semen","Semih","Sercan ","Serdar","Seretan","Sergen","Serhan","Serhat","Serkan","Sertaç","Sertug","Seza","Sinan","Sipahi","Siper","Sokra","Sonat","Sorgun","Sogut","Soylem","Soz","Sunay","Sungun","Sungur","Suavi","Suer","Suerdem","Suha","Suheyl","Sumer","Sureyya","sahin","san","sansal","sarik","sehmuz","sen","sirzat","Tan","Tansel ","Tarik","Tarkan","Tayfun","Tayga","Taylan","Tibet","Tinaz","Toktamis","Tolga","Tolun","Tonguç","Toprak","Toraman","Toygar","Toygun","Tore","Toz","Tufan","Tugay","Tuna","Tunca","Tunç","Tumer ","Ufuk","Ugur","Ulubey","Uluç","Ulug","Ulum","Umar","Umur","Umut","Unan","Uragan","Uran","Uray","Utarit","Utku","Uygar","Uygur","Uzay","Ülke","Ünal ","Ürun","Vadi","Verim","Volkan","Yagiz","Yalgin","Yalim","Yalin","Yalman","Yalvaç","Yamaç","Yaman","Yardan ","Yarin","Yasin","Yaver","Yekta","Yetkin","Yigit","Yilaydin","Yuce","Zaman"),n,replace=FALSE))
	}

	if(isim_soyad=="isim" & cinsiyet=="Kiz"){
		return(sample(c("Ada","Afet","Agit","Ahenk","Ahu","Ajlan","Akarsu","Akasya","Aksu","Alba","Alev","Algin","Alpike","Altin","Arya","Arzu","Asena","Asli","Asu","Asuman","Asya","Atlas","Aybike","Aybirgen","Ayça","Ayçiçek","Ayda","Aydan","Ayevi","Aykiz","Ayla","Aylin","Aysar","Aysin","Aysu","Ayse","Aysegul","Aysenur","Bade","Baglan","Bahar","Baklan","Bala","Balkin","Balkiz","Banu","Basak","Begum","Belde","Belen","Belen","Belgin","Beliz","Benan","Benek","Bengi","Beniz","Berguzar","Beria","Beril","Berna","Berrak","Berran","Besisu","Beste","Bestenigar","Betul","Beyza","Bike","Bilge","Bilgun","Bilhan","Billur","Biricik","Bugday","Buket","Burcu","Burçak","Burçin","Buse","Bukum","Busra","Cana","Canan","Candan","Canfeza","Cankiz","Canova","Cansu ","Cemre","Ceren","Cevza","Ceyda","Ceylan","Çagla","Çakil","Çiçek","Çigdem","Çiglik","Çilek","Çiler","Çim","Çimen","Çise-M","Çisil","Çolpan","Damla","Defne","Demet","Demre","Deniz","Derya","Desen","Destegul","Devin","Dicle","Didem","Dilara","Dilay","Dilek","Dilem","Dilnisin","Dilruba","Dilsu","Dilsah","Dolunay","Duygu","Ebru","Ece","Ecmel","Eda","Ege","Elçin","Elif","Elvan","Esen","Esin","Esna","Esra","Eti","Evin","Eylul","Ezgi","Ferah","Feray","Ferda","Feyza","Fidan","Figen","Firuze","Fulya","Funda","Furuzan","Fusun","Gamze","Gaye","Gece","Gelincik","Gerçek","Gizem","Gonca","Gokçe","Goksu ","Golge-N","Gozde","Gozen","Guher","Gulbahar","Gulçin","Gulfem","Gulgun","Gulistan","Guliz","Gulizar","Gulriz","Gulsah *","Gul-Üm","Gulumse","Gunçiçek","Guvercin","Guz","Guzel","Guzin","Handan","Harika","Haslet","Hayal","Hazal","Hazan","Hazar","Haziran","Hece","Heves","Hilal","Hosseda","Hulya","Huma","Humeyra","Huner","Hurrem","Husna","Husun","Idil","Ilayda","Ilgaz","Ilgim","Ilgin","Ilgun","Ilkbahar","Ilke","Ilkyaz","Ilsu","Ilter","Imge","Imren","Inci","Incilay","Ipar","Ipek","Iraz-Ca *","Irem","Iris","Irmak","Isik","Isil","Isilay","Isin","Itir","Iyem","Izel","Izem","Izgi","Iz-Im","Kamelya","Kardelen","Kelebek","Kimya","Kosem","Kugu","Kumru","Kumsal","Kutay","Kutsal","Lal","Lale","Lerzan","Leyla","Leylifer","Lila","Manolya","Maral","Mavisu","Mehtap","Mehves","Melda","Melike","Melis","Melisa","Melodi","Menekse","Menevis","Meral","Mercan","Merih","Merve","Mevsim","Mimoza","Mine","Muge","Nagme","Naz","Nazli-M","Nehir","Nergis","Neslisah","Nesrin","Nese-M","Neval","Nevbahar","Neveser","Nevgece","Nevgul","Nevra","Neyir","Nigar","Nihal","Nihan","Nil","Nilufer","Nisan","Nurgul","Nurgun","Nurseli","Nuket","Nukhet","Nukte","Oya","Oylum","Ödul","Örgun","Övgu","Öyku","Özen","Özge","Özlem","Pamira","Papatya","Pelin","Pera","Perçem","Peri","Perran","Petek","Pinar","Piril","Pirilti","Pitircik","Piyale","Rana","Renan","Rengin","Rezzan","Ruhsar","Ruçhan","Ruya","Saba","Sabah","Sadberk","Sahil","Sahra","Salkim","Sanem","Saygin","Sayil","Sebil","Sebla","Seçil","Seçkin","Seda","Sedef","Seden","Seher","Sel","Selda","Selen","Selin","Selinti","Selis","Selmin","Selvi","Sema","Semiramis","Sena","Seren","Serenat","Serra","Sertap","Servi","Ses","Sevdem","Seven","Sevgili","Sevi","Sevil","Sevinç","Seyyal","Sezen","Sezgi","Sibel","Sila","Sim","Sima","Simge","Simin","Simya","Sine-M","Siren","Siret","Sirma","Sonyaz","Su","Sumru","Suna","Sulun","Sundus","Susen","sahbanu","sahika","san","sans","sayeste","sebboy","sebnem","sehnaz","sehrazat","selale","sermin","sevval","seyda","siir","simal","sirin","solen","solen-De","sule","Tamar","Tango","Tanyeli","Tilbe","Tilsim","Tomris","Toren","Tugba","Tugçe","Tulu","Tutku","Tutya","Tulin","Tumay","Turkuvaz","Turku","Tuvana","Umay","Ülgen","Ülger","Ülker","Ülku-M","Ürun","Üvercinka","Üzum","Venus","Verda","Verdinaz","Vildan","Vuslat","Yagmur","Yanki","Yaprak","Yar","Yaren","Yasemin","Yazgi","Yazgulu","Yelda","Yeliz","Yeniay","Yeser","Yesim","Yildiz","Yonca","Yosun","Yoruk","Yurdagul","Yurdanur","Zeren","Zerrin","Zeynep","Zeyno","Zuhal","Zuhal","Zulal","Zulal","Zuleyha","Zuleyha","Zuluf","Zuluf","Zumra","Zumra","Zumrut","Zumrut"),n,replace=FALSE))
	}

	if(isim_soyad=="soyad"){
		return(sample(c("Abaci","Abadan","Aclan","Adal","Adan","Adivar","Akal","Akan","Akar","Akay","Akaydin","Akbulut","Akgul","Akisik","Akman","Akyurek","Akyuz","Aksit","Alniaçik","Alpugan","Alyanak","Arican","Arslanoglu","Atakol","Atan","Avan","Ayaydin","Aybar","Aydan","Aykaç","Ayverdi","Agaoglu","Asikoglu","Babacan","Babaoglu","Bademci","Bakircioglu","Balaban","Balci","Barbarosoglu","Baturalp","Baykam","Basoglu","Berberoglu","Beserler","Besok","Biçer","Bolatli","Dalkiran","Dagdas","Daglaroglu","Demirbas","Demirel","Denkel","Dizdar","Dogan","Durak","Durmaz","Duygulu","Dusenkalkar","Egeli","Ekici","Eksioglu","Eliçin","Elmastasoglu","Elçiboga","Erbay","Erberk","Erbulak","Erdogan","Erez","Erginsoy","Erkekli","Eronat","Ertepinar","Erturk","Erçetin","Evliyaoglu","Fahri","Gonultas","Gumuspala","Gunday","Gurmen","Hakyemez","Hamzaoglu","Ilicali","Kahveci","Kaplangi","Karabulut","Karabocek","Karadas","Karaduman","Karaer","Kasapoglu","Kavaklioglu","Kaya","Keseroglu","Keçeci","Kiliççi","Kiraç","Kocabiyik","Korol","Koyuncu","Koç","Koçoglu","Koçyigit","Kuday","Kulaksizoglu","Kumcuoglu","Kunt","Kunter","Kurutluoglu","Kutlay","Kuzucu","Kormukçu","Koybasi","Koyluoglu","Kuçukler","Limoncuoglu","Mayhos","Menemencioglu","Mertoglu","Nalbantoglu","Nebioglu","Numanoglu","Okumus","Okur","Oraloglu","Orbay","Ozansoy","Paksut","Pekkan","Pektemek","Polat","Poyrazoglu","Poçan","Sadiklar","Samanci","Sandalci","Sarioglu","Sayginer","Sepetçi","Sezek","Sinanoglu","Solmaz","Sozeri","Suleymanoglu","Tahincioglu","Tanrikulu","Tazegul","Tasli","Tasçi","Tekand","Tekelioglu","Tokatlioglu","Tokgoz","Topaloglu","Topçuoglu","Toraman","Tunaboylu","Tunçeri","Tuglu","Tugluk","Turkdogan","Turkyilmaz","Tutuncu","Tuzun","Uca","Uluhan","Velioglu","Yalçin","Yazici","Yetkiner","Yesilkaya","Yildirim","Yildizoglu","Yilmazer","Yorulmaz","Âdem","Çamdali","Çapanoglu","Çatalbas","Çagiran","Çetin","Çetiner","Çevik","Çorekçi","Önur","Örge","Öymen","Özberk","Özbey","Özbir","Özdenak","Özdogan","Özgorkey","Özkara","Özkok","Öztonga","Öztuna","Özturk","Özyigit","Üstunel","Üzumcu","Üçtas","ibrahimoglu","ince","incekara","inler","ipekkaya","irem","islimyeli","izbirak","izgi","samlioglu","sen","sengun","soray"),n,replace=FALSE))
	}
}

sinif_olustur<-function(sinif_sayisi=2,sinif_buyuklugu=30,dersler=c("Matematik","Kodlama","Tarih"),sinav_sayisi=2,min_not=10,max_not=100,rastgelelik=338){
	set.seed(338) #Rastgelelik tohumunu ek

	notlar<-matrix(sample(min_not:max_not,sinif_buyuklugu*sinif_sayisi*length(dersler)*sinav_sayisi,replace=TRUE),ncol=length(dersler)*sinav_sayisi)

	if(sinav_sayisi==1){
		colnames(notlar)<-dersler
	}else{
		colnames(notlar)<-paste0(sort(rep(dersler,sinav_sayisi)),"_",1:sinav_sayisi)
	}

	cinsiyet<-sample(c("Kiz","Erkek"),sinif_buyuklugu*sinif_sayisi,replace=TRUE)
	isim<-rep("",length(cinsiyet))
	isim[cinsiyet=="Erkek"]<-isim_getir(isim_soyad="isim",cinsiyet="Erkek",n=sum(cinsiyet=="Erkek"))
	isim[cinsiyet=="Kiz"]<-isim_getir(isim_soyad="isim",cinsiyet="Kiz",n=sum(cinsiyet=="Kiz"))
	soyad<-isim_getir(isim_soyad="soyad",cinsiyet="",n=length(isim))
	sube<-sort(rep(paste0(9,toupper(letters[1:sinif_sayisi])),sinif_buyuklugu))

	return(data.frame(isim,soyad,cinsiyet,sube,notlar))

}


