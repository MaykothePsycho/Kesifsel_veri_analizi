# 1. BÖLÜM: Ürettiğim veriyle keşifsel veri analizi

# Rastgele veri oluşturmak için MASS kütüphanesinden faydalanıyorum.
install.packages("MASS")
library(MASS)

# Veri setini oluşturma

set.seed(333) # Tekrarlanabilirlik için seed belirledim, genelde 123 verilir. Hep aynı seti getirmesi için.
n <- 200 # Gözlem sayısı
x1 <- rnorm(n, mean = 8, sd = 1) # Bağımsız değişken 1
x2 <- rnorm(n, mean = 16, sd = 2) # Bağımsız değişken 2
x3 <- rnorm(n, mean = 12, sd = 1.5) # Bağımsız değişken 3
x4 <- rnorm(n, mean = 3, sd = 0.5) # Bağımsız değişken 4
x5 <- rnorm(n, mean = 20, sd = 2) # Bağımsız değişken 5
y <- 2 + 1.2*x1 - 1.8*x2 + 1.5*x3 + 0.6*x4 - 1.1*x5 + rnorm(n, mean = 0, sd = 2) # Bağımlı değişken


# Oluşturulan veri setini dataframe'e dönüştürüyorum.
data1 <- data.frame(y, x1, x2, x3, x4, x5)

head(data1) # Verisetimizin ilk 6 satırına bakalım.

#Şimdi değişkenlerin nasıl tutulduğunu görebilmek için verisetiminizin structure'ına bakalım.

str(data1) #Hepsinin number olarak turulduğunu görüyorum, decimal sayıları olduğu için number olarak tutulmasını istiyorum, bu yüzden dönüşüm uygulamayacağım.


#Şimdi verimizin kalitesini kontrol edeceğiz. Eksik veri olup olmadığını tespit edelim.

is.na(x1) #Eksik veri yok
is.na(x2) #Eksik veri yok
is.na(x3) #Eksik veri yok
is.na(x4) #Eksik veri yok
is.na(x5) #Eksik veri yok


#Eksik veri olmadığı için doldurma işlemine gerek duymuyorum. Gerekseydi ortalama ile dolduracaktım.

#Şimdi aykırı değerleri tespit edebilmek için öncelikle kutu grafiği oluşturacağım.
boxplot(data1) #Bazı aykırı değer olabilecek değerler gözüküyor.

#Sütunları sortlayarak hangileri olabileceğine bakalım

sort(data1$x1) #5.546441 değeri aykırı olabilir gibi duruyor.
sort(data1$x2) #22.79155 değeri aykırı olabilir gibi duruyor.
sort(data1$x3) #7.333328, 7.837208, 16.255690 değerleri aykırı olabilir gibi duruyor.
sort(data1$x4) # Aykırı değer görünmüyor
sort(data1$x5) #13.64923 değeri aykırı olabilir gibi duruyor.

#Net bir cevap verebilmek için aykırı değerlere kendim bakıyorum.

# Öncelikle Z-skoru hesaplıyorum
z_scores <- scale(data1)

# Sonra da z-skorun mutlak değerce (abs fonksiyonu) 3'ten büyük olanlarını bularak aykırı değerleri belirliyorum.
outliers <- abs(z_scores) > 3

#aykırı değerleri görelim

outliers #Outliers dataframe'ine bakarak 3 tane TRUE değeri yani 3 tane outlier olduğunu görüyoruz. Bunlar: x2 105. satır; x3 166. satır; x5 69. satır. Bu değerlere bakarak grafikten yaptığım yorumları check ediyorum.

#x1'de outlier çıkmadı, grafiğe bakarak 1 outlier olabilir demiştim, doğru değilmiş.
data1[105,3] #x2'de 22.79155 outlier değer çıktı. Grafiğe bakarak yaptığım tahmin doğruymuş. 
data1[166,4] #x3'te 7.333328 outlier değer çıktı. Grafiğe bakarak 3 outlier olabileceği tahmininde bulunmuştum. 1 tanesi doğruymuş.
#x4'de outlier çıkmadı, grafiğe bakarak yaptığım tahmin doğruymuş. 
data1[69,6] #x5'de 13.64923 outlier değer çıktı. Grafiğe bakarak yaptığım tahmin doğruymuş. 

# Verimizin dağılımlarına bakalım

#Nasıl dağıldığını görebilmek için öncelikle değişkenlerin histogramlarına veya boxplotlarına bakabilirim.

hist(data1$x1)
hist(data1$x2)
hist(data1$x3)
boxplot(data1$x4)
boxplot(data1$x5)

#Grafiklerde aykırı değer gibi görünen değerleri zaten aykırı değer testine tutmuştuk, bir kısmı aykırı değer, bir kısmı değildi. 


#Q-Q plot ile normal dağılıp dağılmadığını tahmin edebiliriz. 

qqnorm(data1$x1)
qqline(data1$x1) # Veriyi zaten normal dağılacak şekilde kendimiz ürettik. Bu nedenşe muntazam şekilde normal dağılıyor. Diğer değişkenler için de aynı şey söz konusu olacak.

#Şimdi de değişkenler arasındaki korelasyona bakacağım. 

correlation1 = cor(x1,y)
print(correlation1) # x1 ve y değişkenleri arasındaki korelasyon katsayısını hesapladım: 0.2430311

#Şimdi de multicollinearity'ye bakalım.

model1 = lm(y~x1+x2+x3+x4+x5) #Önce modeli kuruyorum
summary(model1) #Sonuçları görüyorum, kat sayı tahminleri, standart sapma, t değerleri vs. Veriyi kendim kurduğum için oldukça istatistiksel olarak oldukça anlamlı çıktı.


#Bağımsız değişkenler arasındaki ilişkiye bakalım. 

correlation_matrix1 <- cor(data.frame(x1, x2, x3, x4, x5))
print(correlation_matrix1) #Düşük korelasyon var.


#Korelasyon matrisini görselleştirelim

install.packages("corrplot")
library(corrplot)
corrplot(correlation_matrix1, method = "color")

# Bir de bağımsız değişkenlerin grafiğini çizelim:
par(mfrow=c(1,5)) # Grafiklerin yan yana yerleştirilmesi için
plot(x1, y, main = "x1 vs. y", xlab = "x1", ylab = "y", col = "blue", pch = 16)
plot(x2, y, main = "x2 vs. y", xlab = "x2", ylab = "y", col = "red", pch = 16)
plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "green", pch = 16)
plot(x4, y, main = "x3 vs. y", xlab = "x4", ylab = "y", col = "pink", pch = 16)
plot(x5, y, main = "x3 vs. y", xlab = "x5", ylab = "y", col = "yellow", pch = 16)


#Son değişiklikleri yapalım: Test & Train

install.packages("caTools")
library(caTools)

split1 <- sample.split(y, SplitRatio = 0.6) # 60%la veriyi eğitim, 40%la test etmek için kullanacağım.
train_data <- subset(data.frame(x1, x2, x3, x4, x5, y), split1 == TRUE)
test_data <- subset(data.frame(x1, x2, x3, x4, x5, y), split1 == FALSE)

# Verinin boyutlarını kontrol ediyorum
dim(train_data)
dim(test_data)
#-------------------------------------------------------------------------


# 2. BÖLÜM: Çektiğim veriyle keşifsel veri analizi

#Hazır bir verisetini kullanmak için carData kütüphanesinden faydalanıyorum.

install.packages("carData")
library(carData)

data("Salaries") # Salaries verisetinin keşifsel veri analinizini yapacağım. 

data2 <- data.frame(Salaries) #Salaries verisetini data 2 ismiyle bir data frame'e çevirdim.

head(data2) # Verisetimizin ilk 6 satırına bakalım.

#Şimdi değişkenlerin nasıl tutulduğunu görebilmek için verisetiminizin structure'ına bakalım.

str(data2) #Kategorik değilkenlerin factor, nümerik değişkenlerin int olarak tutulduğunu görüyorum. Tüm sayıları tam sayı olduğu için şu anki haliyle uygun olduğunu düşünüyorum, bu yüzden dönüşüm uygulamayacağım.

#Şimdi verimizin kalitesini kontrol edeceğiz. Eksik veri olup olmadığını tespit edelim.

is.na(data2$rank) #Eksik veri yok
is.na(data2$discipline) #Eksik veri yok
is.na(data2$yrs.since.phd) #Eksik veri yok
is.na(data2$yrs.service) #Eksik veri yok
is.na(data2$sex) #Eksik veri yok
is.na(data2$salary) #Eksik veri yok


#Eksik veri olmadığı için doldurma işlemine gerek duymuyorum. Gerekseydi nümerik değişkenleri ortalama ile, kategorik değişkenleri bir önceki değerle doldurabilirdim.

#Şimdi aykırı değerleri tespit edebilmek için öncelikle kutu grafiği oluşturacağım.
boxplot(data2) #Salary sütununda aykırı değer olabilecek değerler gözüküyor.

#Salary sütununu sortlayarak hangileri olabileceğine bakalım

sort(data2$salary) # 204000 205500 231545 değerleri aykırı olabilir gibi duruyor.

#Net bir cevap verebilmek için aykırı değerlere kendim bakıyorum.

# Öncelikle Salary sütunundaki değerlerin Z-skorunu hesaplıyorum
z_scores2 <- scale(data2$salary)

# Sonra da z-skorun mutlak değerce (abs fonksiyonu) 3'ten büyük olanlarını bularak aykırı değerleri belirliyorum.
outliers2 <- abs(z_scores2) > 3

#aykırı değerleri görelim

outliers2 #Outliers dataframe'ine bakarak 3 tane TRUE değeri yani 2 tane outlier olduğunu görüyoruz. Bunlar: 44 ve 365. satırlar Bu değerlere bakarak grafikten yaptığım yorumları check ediyorum.


data2[44,6] #231545 
data2[365,6] #205500

# Grafiğe bakarak 3 değer outlier olabilir diye tahmin etmiştim. 2 tanesi doğruymuş.


# Verimizin dağılımlarına bakalım

#Nasıl dağıldığını görebilmek için öncelikle nümerik değişkenlerin histogramlarına veya boxplotlarına bakabilirim.

hist(data2$yrs.since.phd)
hist(data2$yrs.service)
boxplot(data2$salary)

#Grafiklerde aykırı değer gibi görünen değerleri zaten aykırı değer testine tutmuştuk, bir kısmı aykırı değer, bir kısmı değildi. 


#Q-Q plot normal dağılıp dağılmadığını tahmin edebiliriz. 

qqnorm(data2$yrs.since.phd)
qqline(data2$yrs.since.phd) #Normal dağılıyor gibi görünüyor

qqnorm(data2$yrs.service)
qqline(data2$yrs.service) #Normal dağılıyor gibi görünüyor

qqnorm(data2$salary)
qqline(data2$salary) #Normal dağılıyor gibi görünüyor


#Şimdi de değişkenler arasındaki korelasyona bakacağım. Burada Salary'i siğer değişkenlere bağlı olduğunu varsayarak, bağımlı değişken olarak ele alacağım.

correlation2 = cor(data2$yrs.service,data2$salary) #Akademisyenlerin çalıştığı yılla aldıkları maaş arasındaki korelasyona bakıyorum.
print(correlation2) # korelasyon katsayısı: 0.3347447. Aralarında zayıf bir korelasyon olduğunu söyleyebiliriz.

#Şimdi de multicollinearity'ye bakalım.

model2 = lm(data2$salary~data2$yrs.service+data2$yrs.since.phd) #Burada da çalıştığı yıl ve phd'lerinin üzerinden geçen yıllara göre inceliyorum.
summary(model2) #Sonuçları görüyorum, kat sayı tahminleri, standart sapma, t değerleri vs. İstatistiksel olarak anlamlı olduğunu gördüğümüz bu yapıda çalışılan yıl arttıkça maaşın azaldığını, ama phdli çalışılan yıl arttıkça maaşın arttığını görüyoruz.


#Bağımsız değişkenler arasındaki ilişkiye bakalım. 

correlation_matrix2 <- cor(data.frame(data2$yrs.service, data2$yrs.since.phd))
print(correlation_matrix2) #Akademisyenlerin çalıştığı yıl ve phd aldığı yıl arasında güçlü bir pozitif ilişki söz konusu.

#Korelasyon matrisini görselleştirelim


corrplot(correlation_matrix2, method = "color")


# Bir de bağımsız değişkenlerin grafiğini çizelim:
par(mfrow=c(1,2)) # Grafiklerin yan yana yerleştirilmesi için
plot(data2$yrs.since.phd, data2$salary, main = "yrs.since.phd vs. salary", xlab = "yrs.since.phd", ylab = "salary", col = "blue", pch = 16)
plot(data2$yrs.service, data2$salary, main = "yrs.service vs. salary", xlab = "yrs.serviced", ylab = "salary", col = "red", pch = 16)

#Son değişiklikleri yapalım: Test & Train


split2 <- sample.split(y, SplitRatio = 0.6) # 60%la veriyi eğitim, 40%la test etmek için kullanacağım.
train_data2 <- subset(data.frame(data2$yrs.since.phd, data2$yrs.service, data2$salary), split2 == TRUE)
test_data2 <- subset(data.frame(data2$yrs.since.phd, data2$yrs.service, data2$salary), split2 == FALSE)


# Verinin boyutlarını kontrol ediyorum
dim(train_data)
dim(test_data)
