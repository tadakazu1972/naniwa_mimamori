#浪速区

#ライブラリ読込
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

#作業ディレクトリ指定
setwd("~/Desktop/naniwa")

#必要なファイル読込
shape <- st_read("h27ka27111.shp") #地図境界線、人口
school <- read_csv("elementaryschool.csv") #小学校
kenchiki <- read_csv("facilities201712.csv") #検知器

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- shape$JINKO %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(shape[4], col=col_km, main="浪速区　人口")
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2], labels=shape$MOJI, cex=0.5)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.0007, labels=shape$JINKO, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

####################################################################
#５歳別人口
#
nenrei <- read_csv("nenrei.csv") #５歳別人口 -は0に置換しないとnumにならないので前処理必要、項目名も変更

#シェープファイルと年齢データを結合
data <- left_join(shape, nenrei, by="MOJI")

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$総数１５歳未満 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(data[4], col=col_km, main="浪速区　総数15歳未満")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$総数１５歳未満, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
#総数５〜９歳

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$TOTAL5_9 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(data[4], col=col_km, main="浪速区　総数5〜9歳")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$TOTAL5_9, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
#総数10〜14歳
#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$TOTAL10_14 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(data[4], col=col_km, main="浪速区　総数10〜14歳")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$TOTAL10_14, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
###################################################################
#男５〜９歳

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$MALE5_9 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Blues"))
plot(data[4], col=col_km, main="浪速区　男5〜9歳")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$MALE5_9, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
#男10〜14歳
#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$MALE10_14 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Blues"))
plot(data[4], col=col_km, main="浪速区　男10〜14歳")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$MALE10_14, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
###################################################################
#女５〜９歳

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$FEMALE5_9 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Reds"))
plot(data[4], col=col_km, main="浪速区　女5〜9歳")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$FEMALE5_9, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
#女10〜14歳
#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$FEMALE10_14 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Reds"))
plot(data[4], col=col_km, main="浪速区　女10〜14歳")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$FEMALE10_14, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
#単年の犯罪発生データ読み込み
crime <- read_csv("naniwa_crime.csv")
crime2015 <- crime %>% filter(crime$YEAR=="2015")
data <- left_join(shape, crime2015, by="MOJI")

#NUMBER列のNAを0変換
data$NUMBER[is.na(data$NUMBER)]<-0

#地図　描画
par(family="HiraKakuProN-W3")
col_km <- data$NUMBER %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Purples"))
plot(data[4], col=col_km, main="浪速区　子ども犯罪 2015-2017年10月")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$NUMBER, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
#３年間犯罪発生データ読み込み
crimeAll <- read_csv("naniwa_crime20152017.csv")
data <- left_join(shape, crimeAll, by="MOJI")

#NUMBER列のNAを0変換
data$NUMBER[is.na(data$NUMBER)]<-0

#地図　描画
par(family="HiraKakuProN-W3")
col_km <- data$NUMBER %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Purples"))
plot(data[4], col=col_km, main="浪速区　子ども犯罪 2015-2017年10月")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$NUMBER, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
#女性被害　単年csvデータを読み込みで描画する場合
women2015 <- read_csv("women2015.csv")

#町丁目ごとに集計
women2015sum <- women2015 %>% group_by(MOJI) %>% summarize(COUNT=n()) 

#シェープファイルと結合
data <- left_join(shape, women2015sum, by="MOJI")

#色付けするためcountのNAを0変換
data$COUNT[is.na(data$COUNT)]<-0

#地図　描画
par(family="HiraKakuProN-W3")
col_km <- data$COUNT %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Purples"))
plot(data[4], col=col_km, main="浪速区　女性被害 2015年")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$COUNT, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")
