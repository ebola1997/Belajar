library(rtweet)

token <- create_token(
  app = "Prakerja",
  consumer_key = "k0iL0GZoHnGSfXoeHoNjAz20T",
  consumer_secret = "hzh5mBGHdDd9Ci1rNW5WIFr4zdtHt5CiL4hDeXNmscfz3904cp",
  access_token = "821731748-kAk1xTZLTAc1upSBptUYMW7qmiNquq8Rw6n9KOa1",
  access_secret = "mQ7oEZpzYw0PTb2kIArb3dWEDbFGwgmEKTFA6nNSGO2YW")
identical(token,get_token())



#cari tweet tentang prakerja
crawling <- search_tweets("Kartu Prakerja OR Prakerja", 
                         n =1000, include_rts = TRUE, 
                         lang = "id", 
                         tweet_mode = "extended" )

save_as_csv(crawling, "data-crawling", 
            prepend_ids = TRUE, 
            na = "", 
            fileEncoding = "UTF-8")

view(skripsi)

crawling %>% ts_plot(by = "days") +
                      theme_minimal() +
                      theme(plot.title = element_text(face = "bold")) +
                      labs(
                        # Berikan label untuk x
                        x = "Waktu Dalam Hari",
                        # Berikan label untuk y
                        y = "Frekuensi Tweet",
                        # Berikan judul
                        title = "Frekuensi Tweet Dengan Kata Kunci 'Kartu Prakerja atau Prakerja'",
                        # Memberi sub-judul
                        subtitle = "per hari",
                        # Memberi caption
                        caption = paste0("Sumber: Twitter, Tanggal: ", "24-11-2020"))

crawling <- read.csv(file.choose(), header = TRUE)

tweets <- crawling$text %>% 
  as.character()
tweets[18]
crawling$text[989]
tweets <- str_replace_all(tweets, "Ã¢â", " ")
tweets <- str_replace_all(tweets, "¬", " ")
tweets <- str_replace_all(tweets, "Å", " ")
tweets <- str_replace_all(tweets, "Ã", " ")
tweets <- str_replace_all(tweets, "Â", " ")
tweets <- str_replace_all(tweets, "\u009d", " ")
tweets <- str_replace_all(tweets, "?", " ")
tweets <- str_replace_all(tweets, ",", " ")
tweets <- str_replace_all(tweets, "“", " ")
tweets <- stripWhitespace(tweets)

tweets <- gsub( "\n"," ",tweets)
tweets[49]
tweets[24]
tweets[235]
tweets[247]
tweets[248]


tweets <- tolower(tweets)
tweets <- tweets %>% 
  replace_html(.) %>% # mengganti html
  replace_url(.) %>% # mengganti URLs
  replace_emoji(.) %>% # mengganti Emoj
  replace_tag(tweets, pattern = "@([A-Za-z0-9_]+)",replacement=" ") %>%  # menghilangkan mentions
  replace_hash(tweets, pattern = "#([A-Za-z0-9_]+)",replacement=" ")      # menghilangkan hashtags
tweets <- gsub("[[:punct:]]"," ", tweets) #Menghapus tanda baca
tweets <- gsub("[[:digit:]]"," ", tweets) #Menghapus Angka
tweets <- strip(tweets) # Menghilangkan spasi berlebih dan merapihkan kata

tweets[2]
tweets[3]
tweets[5]

# Proses Normalisasi
# Memasukan lexicon (kamus normalisasi) bahasa Indonesia
spell.lex <- read.csv(file.choose(), header = TRUE)
# Mengganti kata slang dengan kata yang terdapat pada kamus colloquial lexicon
tweets <- replace_internet_slang(tweets, slang = paste0("\\b",
                                                        spell.lex$slang, "\\b"),
                                 replacement = spell.lex$formal, ignore.case = TRUE)

tweets[]
tweets[5]

normalisasi <- data.frame(normalisasi=tweets)
save_as_csv(normalisasi, "data-normalisasi")

#Menghilangkan tweet yang sama atau tweet RT
twit_bersih <- data.frame(tweet=tweets)
twit_bersih <- distinct(twit_bersih)

#Mengathui total tweet setelah menghilangkan tweet duplikat atau sama
nrow(twit_bersih)



save_as_csv(twit_bersih, "data-clean", 
            prepend_ids = TRUE, 
            na = "", 
            fileEncoding = "UTF-8")

#Dilakukan pembersihan manual Untuk membersihkan tweet yang mengandung kata tidak baku atau typo

twit_bersih <- read.csv(file.choose(), header = TRUE)

tweets <- as.character(twit_bersih$tweet)

#Visualisasi
kata <- str_c(twit_bersih$tweet, collapse = " ")
frekuensikata <- 
  Corpus(VectorSource(kata)) %>%
  TermDocumentMatrix() %>%
  as.matrix()
frekuensikata <- sort(rowSums(frekuensikata), decreasing=TRUE)
frekuensikata <- data.frame(kata = names(frekuensikata), frekuensi=frekuensikata, row.names = NULL)
wordcloud <- wordcloud2(data = frekuensikata, size = 1, shape = 'triangle', color = "random-dark", 
                        minRotation = 1, maxRotation = 0)
wordcloud

#Visualisasi Intent Sentiment
#Intent Analisis Sentimen menggunakan fitur syuzhet, data tweet harus berbentuk bahasa inggris
data.intent <- read.csv(file.choose(), header = TRUE)
intent <- data.intent$tweet %>% 
  as.character()
intent <- tolower(intent)
intent[1]
intent <- strip(intent)
intent <- gsub("[[:punct:]]","", intent)
intent <- stripWhitespace(intent)
intent <- get_nrc_sentiment(intent)
intent_score <- data.frame(colSums(intent[,]))
intent_sentiment <- data.frame(id=twit_bersih$tweet, eng=data.intent$tweet, intent)
barplot(colSums(intent),
        las = 1,
        col = rainbow(10), 
        ylab = 'Total', 
        main = "Intent Sentiment Analysis")

#Intent 2
snti <- strip(intent_sentiment$eng)
head(snti)

mysentiment_data <-get_sentiment(snti, method = "afinn")
mytotal_senti <- get_nrc_sentiment((snti))
Sentimentscores_data<-data.frame(colSums(mytotal_senti[,]))
intent_sentiment$score <- mysentiment_data

names(Sentimentscores_data)<-"Score"
Sentimentscores_data<-cbind("sentiment"=rownames(Sentimentscores_data),Sentimentscores_data)
rownames(Sentimentscores_data)<-NULL

ggplot(data=Sentimentscores_data,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments Pengguna Twitter Terhadap Kartu Prakerja")


#contoh nrc
get_nrc_sentiment("i am happy but i hate it")
get_sentiment("iam happy", method="nrc")
get_sentiment("i am happy and sad, and i hate it", method ="afinn")

get_sentiment("love", method = "syuzhet")
get_sentiment("love", method = "nrc")
get_sentiment("love", method = "afinn")
get_sentiment("love", method = "bing")

#Menghilangkan tweet yang sama atau tweet RT
twit_bersih <- data.frame(tweet=tweets)
twit_bersih <- distinct(twit_bersih)

#Mengathui total tweet setelah menghilangkan tweet duplikat atau sama
nrow(twit_bersih)



save_as_csv(twit_bersih, "data-clean", 
            prepend_ids = TRUE, 
            na = "", 
            fileEncoding = "UTF-8")

#Dilakukan pembersihan manual Untuk membersihkan tweet yang mengandung kata tidak baku atau typo

twit_bersih <- read.csv(file.choose(), header = TRUE)

tweets <- as.character(twit_bersih$tweet)

#Memasukan Kata Dasar Bahasa Indonesia
library(devtools)
install_github("nurandi/katadasaR")
library(katadasaR)

#Melihat daftar katadasaR
katadasaR::kamus_katadasar

kata.dasar <- data.frame(kata=kamus_katadasar)
#Menghapus kata imbuhan dan mengganti jadi kata dasar
katadasaR("makanan") #Contoh = Akan Menghasilkan kata Makan
katadasaR("sasaran") #menjadi sasar


#Menambahkan Fungsi stemming pada R
stemming <- function(x){paste(lapply(x,katadasar),collapse = " ")}


#Mengaplikasikan fungsi stemming pada data tweet
tweets <- sapply(tokenize_words(tweets[]), stemming)
tweets[29] #contoh tweet yang sudah di stemming

tweets[3]

#Mengaplikasikan Tokenizing data tweet (memecah data tweet)
tokenize <- tokenize_words(tweets)
head(tokenize,4)
head(tweets,4)

#Membuat stopwords dan Mengaplikasikan stopwords
stopwords.id <- read.csv(file.choose(), header = TRUE) #File stopword bahasa Indonesia
stopword.id <- scan(file.choose(),what="character",comment.char=";") #file stopword berbentuk txt
stop <- as.character(stopwords.id$stop)
stop_id <- c(stopword.id, stop) #Menggabungkan 2 file stopwords
stop_id <- data.frame(stopword=stop_id)
stop_id <- distinct(stop_id) #Menghilangkan kata stopword yang sama
stop <- as.character(stop_id$stopword)
stop <- c(stop, "kak", "kakak", "bro") #Menggabungkan stopwords dengan tambahan kata manual

kamus.stop <- data.frame(stopword = stop) #data kamus stopword yang sudah diperbarui
#Menjalankan proses stopwords
#cara 1
skripsi <- as.character(tokenize)
skripsi <- removeWords(skripsi, stop)
skripsi <- strip(skripsi)
skripsi[4]


#cara 2
#Dalam cara ini menjalankan stopword dan langsung memecah kata
dt.stopword <- tokenize_ngrams(tokenize, n=2, n_min = 1, stopwords = stop)
dt.stopword[4]

#Memasukan kamus lexicon positif dan negatif 
#Jika kamus berbentuk csv
pos <- read.csv(file.choose(), header = TRUE) 
neg <- read.csv(file.choose(), header = TRUE)

#Menghilangkan kata yang sama
pos <- distinct(pos) 
neg <- distinct(neg)

#Jika kamus berbentuk txt
pos <- scan(file.choose(),what="character",comment.char=";")
neg <- scan(file.choose(),what="character",comment.char=";")

#Menbamhkan kata manual pada data
pos.words <- as.character(pos$kata.positif) 
neg.words <- as.character(neg$kata.negatif)
pos.words = c(pos.words, "asik", "tarik", "lebih", "sosialisme", "sosialis", 
              "demokrasi", "transparan", "tepat sasar")
neg.words = c(neg.words, "curi", "ingkar", "belum", "kapitalisme", "kapitalis",
              "ngritik", "komunisme", "komunis", "larang", "ricuh", "demo",
              "muak", "salah sasar", "kurang sasar")


#Proses scoring
#Memilih data yang disimpan pada device
sentences <- read.csv(file.choose(), header = TRUE)
sentences <- data.frame(tweet=skripsi) #Memilih data pada data R


#Proses Score sentimen menggunakan combinasi ngram dan bigram
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  sentimen = laply(sentences, function(skripsi, pos.words, neg.words) {
    tokenizing <- tokenize_ngrams(skripsi, n = 2, n_min = 1)
    words <- unlist(tokenizing)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    sentimen = sum(pos.matches) + sum(neg.matches)
    return(sentimen)
  }, pos.words, neg.words, .progress=.progress)
  require(plyr)
  require(stringr)
  positif = laply(sentences, function(skripsi, pos.words, neg.words) {
    tokenizing <- tokenize_ngrams(skripsi, n = 2, n_min = 1)
    words <- unlist(tokenizing)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    positif = sum(pos.matches)
    return(positif)
  }, pos.words, neg.words, .progress=.progress)
  require(plyr)
  require(stringr)
  negatif = laply(sentences, function(skripsi, pos.words, neg.words) {
    tokenizing <- tokenize_ngrams(skripsi, n = 2, n_min = 1)
    words <- unlist(tokenizing)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    negatif = -sum(neg.matches)
    return(negatif)
  }, pos.words, neg.words, .progress=.progress)
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(skripsi, pos.words, neg.words) {
    tokenizing <- tokenize_ngrams(skripsi, n = 2, n_min = 1)
    words <- unlist(tokenizing)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df = data.frame(text=sentences, total=sentimen, positive=positif, 
                         negative=negatif, score=scores)
  return(scores.df)
}

#Melihat hasil scoring
hasil = score.sentiment(sentences$tweet, pos.words, neg.words)
View(hasil)

#Proses Labelling
hasil$label<- ifelse(hasil$score<0, "Negatif",ifelse(hasil$score==0,"Netral","Positif"))


#Proses Tagging Angka
#0 = Netral, #1 = Positif, dan #2 = Negatif
hasil$tag <- ifelse(hasil$score<0, "-1",ifelse(hasil$score==0,"0","1"))
View(hasil)

#Melihat total angka
sum(hasil$score) #Hasil seluruh sentimen
sum(hasil$score==0) #Hasil Sentimen Netral
sum(hasil$score<0) #Hasil Sentimen Negatif
sum(hasil$score>0) #Hasil Sentimen Positif
sum(hasil$total) #Melihat total sentimen
sum(hasil$positive) #Melihat total kata positif
sum(hasil$negative) #Melihat total kata negatif

save_as_csv(hasil, "data-lexicon", 
            prepend_ids = TRUE, 
            na = "", 
            fileEncoding = "UTF-8")


#Memisahkan data sentimen
#Mengambil data positif
data.pos <- hasil[hasil$score>0,]
View(data.pos)
data.pos <- data.pos[c(1,2,3,5,6,7)]
View(data.pos)
write.csv(data.pos, file = "data-pos.csv")
#Mengambil data negatif
data.neg <- hasil[hasil$score<0,]
View(data.neg)
data.neg <- data.neg[c(1,2,4,5,6,7)]
View(data.neg)
write.csv(data.neg, file = "data-neg.csv")
#Mengambil data netral
data.net <- hasil[hasil$score==0,]
View(data.net)
data.net <- data.net[c(1,2,5,6,7)]
View(data.net)
write.csv(data.net, file = "data-net.csv")

#Mengambil data tweet positif, negatif, netral dalam bentuk txt
text<-data.pos[1]
View(text)
write.table(text, file = "'tweet_positif.txt'", sep = " ")

text1<-data.neg[1]
View(text1)
write.table(text1, file = "'tweet_negatif.txt'", sep = " ")

text2<-data.net[1]
View(text2)
write.table(text2, file = "'tweet_netral.txt'", sep = " ")


#Menghilangkan Tweet dari data frame
tugas <- hasil[c(2,3,4,5,6)]
tugas$tag <- as.factor(tugas$tag)

#Menampilkan plot untuk klasifikasi SVM
qplot(total, score, data = tugas,
      color = label) #Hasil Terbaik

qplot(positive, negative, data = tugas, color = label)

#Membuat Klasifikasi SVM 1 (Total, Score)
#Membuat garis regresi
# a. regression
total <- tugas$total
score <- tugas$score
m <- svm(score ~ total, data= tugas, scale = FALSE, kernel = "linear")
coef(m)
plot(score ~ total, data = tugas)
abline(m, col = "red")
#b. classification
netral <- as.factor(tugas$label == "Negatif")
tugas2 <- tugas[c(1,4,5)]
tugas3 <- scale(tugas2[, -3]) #Scale untuk standaraisasi Data (Li&Liu)
#memasukan binary C-classification model
binary <- svm(netral~total+score, 
         data = tugas3, kernel = "linear")
# plot data and separating hyperplane
plot(score~total, data = tugas3, col = netral)
(cf <- coef(binary))
abline(-cf[1]/cf[3], -cf[2]/cf[3], col = "black") #Tengah

# plot margin and mark support vectors
abline(-(cf[1] + 1)/cf[3], -cf[2]/cf[3], col = "blue") #Positif
abline(-(cf[1] - 1)/cf[3], -cf[2]/cf[3], col = "blue") #Negatif
points(binary$SV, pch = 5, cex = 3)


#SVM KLASIFIKASI 2 (Positive, Negative)#
# a. regression
positive <- tugas$positive
negative <- tugas$negative
m <- svm(positive ~ negative, data= tugas, scale = FALSE, kernel = "linear")
coef(m)
plot(negative ~ positive, data = tugas)
abline(m, col = "red")
#b. classification
netral <- as.factor(tugas$score == "5")
tugas2 <- tugas[c(2,3,5)]
tugas3 <- scale(tugas2[, -3]) #Scale untuk standaraisasi Data (Li&Liu)
#memasukan binary C-classification model
binary <- svm(netral~positive+negative, 
              data = tugas3, kernel = "linear")
# plot data and separating hyperplane
plot(negative~positive, data = tugas3, col = netral)
(cf <- coef(binary))
abline(-cf[1]/cf[3], -cf[2]/cf[3], col = "black") #Tengah

# plot margin and mark support vectors
abline(-(cf[1] + 1)/cf[3], -cf[2]/cf[3], col = "blue") #Positif
abline(-(cf[1] - 1)/cf[3], -cf[2]/cf[3], col = "blue") #Negatif
points(binary$SV, pch = 5, cex = 3)



#Mengubah bentuk data
tugas$total <- as.numeric(tugas$total)
tugas$score <- as.numeric(tugas$score)
tugas$label <- as.factor(tugas$label)
tugas$positive <- as.numeric(tugas$positive)
tugas$negative <- as.numeric(tugas$negative)

#Pembagian Data Latih dan Uji 
#Data Latih dan Uji adalah 60:40
set.seed(1234)
library("caTools")
Split = sample.split(tugas$label, SplitRatio = 0.60)
col <- c("score", "total", "label")

prakerja_train <- tugas[Split==TRUE, col]
prakerja_uji <- tugas[Split==FALSE, col]

table(prakerja_train$label)
table(prakerja_uji$label)
table(hasil$label)



#Percobaan CM
#Pengguanan Sample untuk penempatan urutan acak
coba <- sample(940,940)
ambil <- ("label")
hasil$label <- as.factor(hasil$label)
cobalagi <- hasil[coba,ambil]
coba <- sample(940,940) #Dijalankan kembali untuk mengacak angka
cobalagu <- hasil[coba,ambil]
cobaconfus <- table(Aktual = cobalagi, Prediksi = cobalagu) #Tabel Confusion
cobaconfus
confusionMatrix(cobaconfus) #Proses CM

(0.29+0.30+0.36)/3
 89/(89+103+110)


#Pengujian/Perhitungan
#kernel radial
mymodel <- svm(label~., data = prakerja_train, kernel = "radial", cost = .1, scale = FALSE)
print(mymodel)
plot(mymodel, prakerja_train[,col])
summary(mymodel)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = prakerja_train, kernel = "radial", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)
plot(tuned)
bestmodel <- tuned$best.model
summary(bestmodel)
p <- predict(bestmodel, prakerja_uji[,col])
plot(bestmodel, prakerja_uji[,col])
plot(p)
kernel <- table(Aktual = prakerja_uji[,3], Prediksi = p) #Tabel Confusion
kernel
confusionMatrix(kernel)
sum(diag(kernel))/sum(kernel) #Akurasi x 100%
1-sum(diag(kernel))/sum(kernel)# Miss clasification x100%
kernel/sum(kernel) #Prediksi Data x 100%
kernel/colSums(kernel) #Hasil Pembagian Data x 100%

mean(p== prakerja_uji[,3]) #CAR Akurasi tanpa table CM

#Perhitungan
#Perhitungan Recall (Sensitivity)
recall <- (0.9+1+1)/3
recall
#Perhitungan Precision (Pos Pred Value)
precision <- (1+1+0.9)/3
precision
#Perhitungan Fmeasure
fmeasure <- 2*recall*precision/(recall+precision)
fmeasure


#Kernel linear
mymodel2 <- svm(label~., data = prakerja_train, kernel = "linear", cost = 0.01, scale = FALSE)
print(mymodel2)
plot(mymodel2, prakerja_train[,col])
summary(mymodel2)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = prakerja_train, kernel = "linear", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)
plot(tuned)
bestmodel2 <- tuned$best.model
print(bestmodel2)
summary(bestmodel2)
p <- predict(bestmodel2, prakerja_uji, type="class")
plot(bestmodel2, prakerja_uji, type="class")
plot(p)
linear <- table(Aktual = prakerja_uji[,3], Prediksi = p) #Tabel Confusion
linear
confusionMatrix(linear)
sum(diag(linear))/sum(linear) #Akurasi x 100%
1-sum(diag(linear))/sum(linear)# Miss clasification x100%
linear/sum(linear) #Prediksi Data x 100%
linear/colSums(linear) #Hasil /
#Perhitungan
#Perhitungan Recall (Sensitivity)
recall <- (1+1+1)/3
recall
#Perhitungan Precision (Pos Pred Value)
precision <- (1+1+1)/3
precision
#Perhitungan Fmeasure
fmeasure <- 2*recall*precision/(recall+precision)
fmeasure


#kernel polynomial
mymodel3 <- svm(label~., data = prakerja_train, kernel = "polynomial", cost = 1, scale = FALSE)
print(mymodel3)
plot(mymodel3, prakerja_train[,col])
summary(mymodel3)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = prakerja_train, kernel = "polynomial", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)
plot(tuned)
bestmodel3 <- tuned$best.model
print(bestmodel3)
summary(bestmodel3)
p <- predict(bestmodel3, prakerja_uji, type="class")
plot(bestmodel3, prakerja_uji, type="class")
plot(p)
polynomial <- table(Aktual = prakerja_uji[,3], Prediksi = p) #Tabel Confusion
polynomial
confusionMatrix(polynomial)
sum(diag(polynomial))/sum(polynomial) #Akurasi x 100%
1-sum(diag(polynomial))/sum(polynomial)# Miss clasification x100%
polynomial/sum(polynomial) #Prediksi Data x 100%
polynomial/colSums(polynomial) #Hasil /

#Perhitungan
#Perhitungan Recall (Sensitivity)
recall <- (1+1+1)/3
recall
#Perhitungan Precision (Pos Pred Value)
precision <- (1+1+1)/3
precision
#Perhitungan Fmeasure
fmeasure <- 2*recall*precision/(recall+precision)
fmeasure

#kernel sigmoid
mymodel4 <- svm(label~., data = prakerja_train, kernel = "sigmoid", cost = 0.1, scale = FALSE)
print(mymodel4)
plot(mymodel4, prakerja_train[,col])
summary(mymodel4)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = prakerja_train, kernel = "sigmoid", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)
plot(tuned)
bestmodel4 <- tuned$best.model
print(bestmodel4)
summary(bestmodel4)
p <- predict(bestmodel4, prakerja_uji, type="class")
plot(bestmodel4, prakerja_uji, type="class")
plot(p)
sigmoid <- table(Aktual = prakerja_uji[,3], Prediksi = p) #Tabel Confusion
sigmoid
confusionMatrix(sigmoid)
sum(diag(sigmoid))/sum(sigmoid) #Akurasi x 100%
1-sum(diag(sigmoid))/sum(sigmoid)# Miss clasification x100%
sigmoid/sum(sigmoid) #Prediksi Data x 100%
sigmoid/colSums(sigmoid) #Hasil /
#Perhitungan
#Recall (Sensitivity)
recall <- (0.8+0.9+1)/3
recall
#Perhitungan Precision (Pos Pred Value)
precision <- (0.9+0.8+0.9)/3
precision
#Perhitungan Fmeasure
fmeasure <- 2*recall*precision/(recall+precision)
fmeasure

F_i = (1+1^2)*precision*recall/((1^2 * precision)+recall)
F_i
#PERHITUNGAN TOTAL AKURASI SVM
Akurasi <- (.99+1+1+.93)/4
Akurasi

Recall <- (.96+1+1+.9)/4
Recall

Precision <- (.96+1+1+.86)/4
Precision

Fmeas <- (.96+1+1+.88)/4
Fmeas


#SELESAIII & TERIMAKASIIHH
?F_meas
?scale
?svm

2*0.31*0.31/(0.31+0.31)

######################################################## Ga dipake
###################################################### Ga dipake
########################################


##############
#Probabilitas sentimen
table(hasil$label)
prop.table(table(hasil$label))

#kernel
mymodel <- svm(label~ score+total, data = tugas, kernel = "radial", cost = 1)
summary(mymodel)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = tugas, kernel = "radial", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)

mymodel <- svm(label~ score+total, data = tugas, kernel = "radial", cost = 1)

p <- predict(mymodel, tugas, type="class")
plot(p)

plot(mymodel, data = tugas,
     score~total)

table(p, tugas$label)

code <- predict(mymodel, tugas)
code
confusionMatrix(code, tugas$label)

tab <- table(Prediksi = code, asli=tugas$label)
tab

1-sum(diag(tab))/sum(tab)

?confusionMatrix

#
mymodel <- svm(label~ score+total, data = tugas, kernel = "linear", cost = 1)
summary(mymodel)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = tugas, kernel = "linear", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)

plot(mymodel, data = tugas,
     score~total)

code <- predict(mymodel, tugas)       

tab <- table(Prediksi = code, asli=tugas$label)
tab

1-sum(diag(tab))/sum(tab)

#
mymodel <- svm(label~ score+total, data = tugas, kernel = "polynomial", cost = 1)
summary(mymodel)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = tugas, kernel = "polynomial", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)

plot(mymodel, data = tugas,
     score~total)

code <- predict(mymodel, tugas)       

tab <- table(Prediksi = code, asli=tugas$label)
tab

1-sum(diag(tab))/sum(tab)

#
mymodel <- svm(label~ score+total, data = tugas, kernel = "sigmoid", cost = 100)
summary(mymodel)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = tugas, kernel = "sigmoid", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)

plot(mymodel, data = tugas,
     score~total)

code <- predict(mymodel, tugas)       

tab <- table(Prediksi = code, asli=tugas$label)
tab

1-sum(diag(tab))/sum(tab)

#Confusion Matrix

confusionMatrix(code, tugas$label)




































################


form_corpus <- Corpus(VectorSource(metal$text))
print(form_corpus)
inspect(form_corpus)
str(form_corpus)

dtm_tweets <- DocumentTermMatrix(form_corpus)
inspect(dtm_tweets)

dtm_data_latih <- hasil[1:564, ]
dtm_data_latih$label <- as.factor(dtm_data_latih$label)

svm.model <- svm(label~score+total, data = dtm_data_latih, kernel ="polynomial")
svm.model

prediksi1 <- predict(svm.model, dtm_data_latih)
prediksi1

confusionMatrix(prediksi1, dtm_data_latih$label)

dtm_data_uji <- hasil[565:940, ]
dtm_data_uji$label <- as.factor(dtm_data_uji$label)
dtm_data_uji$score <- as.numeric(dtm_data_uji$score)
dtm_data_uji$total <- as.numeric(dtm_data_uji$total)

svm.model2 <- svm(label~score+total, data = dtm_data_uji, kernel="polynomial")
svm.model2

prediksi2 <- predict(svm.model2, dtm_data_uji)
prediksi2

confusionMatrix(prediksi2, dtm_data_uji$label)


label_sentimen_latih <- dtm_tweets[1:564, ]$label

targetLatih <- as.numeric(label_sentimen_latih)
modelLatih <- svm(dtm_data_latih, targetLatih, kernel = "linear", cost = 10, gamma = 0.25)
summary(modelLatih)

code <- predict(modelLatih, dtm_data_latih)
summary(code )
confusionMatrix(prediksi = code, asli= label_sentimen_latih)
#Stopwords Cara GUNDAR
tweetremove <- VCorpus(VectorSource(skripsi))
tweetstop <- as.character(tweets)
tweetstop <- tm_map(tweetremove, removeWords, stopwords.id)
tweetstop <- tm_map(tweetremove, removeWords, stopwords.id2)

skripsi <- removeWords(skripsi, stp_id)
tweetstop <- tokenize_words(tweetstop, stopwords = stopwords.id) # Mengaplikasikan stopword  
skripsi[3]
inspect(tweetstop[[3]])

tweetstop <- tm_map(tweetstop, stripWhitespace)

tdm <- TermDocumentMatrix(tweetstop, control = list(wordLengths = c (1, Inf)))
freq.terms <- findFreqTerms(tdm, lowfreq = 15)

remove(belajar)
freq.terms[1:50]

naiveBayes()

newmeta <- data.frame(stopword=stopwords.id2)

newskin <- stp_id %>% 
  as.data.frame() %>% 
  distinct()

###########
 
#NaiveBayes
mymodel5 <- naiveBayes(label~., data = prakerja_train)
predict(mymodel5, prakerja_train) 
predict(mymodel5, prakerja_train, type = "raw") 

pred <- predict(mymodel5, prakerja_uji)
table(pred, prakerja_uji$label)

#With laplace
mymodel5 <- naiveBayes(label~., data = prakerja_train, laplace = 1)
pred <- predict(mymodel5, prakerja_uji)
table(pred, prakerja_uji$label)


#Langsung Hajar NaiveBayes Dengan Data Uji
m <- naiveBayes(prakerja_uji[,-3], prakerja_uji[,3])
m

table(predict(m, prakerja_uji), prakerja_uji[,3])


?confusionMatrix