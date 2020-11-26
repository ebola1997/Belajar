#Meminta izin kepada twitter dengan meruning berikut:
##Autenti via web
token <- create_token(
  app = "Prakerja",
  consumer_key = "api key", #data saya sensor 
  consumer_secret = "api secret key",
  access_token = "token",
  access_secret = "secret_token")

#Mencari tweet tentang kata yang ada perlukan (prakerja)
## tweets adalah nama data anda pada R (nama dapat diganti)
#n = jumlah, include_rts = data retweet diambil (jika iya = TRUE, jika tidak = FALSE), dan lang = bahasa
## jika ingin info lebih lanjut coding "?search_tweets" untuk lebih jelas (tanpa tanda kutip ")
tweets <- search_tweets("prakerja", n=1000, include_rts = FALSE, lang = "id")


#Melihat hasil
tweets

##save datanya ke dalam bentuk rds
saveRDS(tweets,file = 'tweet-mentah.rds')

##Save datanya ke dalam bentuk csv (excel)
##Data akan disimpan kedalam folder documents
write.csv(tweets, "Apaini.csv")

## Contoh lebih lengkap jika ingin menyimpan data untuk col atau row TRUE/FALSE dll
write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

##Load dataset rds
tweet <- readRDS('tweet-mentah.rds')

##Load dataset csv / excel ##Anda dapat membaca data lain
tweets <- read.csv('Apaini.csv')

##Membaca data txt 
mycamus = readLines("kamus.txt")


##Karena variable data yang di crawling adalah 90 variable
##Maka anda dapat memilih variable yang ingin anda gunakan
tweets <- tweets %>% select(text) #data yang dipilih hanya tweet




##Langkah dibawah tidak perlu anda lakukan kecuali ingin mengubah data


#Menggubah menjadi data frame
tweets <-as.data.frame(tweets)

#Mengubah data menjadi karakter
tweets <- as.character(tweets)

#Mengubah data menjadi matrix
tweets <- as.matrix(tweets)