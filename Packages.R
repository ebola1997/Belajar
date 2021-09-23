###MENGINSTALL DAN MENGAPLIKASIKAN PACKAGES YANG TERDAPAT PADA R UNTUK ANALISIS SENTIMEN DATA TWITTER

#CONTOH INSTALL
install.packages("tm")

#CONTOH MENJALANKAN/MEMANGGIL PACKAGES
library(tm) #textmining
library(twitteR) #crawling twitter
library(rtweet) #crawling twitter terbaru
library(dplyr) #mengganti / memanipulasi data menjadi data frame
library(NLP) #Membaca data pada colom dataset
library(tidyr) #merapihkan data, manipulasi/transformasi data (menyimpan data)
library(tidytext) #textmining
library(textmineR) #textmining
library(ggplot2) #membuat grafik yang merepresentasikan data numerik dan kategorik baik univariat maupun multivariat secara simultan.
library(purrr) # Fungsi programing tools
library(ROAuth) #Authentication API
library(RCurl) #Authentication melalui URL
library(expss) #Membaca/membuat file xpss
library(csv) #Membaca/membuat file csv
library(stringr) # Untuk Operasi String 
library(SentimentAnalysis) #Tujuannya adalah untuk memperoleh opini dari data dengan kamus dasar sentimen analysis
library(SnowballC)
library(plyr) #Untuk memecah atau menggabungkan data
library(sentimentr) #Tujuannya mengklasifikasikan text polarity
library(e1071) #Untuk memasukan metode analisis sentimen (Naivebayes, SVM, Decision Tree, dll)
library(caret) #Untuk menjalankan proses perhitungan
library(SVMMatch) #SVM
library(naivebayes) #Naive Bayes
library(RTextTools) #Machine Learning
library(wordcloud2) #Untuk membuat wordcloud
library(syuzhet) #Menampilkan plots
library(DT) #Data Table
library(devtools) #Developer Tools
install_github("nurandi/katadasaR") # Mendownload Kata Dasar
library(katadasaR) #Memuat kata dasar
library(tokenizers) #Task Tokenizing
library(stopwords) #Task Stopwords
library(wordcloud) #Untuk membuat wordcloud
library(RColorBrewer) #Untuk memberikan warna


## Beberapa packages mungkin ada yang memiliki fungsi yg sama seperti twitteR dan rtweet
## Karena rtweet adalah packages terbaru dan terupdate untuk crawling data tweet twitter
## Tetapi kedua packages tersebut tetap dijalankan untuk menutupi fungsi yg tidak dimiliki masing-masing packages
