library(textclean)
install_github("nurandi/katadasaR")
library(katadasaR)
library(tokenizers)
library(stopwords)
library(wordcloud)
library(RColorBrewer)


#### CLEANING, SUBBING, STEMMING, TOKENIZING, WORDCLOUD

## Contoh file diberikan nama tweets (dapat diganti sesuka hati)

tweets <- read.csv("PrakerjaCrawling.csv") #Baca Data Tweet Crawling
tweets <- tweets$text %>%  #Memilih hanya mengambil data tweet
  as.character()
head(tweets)

tweets <- gsub( "\n"," ",tweets) #Subbing

tweets <- tweets %>% 
  replace_html() %>% # mengganti html dengan blank 
  replace_url()   # mengganti URLs dengan blank

replace_emoji(tweets[20]) #Mengganti emoji pada tweet tertentu
replace_html(replace_emoji(tweets[20])) #Menghilangkan pengganti emoji pada tweet

# Cara mengganti emojo dan menghilangkan pada semua tweet
tweets <- tweets %>% 
  replace_emoji(.) %>% 
  replace_html(.)

# Menghilangkan Mentions dan Hashtag
tweets <- tweets %>% 
  replace_tag(tweets, pattern = "@([A-Za-z0-9_]+)",replacement="") %>%  # menghilangkan mentions
  replace_hash(tweets, pattern = "#([A-Za-z0-9_]+)",replacement="")      # menghilangkan hashtags

# memasukan lexicon (kamus) bahasa Indonesia
spell.lex <- read.csv("colloquial-indonesian-lexicon.csv")

# mengganti kata slang dengan kata yang terdapat pada lexicon
tweets <- replace_internet_slang(tweets, slang = paste0("\\b",
                                                        spell.lex$slang, "\\b"),
                                 replacement = spell.lex$formal, ignore.case = TRUE)

# Menghilangkan tanda pada bacaan titik dan menjadikan ke lower text dan merapihkan kata
tweets <- strip(prakerja$text)
tweets <- as.data.frame(tweets)

#Menghilangkan tweet yang sama atau tweet RT
tweets <- tweets %>% 
  as.data.frame() %>% 
  distinct()

# number of tweet rows after duplicated text removed
nrow(tweets)

#Memasukan Kata Dasar Bahasa Indonesia
katadasaR("makanan") #Contoh = Akan Menghasilkan kata Makan

#Menghapus kata imbuhan dan mengganti jadi kata dasar
#Menambahkan Fungsi stemming pada R
stemming <- function(x){
  paste(lapply(x,katadasar),collapse = "")}

#Mengaplikasikan fungsi stemming pada data tweet
tweets <- lapply(tokenize_words(tweets[]), stemming)
words[32] #contoh tweet yang sudah di stemming (angka 32 adalah contoh tweet 32)

#Mengaplikasikan Tokenizing data tweet (memecah data tweet)
tweets <- tokenize_words(tweets)

#Membuat stopwords dan Mengaplikasikan stopwords
myStopwords <- readLines("stopword_list_id_2.txt") #File stopword dapat diganti sesuai dengan kamus yang anda punya
tweets <- as.character(tweets) #Mengubah data menjadi karakter agar dapat dilakukan proses stopword
tweets <- tokenize_words(tweets, stopwords = myStopwords) # Mengaplikasikan stopword  

# Cara lain Menghapus stopword
myStopwords = readLines("stopwords.txt")
twitclean <- tm_map(twitclean,removeWords,myStopwords) # removeWords adalah task tm untuk menghilangkan text
twitclean <- tm_map(twitclean,removeWords, 
                    c('indonesia','presiden','bilang','')) # Menambahkan text tersendiri yang tidak ada pada kamus/file stopword

#Membuat WordCloud
class(tweets)
tweets <- as.character(tweets)
wordcloud(tweets)

#Membuat wordcloud dengan warna
wordcloud(tweets, min.freq = 4,max.words=1000,random.order=F,colors=brewer.pal(8,"Dark2"))



##CONTOH SULIT Wordcloud dengan UI
twitter<- vroom(here("PrakerjaCrawling.csv"))
tweet<- twitter$text
ui <- fluidPage(
  titlePanel("PRAKERJA TWITTER DATA"),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Scatterplot", plotOutput("scatterplot")), 
                # Plot
                tabPanel("Tweet dalam bahasa indonesia", DT::dataTableOutput('tbl')), # Output Data Dalam Tabel
                tabPanel("Wordcloud", plotOutput("Wordcloud"))
    )
  )
)