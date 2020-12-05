

#Membaca data tweet yang sudah di processing
twit_clean <-read.csv("prakerjaClean.csv",header=TRUE)


#ambil kata kata untuk skoring
positif <- scan("s-pos.txt",what="character",comment.char=";")
negatif <- scan("s-neg.txt",what="character",comment.char=";")
kata.positif = c(positif)
kata.negatif = c(negatif)
score.sentiment = function(twit_clean, kata.positif, kata.negatif, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(twit_clean, function(kalimat, kata.positif, kata.negatif) {
    kalimat = gsub('[[:punct:]]', '', kalimat)
    kalimat = gsub('[[:cntrl:]]', '', kalimat)
    kalimat = gsub('\\d+', '', kalimat)
    kalimat = tolower(kalimat)
    
    list.kata = str_split(kalimat, '\\s+')
    kata2 = unlist(list.kata)
    positif.matches = match(kata2, kata.positif)
    negatif.matches = match(kata2, kata.negatif)
    positif.matches = !is.na(positif.matches)
    negatif.matches = !is.na(negatif.matches)
    score = sum(positif.matches) - (sum(negatif.matches))
    return(score)
  }, kata.positif, kata.negatif, .progress=.progress )
  scores.df = data.frame(score=scores, text=twit_clean)
  return(scores.df)
}

#melakukan skoring text
hasil = score.sentiment(twit_clean$text, kata.positif, kata.negatif)
head(hasil)

# melakukan labeling pada nilai yang kurang dari 0 sebagai negatif dan lebih dari = 0 adalah positif
hasil$klasifikasi<- ifelse(hasil$score<0, "Negatif",ifelse(hasil$score==0,"Netral","Positif"))
hasil$klasifikasi

#Tukar Row
data <- hasil[c(3,1,2)]
View(data)
write.csv(data, file = "data ter labeli.csv")

#Memisahkan twit
data.pos <- hasil[hasil$score>0,]
View(data.pos)
write.csv(data.pos, file = "data-pos.csv")

data.neg <- hasil[hasil$score<0,]
View(data.neg)
write.csv(data.neg, file = "data-neg.csv")

data.net <- hasil[hasil$score==0,]
View(data.net)
write.csv(data.net, file = "data-net.csv")