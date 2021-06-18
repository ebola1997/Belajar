#Data Latih dan Uji adalah 60:40
set.seed(1234)
library("caTools")
Split2 = sample.split(tugas$label, SplitRatio = 0.70)
col <- c("score", "total", "label")

prakerja_train2 <- tugas[Split2==TRUE, col]
prakerja_uji2 <- tugas[Split2==FALSE, col]

table(prakerja_train2$label)
table(prakerja_uji2$label)
table(hasil$label)

#Pengujian/Perhitungan
#kernel radial
mymodel5 <- svm(label~., data = prakerja_train2, kernel = "radial", cost = .1, scale = FALSE)
print(mymodel5)
plot(mymodel5, prakerja_train2[,col])
summary(mymodel5)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = prakerja_train2, kernel = "radial", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)
plot(tuned)
bestmodel5 <- tuned$best.model
summary(bestmodel5)
p <- predict(bestmodel5, prakerja_uji2[,col])
plot(bestmodel5, prakerja_uji2[,col])
plot(p)
kernel <- table(Aktual = prakerja_uji2[,3], Prediksi = p) #Tabel Confusion
kernel
confusionMatrix(kernel)
sum(diag(kernel))/sum(kernel) #Akurasi x 100%
1-sum(diag(kernel))/sum(kernel)# Miss clasification x100%
kernel/sum(kernel) #Prediksi Data x 100%
kernel/colSums(kernel) #Hasil Pembagian Data x 100%

mean(p== prakerja_uji2[,3]) #CAR Akurasi tanpa table CM

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
mymodel6 <- svm(label~., data = prakerja_train2, kernel = "linear", cost = 0.01, scale = FALSE)
print(mymodel6)
plot(mymodel6, prakerja_train2[,col])
summary(mymodel6)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = prakerja_train2, kernel = "linear", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)
plot(tuned)
bestmodel6 <- tuned$best.model
summary(bestmodel6)
p <- predict(bestmodel6, prakerja_uji2, type="class")
plot(bestmodel6, prakerja_uji2, type="class")
plot(p)
linear <- table(Aktual = prakerja_uji2[,3], Prediksi = p) #Tabel Confusion
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
mymodel7 <- svm(label~., data = prakerja_train2, kernel = "polynomial", cost = 1, scale = FALSE)
print(mymodel7)
plot(mymodel7, prakerja_train2[,col])
summary(mymodel7)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = prakerja_train2, kernel = "polynomial", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)
plot(tuned)
bestmodel7 <- tuned$best.model
print(bestmodel7)
summary(bestmodel7)
p <- predict(bestmodel7, prakerja_uji2, type="class")
plot(bestmodel7, prakerja_uji2, type="class")
plot(p)
polynomial <- table(Aktual = prakerja_uji2[,3], Prediksi = p) #Tabel Confusion
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

#sigmoid
mymodel8 <- svm(label~., data = prakerja_train2, kernel = "sigmoid", cost = 0.1, scale = FALSE)
print(mymodel8)
plot(mymodel8, prakerja_train2[,col])
summary(mymodel8)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = prakerja_train2, kernel = "sigmoid", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)
plot(tuned)
bestmodel8 <- tuned$best.model
print(bestmodel8)
summary(bestmodel8)
p <- predict(bestmodel8, prakerja_uji2, type="class")
plot(bestmodel8, prakerja_uji2, type="class")
plot(p)
sigmoid <- table(Aktual = prakerja_uji2[,3], Prediksi = p) #Tabel Confusion
sigmoid
confusionMatrix(sigmoid)
sum(diag(sigmoid))/sum(sigmoid) #Akurasi x 100%
1-sum(diag(sigmoid))/sum(sigmoid)# Miss clasification x100%
sigmoid/sum(sigmoid) #Prediksi Data x 100%
sigmoid/colSums(sigmoid) #Hasil /
#Perhitungan
#Recall (Sensitivity)
recall <- (0.9+0.8+0.9)/3
recall
#Perhitungan Precision (Pos Pred Value)
precision <- (0.9+0.9+0.9)/3
precision
#Perhitungan Fmeasure
fmeasure <- 2*recall*precision/(recall+precision)
fmeasure

Akurasi <- (.99+1+1+.92)/4
Akurasi

Recall <- (.96+1+1+.86)/4
Recall

Precision <- (.96+1+1+.9)/4
Precision

Fmeas <- (.96+1+1+.88)/4
Fmeas

?F_meas
