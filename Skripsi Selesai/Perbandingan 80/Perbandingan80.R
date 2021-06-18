Split3 = sample.split(tugas$label, SplitRatio = 0.80)
col <- c("score", "total", "label")

prakerja_train3 <- tugas[Split3==TRUE, col]
prakerja_uji3 <- tugas[Split3==FALSE, col]

table(prakerja_train3$label)
table(prakerja_uji3$label)
table(hasil$label)


#Pengujian/Perhitungan
#kernel radial
mymodel9 <- svm(label~., data = prakerja_train3, kernel = "radial", cost = .1, scale = FALSE)
print(mymodel9)
plot(mymodel9, prakerja_train3[,col])
summary(mymodel9)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = prakerja_train3, kernel = "radial", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)
plot(tuned)
bestmodel9 <- tuned$best.model
summary(bestmodel9)
p <- predict(bestmodel9, prakerja_uji3[,col])
plot(bestmodel9, prakerja_uji3[,col])
plot(p)
kernel <- table(Aktual = prakerja_uji3[,3], Prediksi = p) #Tabel Confusion
kernel
confusionMatrix(kernel)
sum(diag(kernel))/sum(kernel) #Akurasi x 100%
1-sum(diag(kernel))/sum(kernel)# Miss clasification x100%
kernel/sum(kernel) #Prediksi Data x 100%
kernel/colSums(kernel) #Hasil Pembagian Data x 100%

mean(p== prakerja_uji3[,3]) #CAR Akurasi tanpa table CM

#Perhitungan
#Perhitungan Recall (Sensitivity)
recall <- (1+1+0.9)/3
recall
#Perhitungan Precision (Pos Pred Value)
precision <- (0.9+1+1)/3
precision
#Perhitungan Fmeasure
fmeasure <- 2*recall*precision/(recall+precision)
fmeasure


#Kernel linear
mymodel10 <- svm(label~., data = prakerja_train3, kernel = "linear", cost = 0.01, scale = FALSE)
print(mymodel10)
plot(mymodel10, prakerja_train3[,col])
summary(mymodel10)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = prakerja_train3, kernel = "linear", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)
plot(tuned)
bestmodel10 <- tuned$best.model
summary(bestmodel10)
p <- predict(bestmodel10, prakerja_uji3, type="class")
plot(bestmodel10, prakerja_uji3, type="class")
plot(p)
linear <- table(Aktual = prakerja_uji3[,3], Prediksi = p) #Tabel Confusion
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
mymodel11 <- svm(label~., data = prakerja_train3, kernel = "polynomial", cost = 1, scale = FALSE)
print(mymodel11)
plot(mymodel11, prakerja_train3[,col])
summary(mymodel11)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = prakerja_train3, kernel = "polynomial", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)
plot(tuned)
bestmodel11 <- tuned$best.model
print(bestmodel11)
summary(bestmodel11)
p <- predict(bestmodel11, prakerja_uji3, type="class")
plot(bestmodel11, prakerja_uji3, type="class")
plot(p)
polynomial <- table(Aktual = prakerja_uji3[,3], Prediksi = p) #Tabel Confusion
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
mymodel12 <- svm(label~., data = prakerja_train3, kernel = "sigmoid", cost = 0.1, scale = FALSE)
print(mymodel12)
plot(mymodel12, prakerja_train3[,col])
summary(mymodel12)
#pengecekan 10 fold cross untuk nilai cost terbaik
tuned <- tune(svm, label~., data = prakerja_train3, kernel = "sigmoid", ranges= list(cost=c(0.001,0.01,1,1,10,100)))
summary(tuned)
plot(tuned)
bestmodel12 <- tuned$best.model
print(bestmodel12)
summary(bestmodel12)
p <- predict(bestmodel12, prakerja_uji3, type="class")
plot(bestmodel12, prakerja_uji3, type="class")
plot(p)
sigmoid <- table(Aktual = prakerja_uji3[,3], Prediksi = p) #Tabel Confusion
sigmoid
confusionMatrix(sigmoid)
sum(diag(sigmoid))/sum(sigmoid) #Akurasi x 100%
1-sum(diag(sigmoid))/sum(sigmoid)# Miss clasification x100%
sigmoid/sum(sigmoid) #Prediksi Data x 100%
sigmoid/colSums(sigmoid) #Hasil /
#Perhitungan
#Recall (Sensitivity)
recall <- (1+0.8+0.9)/3
recall
#Perhitungan Precision (Pos Pred Value)
precision <- (0.8+0.9+0.9)/3
precision
#Perhitungan Fmeasure
fmeasure <- 2*recall*precision/(recall+precision)
fmeasure

Akurasi <- (1+1+1+.91)/4
Akurasi

Recall <- (1+1+1+.9)/4
Recall

Precision <- (1+1+1+.86)/4
Precision

Fmeas <- (1+1+1+.88)/4
Fmeas
