#!/usr/bin/env Rscript

library(readr)

dados <- read_delim("DADOS_CE081_2022.csv", quote = "\"", col_types = "nccnccccccc", locale = locale(encoding = "ISO-8859-1", decimal_mark = ","), delim = ";")
vq <- c(2,3,5,6,7,8,9,10)
tamanho <- c(length(vq),length(vq))
result <- array(NA, dim=tamanho)
colnames(result) <- paste0("Q", (vq-1))
rownames(result) <- paste0("Q", (vq-1))
I <- array(0, dim=tamanho)
J <- array(0, dim=tamanho)
for (k in c(1:(length(vq)-1))){
    for (l in c((k+1):length(vq))){
        item <- table(dados[,c(vq[k],vq[l])])
        I[[k,l]] <- length(item[1,])
        J[[k,l]] <- length(item[,1])
        result[[k,l]] <- (chisq.test(item))$statistic
    }
}
T <- round((sqrt((result/44)/((I-1)*(J-1))))*100, 2)
#print(T[1:7,2:8], na.print="")
plots <- 5
bigt <- tail(sort(c(T)), n=plots)[1]
max <- which(T >= bigt, arr.ind=TRUE)
aplotar <- list(plots)

for (m in 1:nrow(max)){
    print(paste(paste0("Q",vq[max[m,1]]-1), paste0("Q", vq[max[m,2]]-1)))
    aplotar[[m]] <- table(dados[c(vq[max[m,1]],vq[max[m,2]])])
}

for (p in 1:plots){
    barplot(aplotar[[p]])
}

#mean(dados$Q3)
#median(dados$Q3)
#summary(dados$Q3)
#quantile(dados$Q3)

#stripchart(dados$Q3)

#boxplot(dados$Q3)

#range(dados$Q3)
#IQR(dados$Q3)
#var(dados$Q3)
#sd(dados$Q3)
#hist(dados$Q3)
#plot(ecdf(dados$Q3))

