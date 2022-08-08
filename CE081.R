#!/usr/bin/env Rscript

library(readr)
library(gridExtra)
dicionario <- c("ID",
                "No momento, você exerce algum tipo de atividade remunerada?",
                "Você já frequentou algum curso de nível superior antes de ser aprovado para o curso de Estatística?",
                "Em média, quanto tempo - em minutos - você demora para chegar a Universidade?",
                "Você estudou Estatística no Ensino Médio?",
                "Você tem algum familiar formado em Estatística?",
                "Qual o principal motivo do seu ingresso no curso de Estatística?",
                "No vestibular, o curso de estatística...",
                "Como você avalia sua habilidade em Matemática?",
                "Como você avalia sua habilidade em Computação?",
                "Qual a sua principal expectativa após a conclusão do curso?")

names(dicionario) <- c("ID", paste0("Q", 1:10))
dados <- read_delim("DADOS_CE081_2022.csv",
                    quote = "\"",
                    col_types = "nccnccccccc",
                    locale = locale(encoding = "ISO-8859-1",
                                    decimal_mark = ","),
                    delim = ";")
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
plots <- 5
bigt <- tail(sort(c(T)), n=plots)[1]
max <- which(T >= bigt, arr.ind=TRUE)
aplotar <- list(plots)
T[is.na(T)] <- ""
plots <- nrow(max)
for (m in 1:plots){
    aplotar[[m]] <- table(dados[c(vq[max[m,1]],vq[max[m,2]])])
    aplotar[[m]] = prop.table(aplotar[[m]], margin=2)
}

arquivo <- 0
resolucao <- 150

png(filename=paste0("plot", sprintf("%02d", arquivo), ".png"),
    width=800, height=600, res=resolucao)
arquivo <- arquivo+1
grid.table(T[1:7,2:8])

for (p in 1:plots){
    png(filename=paste0("Q",vq[max[p,1]]-1, "xQ", vq[max[p,2]]-1, ".png"),
        width=800, height=600, res=resolucao)
    barplot(aplotar[[p]])
}
