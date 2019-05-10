library(dplyr)
library(tidyr)



PGA <- PGA_Data_Historical[-3]

PGA <- as_tibble(PGA)
PGA <- PGA %>% spread(key=Variable, value= Value) #transform data to a wide format 


#supprimer les lignes où il y a plus de 100NA 
PGA2[] <- lapply(PGA2, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

sapply(PGA2, class)
PGA3<- PGA2[rowSums(is.na(PGA2[,c(3:2074)])) <=1000, ]

#enlever les colonnes o? il reste encore 90% de NA
miss <- c()
for(i in 1:ncol(PGA3)) {
  if(length(which(is.na(PGA3[,i]))) > 0.90*nrow(PGA3)) miss <- append(miss,i) 
}
PGA4 <- PGA3[,-miss]

# git checkout -b new-feature
# git push origin new-feature
#Comment supprimer des variables qui contiennent un certain string, reste plus qu'? trouver comment en combiner plusieurs
install.packages("data.table")
library(data.table)
PGA5 <- PGA4[,!grepl("FedExCup", colnames(PGA4))]
setDT(PGA5)
ind = PGA5[, lapply(.SD, function(x) grepl("FedExCup", x, perl=TRUE))] 
PGA5[,which(colSums(ind) == 0), with = FALSE]