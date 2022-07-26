## Estimate impacts on the prevalence and frequency of use of a list of pre-identified
# "concept" words and phrases within each grade and subject

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
options(stringsAsFactors = F)

library(dplyr)
library(tidyverse)
library(tm)
library(textreg)
library(knitr)
library(kableExtra)
library(quanteda)
load("Generated Data/meta.RData")

cwords = read.csv("Data/concept_words.csv")
cwords$Subject=as.factor(cwords$Subject)
levels(cwords$Subject)=c("science","social")
cwords$Taught = as.factor(cwords$Taught)

# Create corpus to analyze for concept word frequencies
tk = tokens_ngrams(tokens(text$text.sc, remove_punct=T, remove_symbols=T, 
                          remove_numbers=T), n=1:2)
dfm = dfm(tk, select=unique(cwords$Concept_word))

cwords.freq = cbind(text[,1:4], as.matrix(dfm))

counts = cwords.freq %>% group_by(grade, subject, more) %>%
  pivot_longer(cols=celebrate:hire, names_to="term",
               values_to="count")

sci1 = filter(counts, grade==1, subject=="science", term %in%
                unique(cwords$Concept_word[cwords$Grade==1 & cwords$Subject=="science"]))
sci2 = filter(counts, grade==2, subject=="science", term %in%
                unique(cwords$Concept_word[cwords$Grade==2 & cwords$Subject=="science"]))

soc1 = filter(counts, grade==1, subject!="science", term %in%
                unique(cwords$Concept_word[cwords$Grade==1 & cwords$Subject!="science"]))
soc2 = filter(counts, grade==2, subject!="science", term %in%
                unique(cwords$Concept_word[cwords$Grade==2 & cwords$Subject!="science"]))


all = rbind(sci1, sci2, soc1, soc2)
all2 = merge(all, cwords, by.x=c("grade","subject","term"),
             by.y=c("Grade","Subject","Concept_word"))

sums = all2 %>% group_by(grade, subject, more, Taught) %>% 
  summarise(n=length(unique(s_id)),
            total.freq = sum(count), 
            num.essays = length(unique(s_id[count>0])),
            prop.essays = num.essays/n)


sums2 = sums %>% group_by(grade, subject, Taught) %>% 
  pivot_wider(names_from=more, values_from=c(total.freq, num.essays, prop.essays,n))


sums3 = sums %>% group_by(grade, subject,more)%>% 
  summarise(n=n[1],
            total.freq=sum(total.freq),
            num.essays=sum(num.essays),
            prop.essays=num.essays/n) 
  

sums3 = sums3 %>% select(-n) %>%
  pivot_wider(names_from=more, values_from=c(total.freq, num.essays, prop.essays))
sums3$Taught="total"
sums3 = select(sums3, grade, subject, Taught, everything())


sums.out = rbind(sums3,sums2[,-c(10:11)])


sig.vals=c()
prev.rates = select(sums2, grade, subject, Taught, num.essays_0, num.essays_1, n_0, n_1)
res = data.frame(diff=NA, p.val=NA, LL=NA, UL=NA)
for (j in 1:nrow(sums2)){
  tmp=prev.rates
  test = prop.test(x=c(tmp$num.essays_1[j], tmp$num.essays_0[j]), 
                   n=c(tmp$n_1[j], tmp$n_0[j]))
  diff=test$estimate[1]-test$estimate[2]
  LL=test$conf.int[1]
  UL=test$conf.int[2]
  res[j,] = c(diff, test$p.value, LL, UL)
}

prev.rates = cbind(prev.rates, res)
prev.rates$p.adj=p.adjust(prev.rates$p.val,"fdr")
sums.out = merge(sums.out, subset(prev.rates,select=c(grade,subject,Taught, p.adj,LL,UL)), by=c("grade","subject","Taught"))


save(sums.out, prev.rates, file="Results/concept_tabs.RData")




