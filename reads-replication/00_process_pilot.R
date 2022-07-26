## Process pilot data (a subset of G1 science essays from a different evaluation) used to train the ML model

setwd( here::here( "../Replication" ) )

options(stringsAsFactors = F)

library(tm)
library(quanteda)
library(textmatch)
library(data.table)
library(tidyverse)



pilot = read.csv("Data/G1sci_pilot_dat.csv")

essay.text = pilot$text
essay.text = gsub(" shoud ", " should ", essay.text, fixed=T)
# Create preliminary text features
nchar = nchar(essay.text)
summary(nchar)

dfm = dfm(essay.text)
ld = textstat_lexdiv(dfm, measure=c("TTR","R","K"))
read = textstat_readability(essay.text, measure=c("Flesch","Flesch.Kincaid", "ARI", "ELF",
                                                    "meanWordSyllables"))
ent = textstat_entropy(dfm(essay.text), margin="documents")

all.feats = select(pilot, ID, Q1, Q2, more)
all.feats = cbind(all.feats, ld[,-c(1)], read[,-c(1)], ent[,-c(1)])

# Count illegible words/phrases (transcribed as XXX)
tmp=as.matrix(dfm)[,which(colnames(dfm)=="xxx")]
all.feats$XXX = as.numeric(tmp)


# Calculate Word2Vec projections for each essay on 50 dimensions
load("Generated Data/glove.50d.RData")
source("Scripts/utils.R")

glove.vocab = unique(removePunctuation(names(glove.50d)))
glove = data.frame(t(glove.50d))
glove$words = rownames(glove)
k = ncol(glove)
glove2 = glove[,c(k, 1:(k-1))]

all.text = clean_txt(essay.text)
proj = softmaxreg::wordEmbed(all.text, dictionary=glove2, meanVec=TRUE)
proj = as.data.frame(proj)
names(proj) = paste0("W2V.d", 1:50)

all.feats = cbind(all.feats, proj) # Combine with other features



rm(glove.50d, glove2, glove, glove.vocab)

# Load LIWC-generated features
liwc=read.csv("Generated Data/pilot/pilot_LIWC.csv")
liwc = liwc[,-c(2:5)]
names(liwc)[1]="ID"

all.feats = merge(all.feats, liwc,  by=c("ID"))
dim(all.feats)

all.feats$WCperChar = all.feats$WC/nchar


# Load TAACO-generated results
add = read.csv("Generated Data/pilot/pilot_taaco_results.csv")
add$ID = as.numeric(gsub(".txt", "", add$Filename))

add = add[,-c(grep("para",names(add)))] # remove paragraph-level measures of cohesion
#add = add[,-c(grep("adjacent",names(add)))] # remove adjacent overlap  between sentences
add = select(add,ID, everything(), -Filename)

all.feats = merge(all.feats, add, by=c("ID"))


library(caret)
f = findLinearCombos(all.feats)
f
all.feats = all.feats[,-c(f$remove)]
nz = nearZeroVar(all.feats[,-c(1:4)],freqCut=99/1, uniqueCut=2, names=T)
nz

all.feats = all.feats[,!names(all.feats)%in%nz]
cor = findCorrelation(cor(all.feats[,-c(1:4)]), cutoff=0.95, names=T)
cor

all.feats = all.feats[,!names(all.feats)%in%cor]
cor = findCorrelation(cor(all.feats[,-c(1:4)]), cutoff=0.9, names=T, verbose=T,exact=T)
all.feats = all.feats[,!names(all.feats)%in%cor]


# Code to generate similarity scores for the pilot data.
# This is not helpful for our case study since we only have G1 science writing
# so the similarity scores for the other grades/subjects are meaningless.
if (FALSE){
  load("Data/ref_texts.RData")
  source("Scripts/utils.R")
  all.refs = all.refs[1:3] # only use G1 science writing
  n.refs = length(all.refs)
  all.raw = clean_txt(c(all.refs, essay.text))
  Z = c(rep(1,n.refs), rep(0,length(essay.text))) # Indicator for reference texts
  dfm1 = quanteda::dfm(all.raw, valuetype="fixed", stem=F)
  tdm.dists = pair_distances(dfm1, Z, include="cosine", form="data.frame")
  all.dists = tdm.dists[with(tdm.dists,order(index.0, as.numeric(index.1))),]
  names(all.dists)[grep("cosine",names(all.dists))]="tdm.raw.cosine"


  proj.refs = as.data.frame(softmaxreg::wordEmbed(clean_txt(all.refs), dictionary=glove2, meanVec=TRUE))
  names(proj.refs)=names(proj)
  proj.all = rbind(proj.refs, proj)


  W2V.dists = pair_distances(proj.all, Z, include="cosine",form="data.frame")
  W2V.dists = W2V.dists[with(W2V.dists, order(index.0, as.numeric(index.1))),]
  names(W2V.dists)[3]="W2V.d50.cosine"
  all.dists = merge(all.dists, W2V.dists, by=c("index.1","index.0"))

  refs = data.frame(index.0=1:n.refs, name=ref.names[1:n.refs])
  all.dists = merge(all.dists,refs, by=c("index.0"))
  all.dists = all.dists[with(all.dists,order(index.1,index.0)),]
  all.dists = all.dists[all.dists$name=="G1.sci.both",]

  tmp = data.frame(ID=all.pilot$ID)
  tmp$MID = (1:nrow(tmp))+n.refs # temporary ID for merging
  all2 = merge(tmp, all.dists, by.x="MID", by.y="index.1")

  pilot.dists = select(all2, -MID, -index.0, -name)
  all.pilot = merge(all.pilot, pilot.dists, by="ID")
}

all.pilot = all.feats
save(all.pilot,file="Generated Data/all.pilot.RData")
