setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
options(stringsAsFactors = F)

library(tm)
library(quanteda)
library(textmatch)
library(data.table)
library(tidyverse)
library(quanteda.sentiment)
library(quanteda.dictionaries)
library(tada)

source("Scripts-tada/utils.R")


load("Generated Data/meta.RData")
all.feats = subset(meta, select=c(s_id, grade, subject))
all.equal(all.feats, subset(text,select=c(s_id, grade,subject)), check.attributes=F)

# Create preliminary text features
feats = tada(text$text, ld=c("TTR","R","K"), terms=c("xxx"))
all.feats = cbind(all.feats, feats)

# Load LIWC-generated features
all.feats = liwc(file="Generated Data/LIWC_g1g2_consented_sc.csv",all.feats, by=c("s_id","grade","subject"))
dim(all.feats)

# Calculate Word2Vec projections for each essay on 50 dimensions
x=clean_txt(text$text.sc)
proj = extract_w2v(x, dim=50)

all.feats = cbind(all.feats, proj) # Combine with other features
rm(proj)

# Measure distances between each essay and it's corresponding reference text(s) and add to features
load("Data/ref_texts.RData")
essays = all.text
n.refs = length(all.refs)
all.raw = clean_txt(c(all.refs, essays))
Z = c(rep(1,n.refs), rep(0,length(essays))) # Indicator for reference texts
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


refs = data.frame(index.0=1:n.refs, name=ref.names)
all.dists = merge(all.dists,refs, by=c("index.0"))
all.dists = all.dists[with(all.dists,order(index.1,index.0)),]

meta = get_meta()
tmp = subset(meta, select=c(s_id, subject, grade))
tmp$MID = (1:nrow(tmp))+n.refs # temporary ID for merging
all2 = merge(tmp, all.dists, by.x="MID", by.y="index.1")

sci.g1 = subset(all2[all2$subject=="science" & all2$grade==1 & all2$name=="G1.sci.both",],select=-c(MID, index.0, name))
soc.g1 = subset(all2[all2$subject=="social" & all2$grade==1 & all2$name=="G1.soc.both",],select=-c(MID, index.0, name))

sci.g2 = subset(all2[all2$subject=="science" & all2$grade==2 & all2$name=="G2.sci",],select=-c(MID, index.0, name))
soc.g2 = subset(all2[all2$subject=="social" & all2$grade==2 & all2$name=="G2.soc.both",],select=-c(MID, index.0, name))

table(meta$grade,meta$subject)

all.dists = rbind(sci.g1, soc.g1, sci.g2, soc.g2)
rownames(all.dists)=NULL

# Merge with existing feature set
all.info = merge(all.dists, all.feats, by=c("s_id","grade","subject"))
gdata::keep(all.info, text, meta, sure=T)

# Add output from TAACO
add = read.csv("Generated Data/taaco_results.csv")
add$s_id = sapply(1:nrow(add),function(x)as.numeric(gsub("s","",strsplit(add$Filename[x], "_", fixed=T)[[1]][1])))
add$grade = sapply(1:nrow(add),function(x)as.numeric(gsub("grade","",strsplit(add$Filename[x], "_", fixed=T)[[1]][2])))
add$subject = sapply(1:nrow(add),function(x)gsub(".txt","",strsplit(add$Filename[x], "_", fixed=T)[[1]][3]))
add = add[,-c(grep("para",names(add)))] # remove paragraph-level measures of cohesion
add = select(add, s_id, grade, subject, everything(), -Filename)

all.info = merge(all.info, add, by=c("s_id", "grade","subject"))


all.info = merge(meta, all.info, by=c("s_id","subject", "grade"))

dim(all.info)
table(apply(all.info,2,anyNA))

all.info = all.info[with(all.info,order(s_id,subject)),]
save(all.info, file="Generated Data/all.info.RData")
