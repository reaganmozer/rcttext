## Process and clean the essay texts


setwd( here::here( "../Replication" ) )
options(stringsAsFactors = FALSE)
library(tidyverse)
library(haven)

dat=read_dta("Data/g1g2_analyticfile_public.dta")

# include only those who have all demographics and pre-test scores
check.admin = select(dat, s_id, s_white_num:s_ses_high, s_dib_score_1819w, s_maprit_1819w, s_itt_consented)
table(apply(check.admin, 1, function(x)sum(is.na(x))))

check.admin$anyNA = sapply(1:nrow(check.admin), function(x) sum(is.na(check.admin[x,-c(1)])))

rm.ids = check.admin$s_id[check.admin$anyNA>0]
dat = dat %>% filter(!s_id %in% rm.ids)

rm(check.admin, rm.ids)

rater.scores = select(dat, s_id, t_moreresearchid, sch_researchid, s_itt_consented,
                      s_sci_claim_r1:s_ss_response)


dat = select(dat, s_id, t_moreresearchid, sch_researchid, s_itt_consented,
             s_grade_center, s_maprit_1819w,
             s_sci_write, s_ss_write,
             s_sci_response, s_ss_response)
dat = dat %>% rename(t_id=t_moreresearchid,
                     sch_id=sch_researchid,
                     more=s_itt_consented,
                     grade=s_grade_center)
dat$grade=as.factor(dat$grade)
levels(dat$grade)=c(1,2)

dat %>% group_by(grade,more) %>% summarise(n.students=length(unique(s_id)))


sci = dat[!is.na(dat$s_sci_write),-c(grep("ss",names(dat),fixed=T))]
soc = dat[!is.na(dat$s_ss_write),-c(grep("sci",names(dat),fixed=T))]

names(sci)[7:8]=names(soc)[7:8]=c("score","text")

sci$subject="science"
soc$subject="social"

dat2 = rbind(sci, soc)
dat2 %>% group_by(grade,subject) %>% summarise(n.students=length(unique(s_id)))
dat2 %>% group_by(subject) %>% summarise(mean.nchar = mean(nchar(text)),
                                         sd.nchar=sd(nchar(text)))


dat2 = select(dat2, s_id, t_id, sch_id, grade, subject, more, s_maprit_1819w, score, text)
apply(dat2,2,anyNA) # check for no missing


library(quanteda)
library(hunspell)
library(tm)

# Some data cleaning/autocorrecting of the misspelled words
dat2$text.sc = iconv(tolower(dat2$text), from="UTF-8", to="ASCII", sub="")
dat2$text.sc = gsub("-", " - ", dat2$text.sc, fixed=T)
dat2$text.sc = gsub("s's", "s'", dat2$text.sc, fixed=T)
dat2$text.sc = gsub("s'", "s' ", dat2$text.sc, fixed=T)
dat2$text.sc = gsub(".", ". ", dat2$text.sc, fixed=T)
dat1= dat2

dat2$text.sc = gsub(" aateroid "," asteroid ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" advanture "," adventure ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" accidently "," accidentally ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" apiniter "," painter ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" beause "," because ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" leonard "," leonardo ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" dinos "," dinosaurs ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" dino's "," dinosaurs ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" dino "," dinosaur ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" shoud "," should ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" wiht "," with ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" earhar "," earhart ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" earhard "," earhart ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" leonarod "," leonardo ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" thinked "," think ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("da vinci","davinci",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("da v inci"," davinci ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" nad "," and ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" dont "," do not ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" theat "," that ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" rees "," trees ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" studing "," studying ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" striked "," strike ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" opinon "," opinion ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" oone "," one ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" cuting "," cutting ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" sthe "," the ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" sould  "," should ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" tryed  "," tried ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" writed  "," write ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" rianforest  "," rainforest ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" i've "," i have ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" i'll "," i will ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" they're "," they are ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" flyed "," fly ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" drawed "," drew ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub(" alantic "," atlantic ",dat2$text.sc,fixed=T)



reads.dict=read.table("Data/reads_dict.txt")
hunspell::dictionary("en_US",add_words = reads.dict$V1)
vocab = tolower(colnames(dfm(c(dat2$text.sc))))
table(hunspell_check(vocab))

mis = sort(vocab[hunspell_check(vocab,hunspell::dictionary("en_US",add_words = reads.dict$V1))==F])
rm = which(removePunctuation(mis)=="" | removeNumbers(mis)=="" | removePunctuation(removeNumbers(mis))=="")
rm2=which(startsWith(mis, "#") | startsWith(mis, "1") | startsWith(mis,"2") | startsWith(mis,"3") | startsWith(mis,"8"))
mis = mis[-c(rm,rm2)]
mis = mis[nchar(mis)>3]

mis = mis[!mis%in%c("mona","striked","dinos","venus","wolly","xbox","youtube","ipads")]

spellcorrect = function(x){
  h=hunspell_suggest(x)[[1]][1]
  return(cbind(x,h))
}
subs=as.data.frame(do.call(rbind,lapply(1:length(mis),function(x)spellcorrect(mis[x]))))
subs=subs[!is.na(subs$h),]
dfm = dfm(dat2$text.sc,tolower=T)
counts=c()
for (j in 1:nrow(subs)){counts[j]=colSums(dfm)[colnames(dfm)==tolower(subs$x[j])]}
subs$counts=counts
subs=subs[with(subs,order(counts,decreasing=T)),]

subs$x=paste0(" ", as.character(subs$x),"")
subs$h=paste0(" ", as.character(subs$h),"")

tmp = as.character(gsub(subs$x[1], subs$h[1],tolower(dat2$text.sc), fixed=T))
for (j in 2:nrow(subs)){
  tmp = gsub(subs$x[j], subs$h[j], tmp, fixed=T)
}

tmp = tolower(tmp)
tmp = gsub(" they're "," they are ",tmp,fixed=T)
tmp = gsub(" i'll "," i will ",tmp,fixed=T)
dat2$text.sc=tolower(tmp)
dat2$text.sc = gsub(" airplain "," airplane ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("airplanee"," airplane ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("airplanees"," airplanes ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("beause"," because ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("oceann"," ocean ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("airplanee"," airplane ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("did't"," did not ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("wouldd"," would ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("wantd"," wanted ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("coildn't"," could not ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("coild"," could ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("probems"," problems ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("probem"," problem ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("studf"," stuff ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("machinee"," machine ",dat2$text.sc,fixed=T)
dat2$text.sc = gsub("anotherr"," another ",dat2$text.sc,fixed=T)

# output file to analyze in LIWC/elsewhere



out=textstat_frequency(dfm(dat2$text.sc))
out$check=hunspell_check(out$feature)
head(out[out$check==F,],n=20)


dat2$spellcheck = 1*(dat1$text.sc!=dat2$text.sc)

dat2 = dat2[with(dat2,order(s_id,subject)),]
rownames(dat2)=NULL
text = select(dat2, s_id, grade, subject, more, text, text.sc)
meta = select(dat2, -text, -text.sc)

meta = meta[!text$text %in%c("","can't decipher"),]
text = text[!text$text%in%c("","can't decipher"),]

text$text.sc = tm::stripWhitespace(text$text.sc)

write.csv(text, file="Generated Data/text_g1g2_consented.csv", row.names=F)
save(meta, text, rater.scores, file="Generated Data/meta.RData")

library(tada)

# Write cleaned essays to text files for analysis via TAACO
dnames = paste0("s", text$s_id, "_grade", text$grade, "_", text$subject, ".txt")

prep_external(text$text.sc,dir="Generated Data/text_files/",docnames=dnames)
