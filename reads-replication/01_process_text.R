# Process and clean the essay texts
#
# Loads the g1 & g2 public file and makes cleaned datasets for later
# analysis.
#
# Actions:
#
# Correct spelling for a list of commonly misspelled words in corpus
#
# Also write individual text files for each document to be processed
# by third party software.
#


source( "reads-replication/setup.R" )

library( haven )
dat=read_dta( reads_file_path("Data/g1g2_analyticfile_public.dta") )

reads.dict=read.table( reads_file_path("Data/reads_dict.txt") )



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
dat2 %>% group_by(grade,subject) %>%
  summarise(n.students=length(unique(s_id)))
dat2 %>% group_by(subject) %>%
  summarise(mean.nchar = mean(nchar(text)),
                                         sd.nchar=sd(nchar(text)))


dat2 = select(dat2, s_id, t_id, sch_id, grade, subject, more, s_maprit_1819w, score, text)
apply(dat2,2,anyNA) # check for no missing


library(quanteda)
library(hunspell)
library(tm)

dat2$text.sc = iconv(tolower(dat2$text), from="UTF-8", to="ASCII", sub="")

# Clean up some punctuation stuff
dat2$text.sc = repair_spelling( dat2$text.sc,
                                c( "s's", "s'", "." ),
                                c( "s'", "s' ", ". " ) )

dat1 = dat2

# Some data cleaning/autocorrecting of the misspelled words
words = tribble( ~from, ~to,
  " aateroid "," asteroid ",
  " advanture "," adventure ",
  " accidently "," accidentally ",
  " apiniter "," painter ",
  " beause "," because ",
  " leonard "," leonardo ",
  " dinos "," dinosaurs ",
  " dino's "," dinosaurs ",
  " dino "," dinosaur ",
  " shoud "," should ",
  " wiht "," with ",
  " earhar "," earhart ",
  " earhard "," earhart ",
  " leonarod "," leonardo ",
  " thinked "," think ",
  "da vinci","davinci",
  "da v inci"," davinci ",
  " nad "," and ",
  " dont "," do not ",
  " theat "," that ",
  " rees "," trees ",
  " studing "," studying ",
  " striked "," strike ",
  " opinon "," opinion ",
  " oone "," one ",
  " cuting "," cutting ",
  " sthe "," the ",
  " sould  "," should ",
  " tryed  "," tried ",
  " writed  "," write ",
  " rianforest  "," rainforest ",
  " i've "," i have ",
  " i'll "," i will ",
  " they're "," they are ",
  " flyed "," fly ",
  " drawed "," drew ",
  " alantic "," atlantic " )
dat2$text.sc = repair_spelling( dat2$text.sc, words )


hunspell::dictionary("en_US",add_words = reads.dict$V1)
vocab = dat2$text.sc %>% tokens() %>%
  dfm() %>%
  colnames() %>%
  tolower()
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
dfm = dat2$text.sc %>% tokens() %>%
  dfm(tolower=TRUE)

counts=c()
for (j in 1:nrow(subs)){
  counts[j]=colSums(dfm)[colnames(dfm)==tolower(subs$x[j])]
}
subs$counts=counts
subs=subs[with(subs,order(counts,decreasing=T)),]

subs$x=paste0(" ", as.character(subs$x),"")
subs$h=paste0(" ", as.character(subs$h),"")

tmp = as.character(gsub(subs$x[1], subs$h[1],tolower(dat2$text.sc), fixed=T))
for (j in 2:nrow(subs)){
  tmp = gsub(subs$x[j], subs$h[j], tmp, fixed=T)
}

tmp = tolower(tmp)
tmp = repair_spelling( tmp,
                       c( "they're", "i'll" ),
                       c( "they are", "i will" ) )

dat2$text.sc=tmp


words = tribble( ~from, ~to,
  "airplain","airplane",
  "airplanee","airplane",
  "airplanees","airplanes",
  "beause","because",
  "oceann","ocean",
  "airplanee","airplane",
  "did't","did not",
  "wouldd","would",
  "wantd","wanted",
  "coildn't","could not",
  "coild","could",
  "probems","problems",
  "probem","problem",
  "studf","stuff",
  "machinee","machine",
  "anotherr","another"
)
dat2$text.sc = repair_spelling(dat2$text.sc, words)



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


#### Save processed data to intermediate files ####

write.csv(text,
          file=reads_file_path("Generated Data/text_g1g2_consented.csv"),
          row.names=F)

save(meta, text, rater.scores,
     file=reads_file_path("Generated Data/meta.RData") )


# Write cleaned essays to text files for analysis via TAACO
dnames = paste0("s", text$s_id, "_grade", text$grade, "_", text$subject, ".txt")

tada::prep_external(text$text.sc,
              dir=reads_file_path("Generated Data/text_files/" ),
              docnames=dnames)
