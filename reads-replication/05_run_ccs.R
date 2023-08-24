##
## Simple CCS analysis of the essays within each grade and subject
##

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
options(stringsAsFactors = FALSE)

library( tidyverse )
library( textreg )
library( tm )
source( "Scripts/cluster_threshold_C.R" )


#### Load the data #####

load("Generated Data/meta.RData")

text$sch_id = meta$sch_id
text$sch_gr_block = paste0( meta$sch_id, "-", meta$grade )


table( paste0( text$subject, "-", text$grade  ), Tx=text$more )


# Number essays in each of our 4 groups
table( text$grade, text$subject )

text$more_pm = 2 * text$more - 1
more_pm  = text$more_pm


# Make corpus object

clean.txt = function(x){
  out = stripWhitespace(removeNumbers(removePunctuation(tolower(x))))
  out
}

corpus = tm::VCorpus( VectorSource( clean.txt(text$text.sc ) ))

#corpus = clean.text( corpus )
corpus

raw_corpus = tm::VCorpus( VectorSource(text$text ) )
raw_corpus = tm::tm_map(raw_corpus, content_transformer(tolower))


###### Function to fit a collection of standard models to look at ######


scat = function( str, ... ) {
  cat( sprintf( str, ... ) )
}




make.result.table = function( sc, more_sc, cluster_id ) {
  require(textreg)

  C = cluster.threshold.C( sc, more_sc, cluster_id=cluster_id, R = 10)
  C
  quantile(C,0.80)
  scat( "L2 C: %.2f / %.2f\n", C[[1]], quantile(C,0.80) )
  C_L2 = ceiling( quantile(C,0.80) )
  res1 = textreg( corpus = sc,
                  more_sc,
                  C = C_L2,
                  verbosity = 0)
  res1

  res2 = textreg( corpus = sc,
                  more_sc,
                  C = C_L2, gap=1,
                  verbosity = 0)
  res2

  res3 = textreg( corpus = sc,
                  more_sc,
                  C = C_L2, binary.features = TRUE,
                  verbosity = 0)
  res3


  C = cluster.threshold.C( sc, more_sc, Lq = 3, cluster=cluster_id, R=10 )
  C
  C_L3 = ceiling( median( C ) )
  quantile(C,0.80)
  scat( "L3 C: %.2f / %.2f\n", C[[1]], quantile(C,0.80) )

  res4 = textreg( corpus = sc,
                  more_sc,
                  C = C_L3,
                  Lq = 3,
                  verbosity = 0)
  res4



  C = cluster.threshold.C( sc, more_sc, Lq = 1.5, cluster=cluster_id, R=10 )
  C
  quantile(C,0.80)
  C_L1.5 = ceiling( quantile(C,0.80) )
  scat( "L1.5 C: %.2f / %.2f\n", C[[1]], quantile(C,0.80) )

  res5 = textreg( corpus = sc,
                  more_sc,
                  C = C_L1.5,
                  Lq = 1.5,
                  verbosity = 0)
  res5

  results = list(  res1, res2,  res3, res4, res5 )
  for ( i in 1:length(results) ) {
    results[[i]]$model$ngram = gsub( " \\*$", "", results[[i]]$model$ngram )
  }

  tbl = make.list.table( results,
                         model.names = paste( c("L2","L2 (gap)","L2 (bin)","L3","L1.5"),
                                              round( c(C_L2, C_L2,C_L2,C_L3,C_L1.5),digits=1),
                                              sep="-" ),
                         method = "rank" )

  tbl
}


results.tab = function(result, corp.sub, Z){
  result$n.mods = sapply(1:nrow(result),function(x) sum(!is.na(result[x,c(2:6)])))
  tmp = subset(result,select=c(phrase, n.mods, num.reports, num.tag))
  tmp$count.neg = tmp$num.reports-tmp$num.tag
  names(tmp)=c("phrase","n.mods","docs.total", "docs1","docs0")
  tmp2 = tmp %>% mutate(prop.docs1= docs1/sum(Z),
                        prop.docs0 = docs0/sum(1-Z),
                        prop.diff = prop.docs1-prop.docs0)


  phrases = unique(tmp2$phrase)
  m = make.phrase.matrix(phrases, corp.sub)
  tmp2$phrase.tot = colSums(m)
  tmp2$tot1 = colSums(m[Z==1,])
  tmp2$tot0 = colSums(m[Z==0,])

  out = select(tmp2, phrase, n.mods, tot1, tot0,prop.diff)
  out$docs1 = paste0(tmp2$docs1, " (",round(tmp2$prop.docs1,2)*100, "%)")
  out$docs0 = paste0(tmp2$docs0, " (",round(tmp2$prop.docs0,2)*100, "%)")
  out$diff.val=tmp2$prop.diff
  out$diff = paste0(round(tmp2$prop.diff,2)*100,"%")
  out$diff[tmp2$prop.diff>0]=paste0("+",out$diff[tmp2$prop.diff>0])
  #out$docs.diff = paste0(round(tmp2$prop.diff*100,1),"%")
  out = out[with(out,order(desc(n.mods),desc(prop.diff))),]
  out = select(out, phrase, n.mods, tot1, tot0,docs1, docs0, diff, diff.val)
  rownames(out)=NULL
  out
}


# Initial pass: find threshold regularization
ind1 = which(text$subject=="science" & text$grade==1)
ind2 = which(text$subject=="science" & text$grade==2)


# Grade 1 science
r_Sci_g1 = make.result.table( corpus[ind1], more_pm[ind1], cluster_id = text$sch_id[ind1] )
r_Sci_g1

sample.fragments("my opinion", more_pm[ind1], corp = corpus[ind1])

out_sci_g1= results.tab(r_Sci_g1, corp.sub=corpus[ind1],
                        Z=text$more[ind1])

# Grade 2
r_Sci_g2 = make.result.table( corpus[ind2], more_pm[ind2], cluster_id = text$sch_id[ind2] )
r_Sci_g2

out_sci_g2= results.tab(r_Sci_g2, corp.sub=corpus[ind2],
                        Z=text$more[ind2])

out_sci = data.frame(subject="science", grade=c(rep(1,nrow(out_sci_g1)), rep(2,nrow(out_sci_g2))),
                     rbind(out_sci_g1, out_sci_g2))

## Social science
ind3 = which(text$subject!="science" & text$grade==1)
ind4 = which(text$subject!="science" & text$grade==2)

# Grade 1
r_Soc_g1 = make.result.table( corpus[ind3], more_pm[ind3], cluster_id = text$sch_id[ind3] )
r_Soc_g1

out_soc_g1= results.tab(r_Soc_g1, corp.sub=corpus[ind3],
                        Z=text$more[ind3])

# Grade 2
r_Soc_g2 = make.result.table( corpus[ind4], more_pm[ind4], cluster_id = text$sch_id[ind4] )
r_Soc_g2

out_soc_g2= results.tab(r_Soc_g2, corp.sub=corpus[ind4],
                        Z=text$more[ind4])

out_soc = data.frame(subject="social", grade=c(rep(1,nrow(out_soc_g1)), rep(2,nrow(out_soc_g2))),
                     rbind(out_soc_g1, out_soc_g2))


ccs_out = rbind(out_sci, out_soc)
ccs_out$grade=as.factor(ccs_out$grade)
ccs_out$subject=as.factor(ccs_out$subject)

save(r_Sci_g1, r_Sci_g2,
     r_Soc_g1, r_Soc_g2,
     ccs_out,
     file="Results/CCS_results.RData" )

