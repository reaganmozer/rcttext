#
# Estimate impacts on top-line results (human-coded essay quality) and across the machine-generated measures of the text.
#

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
options(stringsAsFactors = FALSE)

library( tidyverse )
library(dplyr)
library(lmerTest)
library(lmtest)

load("generated_data/meta.RData")

# Characteristics of the sample
length(unique(meta$s_id))
length(unique(meta$sch_id))

sum(is.na(meta$s_maprit_1819w))

meta$grade = as.factor(meta$grade)
apply(meta,2,function(x)sum(is.na(x)))

# Impute pretest score.
#meta$maprit_imp = ifelse(is.na(meta$s_maprit_1819w),meta$mean.pretest, meta$s_maprit_1819w)

# Standardize pretest
meta$maprit_std = scale(meta$s_maprit_1819w)


stats = meta %>% group_by( subject, grade, more ) %>%
  summarise( mn = mean( score ),
             sd = sd( score ),
             n = n() )
stats
avg_stats = stats %>% group_by( subject ) %>%
    summarise( sd_bar = sqrt( weighted.mean( sd^2, w=n ) ) )
avg_stats

meta %>% group_by( subject ) %>%
  summarise( mn = mean( score ),
             sd = sd( score ) )


sci = meta[meta$subject=="science",]
soc = meta[meta$subject!="science",]
sci$score_std = sci$score/avg_stats$sd_bar[[1]]
soc$score_std = soc$score/avg_stats$sd_bar[[2]]


##### Estimate main impacts #####

Mod.Sci = lm( score_std ~ as.factor(sch_id) + grade +  maprit_std + more, data=sci)
vcov_clust = sandwich::vcovCL( Mod.Sci, sci$t_id )
vcov_clust
library( lmtest )
est.sci=coeftest( Mod.Sci, vcov. = vcov_clust )

Mod.SS = lm( score_std ~ as.factor(sch_id) + grade +maprit_std + more, data=soc)
vcov_clust2 = sandwich::vcovCL( Mod.SS, soc$t_id )
est.soc=coeftest( Mod.SS, vcov. = vcov_clust2 )


library(emmeans)
emmeans(Mod.Sci, ~more|grade)
emmeans(Mod.SS, ~more|grade)


texreg::screenreg(list(est.sci, est.soc),
                  custom.model.names=c("Science", "Social Studies"),
                  custom.coef.map = list("(Intercept)"=NA,
                                         "grade2"="Grade 2", "maprit_imp"="Pre-test score", "more"="MORE"))



# Sensitivity check: Clustering at school level (most conservative SE approach)
vcov_clust_sch = sandwich::vcovCL( Mod.Sci, sci$sch_id )
est.sci_sch=coeftest( Mod.Sci, vcov. = vcov_clust_sch )

vcov_clust2_sch = sandwich::vcovCL( Mod.SS, soc$sch_id )
est.soc_sch=coeftest( Mod.SS, vcov. = vcov_clust2_sch )

# Relative SEs: about the same
est.sci_sch[,2] / est.sci[,2]
est.soc_sch[,2] / est.soc[,2]


# Results basically identical.
texreg::screenreg(list(est.sci, est.sci_sch, est.soc, est.soc_sch),
                  custom.model.names=c("Science", "Sci (sch)", "Social Studies", "SS (sch)"),
                  custom.coef.map = list("(Intercept)"=NA,
                                         "grade2"="Grade 2", "maprit_imp"="Pre-test score", "more"="MORE"))



if (FALSE){
  # Multilevel model for science scores
  mod.sci = lmer(score_std ~ 1 + grade + #mean.pretest+
                   maprit_imp +
                   more + (1|sch_id) + (1|t_id),
                 data=sci)
  summary(mod.sci)
  mod.sci1 = lmer(score_std ~ 1 + grade + #mean.pretest+
                    maprit_imp+
                    more + grade*more+ (1|sch_id) + (1|t_id),
                  data=sci)

  lrtest(mod.sci, mod.sci1)

  # Multilevel model for social scores
  mod.soc = lmer(score_std ~ 1 + grade + #mean.pretest+
                   maprit_imp +
                   more + (1|sch_id) + (1|t_id),
                 data=soc)

  mod.soc1 = lmer(score_std ~ 1 + grade + #mean.pretest+
                    maprit_imp +
                    more + (grade*more)+ (1|sch_id) + (1|t_id),
                  data=soc )

  lrtest(mod.soc, mod.soc1)
}
save(Mod.Sci, Mod.SS, est.sci, est.soc, file="results/tx_models.RData")




##### Estimate treatment impacts on generated text features #####

load("generated_data/all.info.RData")
tmp = select(meta, s_id, grade, subject, more, maprit_std)
all.info = merge(tmp, all.info, by=c("s_id","grade", "subject", "more"))
library(caret)

# Dropping some features until TAACO and LIWC are redone
# dat = dplyr::select(all.info, sch_id,t_id,more, maprit_std,subject,
#                     grade, spellcheck, TTR:Sixltr, prep:WCperChar, lemma_ttr:function_ttr,
#                     basic_connectives:addition, reason_and_purpose:all_demonstratives, all_connective, pronoun_density)
dat = dplyr::select(all.info, sch_id,t_id,more, maprit_std,subject,
                    grade, spellcheck, lex_TTR:sent_Sixltr, WCperChar)

x = dat[,-c(1:6)]
names(x)[findLinearCombos(x)$remove]
dat = select(dat, -meanWordSyllables)
findCorrelation(cor(dat[,-c(1:6)]),names=T,exact=T,cutoff=0.9)

dat = select(dat, -Flesch, -ARI,
             -entropy, -lemma_ttr) # remove highly correlated features
findCorrelation(cor(dat[,-c(1:6)]),names=T,exact=T,cutoff=0.9)
x = dat[,-c(1:6)]

# find features with near zero variance
rm=nearZeroVar(x, uniqueCut = 2, freqCut=99/1, names=T)
sort(apply(dat[,names(dat)%in%rm], 2, function(x)length(unique(x))))

# remove features with near zero variance or high VOF
dat = dat %>% select( -filler, -SemiC, -sexual, -swear, -Parenth, -OtherP,
                      -Colon, -Quote, -nonflu, -Apostro, -Comma,
                      -Dash, -Period)


#get_diffs = function(x, Z){
#  Z = relevel(as.factor(Z),"1")
#  res = data.frame(var=names(x), diff=NA, stat=NA, p.raw=NA, LL=NA, UL=NA)
#  for (j in 1:ncol(x)){
#  tmp = t.test(x[,j]~Z)
#  res[j,-c(1)]=c(tmp$estimate[1]-tmp$estimate[2], tmp$statistic, tmp$p.value, tmp$conf.int[1], tmp$conf.int[2])
#  }
#  res$p.adj = p.adjust(res$p.raw, "fdr")
#  return(res)
#}

#' For each column of x, conduct an analysis of impact of MORE intervention on
#' feature represented by that column.
#'
#' Adjust all tests with FDR at end.
get_diffs = function(d, x){
  res = data.frame(var=names(x),
                   est=NA, SE=NA, stat=NA,
                   p.raw=NA, LL=NA, UL=NA )
  tmp = select(d, sch_id, t_id, more, grade, maprit_std)
  for (j in 1:ncol(x)){
    tmp$var=x[,j]
    tmp$more=as.factor(tmp$more)
    mod = lm(var ~ maprit_std + more, data=tmp)
    vc = sandwich::vcovCL(mod, tmp$sch_id)

    #mod = lm(var ~ as.factor(sch_id)+maprit_std + more*grade, data=tmp)
    #vc = sandwich::vcovCL(mod, tmp$t_id)

    est=coeftest( mod, vcov. = vc )
    CI = coefci(mod, vcov.=vc)
    res[j,2:5]=est[grep("more",rownames(est)),]
    res[j,6:7] = CI[grep("more",rownames(CI)),]
  }
  #res$p.adj=p.adjust(res$p.raw, "fdr")
  return(res)
}


out=plyr::ddply(dat, ~subject+grade, function(d) get_diffs(d, x=d[,-c(1:6)]))
add.vars = c("Analytic","Authentic","Clout","Tone",
             "WC","WPS","Sixltr","XXX",
             "TTR","Flesch.Kincaid")
out$planned=1*(out$var%in%add.vars)
out.pl = out[out$planned==1,]
out.un = out[out$planned==0,]
out.pl$p.adj= p.adjust(out.pl$p.raw,"fdr")
out.un$p.adj= p.adjust(out.un$p.raw,"fdr")
out = rbind(out.pl, out.un)

table(out$p.adj<=0.05, out$p.raw<=0.05)

sig.vars = unique(out$var[out$p.adj<=0.05])

all.vars = unique(c(sig.vars,add.vars))


diffs= all.info %>% group_by(subject, grade,more ) %>%
  dplyr::summarise_at(add.vars, mean)

diffs2 = all.info %>% group_by( subject, grade,more ) %>%
  dplyr::summarise_at(sort(all.vars[!all.vars%in%names(diffs)]), mean)

#print(diffs)
#print(diffs2)
dd = diffs %>% pivot_longer(cols = -c(subject, grade,more ) ) %>%
  pivot_wider( names_from = more, values_from = value,
               names_prefix = "Grp_")
dd2 = diffs2 %>% pivot_longer(cols = -c(subject, grade,more ) ) %>%
  pivot_wider( names_from = more, values_from = value,
               names_prefix = "Grp_")
dd = bind_rows( dd, dd2 )
dd =  dd %>%
  mutate( delta =  Grp_1 - Grp_0 ) %>%
  dplyr::rename( MORE="Grp_1",
                 Control="Grp_0")

dd = dd %>% pivot_wider(names_from=c(subject),
                        values_from=c(Control,MORE,delta)) %>%
  select(grade, name, ends_with("science"), ends_with("social"))



dd.out = arrange(dd, grade,name)
dd.out.raw = dd.out

out2 = out[out$var%in%all.vars,]
names(out2)[grep("var",names(out2))]="name"

d2 = all.info %>% group_by( subject, grade,more ) %>%
  dplyr::summarise_at(all.vars, .funs = c( mn = mean, sd = sd ) )

dd = d2 %>% pivot_longer(cols = -c(subject,grade, more ),
                         names_to = c("name", ".value"),
                         names_pattern = "(.*)_(.*)" ) %>%
  pivot_wider( names_from =more, values_from=c(mn,sd) )

dd = mutate( dd, delta = (mn_1 - mn_0) / ((sd_1+sd_0)/2) )
dd = dd[dd$name%in%all.vars,]


dd = dd %>%
  dplyr::select(  subject, grade,name,mn_0, sd_0,  mn_1, sd_1, delta ) %>%
  dplyr::rename( MORE="mn_1",
                 Control="mn_0",
                 MORE_sd = sd_1,
                 Co_sd = sd_0 )

dd = merge(dd, out2, by=c("grade","subject","name"))
dd$LL.std = dd$LL/((dd$MORE_sd+dd$Co_sd)/2)
dd$UL.std = dd$UL/((dd$MORE_sd+dd$Co_sd)/2)


dd.out = select(dd, grade, subject, name, Control, MORE,
                est, LL, UL,
                delta, LL.std, UL.std,
                p.adj, p.raw)
dd.out$pretty.CI.raw = paste0("(", sprintf("%.2f",round(dd$LL,2)), ", ",
                              sprintf("%.2f",round(dd$UL,2)), ")")
dd.out$pretty.CI.std = paste0("(", sprintf("%.2f",round(dd$LL.std,2)), ", ",
                              sprintf("%.2f",round(dd$UL.std,2)), ")")

dd = select(dd, grade, subject, name, delta, LL.std, UL.std, p.raw, p.adj)


#dd = pivot_wider( dd, names_from=subject,
#                 values_from = c(delta, LL.std, UL.std, p.raw, p.adj, pretty.CI) )
save(dd.out, dd, file="results/LIWC_diffs_results.RData")

