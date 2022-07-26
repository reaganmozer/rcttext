setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
options(stringsAsFactors = FALSE)


#library(knitr)
library(kableExtra)
library(dplyr)
library(tidyverse)
library(xtable)



#### Descriptives table ####

load("Generated Data/meta.RData")
meta$grade = as.factor(meta$grade)

meta.sum = meta %>% group_by(subject, grade, more) %>%
  summarise(n.students = length(unique(s_id)),
            n.teach = length(unique(t_id)),
            n.sch = length(unique(sch_id)),
            mean.pretest = mean(s_maprit_1819w))

meta.sum2 = meta.sum %>% group_by(subject, grade) %>% 
  pivot_longer(cols=c(n.sch, n.teach, n.students, mean.pretest)) %>% 
  pivot_wider(names_from=more, names_prefix="Grp_", values_from=value) %>%
  pivot_wider(names_from=subject, values_from=c(Grp_0,Grp_1)) %>%
  select(grade, name, ends_with("_science"), ends_with("_social"))

meta.sum2 = as.data.frame(meta.sum2)
meta.sum2[meta.sum2$name=="mean.pretest",-c(1:2)]=t(apply(meta.sum2[meta.sum2$name=="mean.pretest",-c(1:2)],1,
                                                          function(x)as.character(sprintf("%.1f",x))))

descriptives = knitr::kable(meta.sum2, format="latex", linesep="", align=c("lccccc"),
             col.names=c("Grade", "","Control","Treatment","Control", "Treatment"),
             caption="Sample sizes and average pretest scores for each of the treatment and control groups within each grade and subject.",
             label="descriptives"
             ) %>%
  kableExtra::add_header_above(c("","","Science"=2, "Social Studies"=2))


sink("Tables/descriptives.tex")
descriptives
sink()


#### Treatment effect estimates from MLM ####


# This is what we have been using for Table 1
load("Results/tx_models.RData")
texreg::texreg(list(est.sci, est.soc), file="Tables/impact_est.tex",
               label="tab:impact_est",
               caption="Estimated effects (in effect size units) of grade level, pretest scores (MAP/RIT), 
               and receipt of MORE intervention compared to typical instruction on average (human-coded) 
               writing quality scores in science and social studies.",
               caption.above=T,
               custom.model.names=c("Science", "Social Studies"),
               custom.coef.map = list("(Intercept)"=NA, "grade2"="Grade 2", 
                                      "maprit_imp"="Pretest Score", "more"="MORE"))


## Table 1: Human v. machine similarity scores
load("Results/overall_impact_ests.RData")

texreg::screenreg(out.simil,
                  custom.model.names=c("Science", "Social Studies"),
                  custom.coef.map = list("grade1"="Grade 1 Baseline",
                                         "grade2"="Grade 2 Baseline", 
                                         "maprit_imp_std"="Pretest Score", 
                                         "grade1:more"="Grade 1",
                                         "grade2:more" = "Grade 2"))

texreg::screenreg(out.simil,
               custom.model.names=c("Science", "Social Studies"),
               custom.coef.map = list(
                 "grade1:more"="Grade 1",
                 "grade2:more" = "Grade 2"))


texreg::texreg(out.simil, file="Tables/simscores.tex",
               label="tab:similarity",
               caption="Estimated effects (in effect size units) of the MORE intervention
               compared to typical instruction on average descriptive similarity 
               between student-generated essays and gold-standard reference texts, controlling
               for pre-test (MAP/RIT) scores.",
               caption.above=T,
               custom.model.names=c("Science", "Social Studies"),
               custom.coef.map = list(
                                      "grade1:more"="Grade 1",
                                      "grade2:more" = "Grade 2"))

               #custom.coef.map = list("(Intercept)"=NA, "grade2"="Grade 2", 
              #                        "maprit_imp_std"="Pretest Score", "more"="MORE"))




### Table 2: Human versus machine predicted quality scores
load("Results/overall_impact_ests.RData")
all.out = list(out.Yobs[[1]], out.ML[[1]], out.Yobs[[2]],out.ML[[2]])

texreg::screenreg(all.out, 
                  custom.header=list("Science"=1:2, "Social Studies"=3:4),
                  custom.model.names=rep(c("Human-coded", "ML predicted"),2) )

texreg::screenreg(all.out, 
                  custom.header=list("Science"=1:2, "Social Studies"=3:4),
                  custom.model.names=rep(c("Human-coded", "ML predicted"),2),
                  custom.coef.map = list( "grade1"="Grade 1 Baseline",
                                         "grade2"="Grade 2 Baseline", 
                                         "maprit_imp_std"="Pretest Score", 
                                         "grade1:more"="MORE (Grade 1)",
                                         "grade2:more" = "MORE (Grade 2)"))


texreg::screenreg(all.out, 
                  custom.header=list("Science"=1:2, "Social Studies"=3:4),
                  custom.model.names=rep(c("Human-coded", "ML predicted"),2),
                  custom.coef.map = list("(Intercept)"=NA, "grade2"="Grade 2", 
                                         "maprit_imp_std"="Pretest Score", "more"="MORE",
                                         "grade2:more" = "MORE x Grade 2"))

texreg::texreg(all.out, file="Tables/compare_quality.tex",
               label="tab:MLquality",
               caption="Estimated effects (in effect size units) of grade level, pretest scores (MAP/RIT), 
               and receipt of MORE intervention compared to typical instruction on average machine learning (ML) predicted quality scores (right)
               compared to estimated estimated effects on average human-coded quality scores (left)
               for each grade level and subject.",
               caption.above=T,
               custom.header=list("Science"=1:2, "Social Studies"=3:4),
               custom.model.names=rep(c("Human-coded", "ML predicted"),2),
               custom.coef.map = list("grade1"="Grade 1 Baseline",
                                      "grade2"="Grade 2 Baseline", 
                                      "maprit_imp_std"="Pretest Score", 
                                      "grade1:more"="MORE (Grade 1)",
                                      "grade2:more" = "MORE (Grade 2)"))


## Update to human are left-most, then similarity, then ML right-most)
all.out2 = list(out.Yobs[[1]], out.Yobs[[2]], 
                out.simil[[1]], out.simil[[2]], 
                out.ML[[1]], out.ML[[2]])

texreg::screenreg(all.out2, 
               caption.above=T,
               custom.header=list("Human-coded"=1:2, "Machine similarity"=3:4, "ML predicted"=5:6),
               custom.model.names=rep(c("Science", "Social studies"),3),
               custom.coef.map = list("grade1"="Grade 1 Baseline",
                                      "grade2"="Grade 2 Baseline", 
                                      "maprit_imp_std"="Pretest Score", 
                                      "grade1:more"="MORE (Grade 1)",
                                      "grade2:more" = "MORE (Grade 2)"))

texreg::texreg(all.out2, file="Tables/compare_quality2.tex",
               label="tab:MLquality2",
               caption="Estimated effects (in effect size units) of grade level, pretest scores (MAP/RIT), 
               and receipt of MORE intervention compared to typical instruction on  average human-coded quality scores (left),
               average descriptive similarity scores (center) and average machine learning (ML) predicted quality scores (right)
               for each grade level and subject.",
               caption.above=T,
               custom.header=list("Human-coded"=1:2, "Machine similarity"=3:4, "ML predicted"=5:6),
               custom.model.names=rep(c("Science", "Social studies"),3),
               custom.coef.map = list("grade1"="Grade 1 Baseline",
                                      "grade2"="Grade 2 Baseline", 
                                      "maprit_imp_std"="Pretest Score", 
                                      "grade1:more"="MORE (Grade 1)",
                                      "grade2:more" = "MORE (Grade 2)"))

#custom.coef.map = list("(Intercept)"=NA, "grade2"="Grade 2", 
#                        "maprit_imp_std"="Pretest Score", "more"="MORE"))






#### Differences between use of taught and untaught concept words  ####
load("Results/concept_tabs.RData")
sums.out = sums.out %>% arrange(grade, subject) %>%
  mutate(diff=prop.essays_1-prop.essays_0)
levels(sums.out$subject)

stars = rep("",nrow(sums.out))

stars[sums.out$p.adj<=0.05]="*"
stars[sums.out$p.adj<=0.01]="**"
stars[sums.out$p.adj<=0.001]="***"
sout1 = sums.out %>% mutate(freq.diff=total.freq_1-total.freq_0,
                            prop.essays_0=paste0(num.essays_0, " (",round(100*prop.essays_0,1),"%)"),
                            prop.essays_1=paste0(num.essays_1," (",round(100*prop.essays_1,1),"%)"),
                            diff=paste0(num.essays_1-num.essays_0," (",round(100*diff,1),"%)")) 

sout1 = select(sout1, grade, subject, Taught,total.freq_1,total.freq_0, freq.diff,
               prop.essays_1, prop.essays_0, diff)
sout1$subject=factor(sout1$subject, labels=c("Science","Social"))
sout1$Taught=factor(sout1$Taught, labels=c("Taught","Untaught"))
sout1 = sout1[sout1$Taught!="Total",]
sout1[,4:6]=apply(sout1[,4:6],2,as.integer)
sout1$p.adj=stars
names(sout1)=c("","","",
               "MORE","Control","Diff.",
               "MORE","Control","Diff.", 
               "")

tab.cwords = xtable(sout1, auto=F, 
caption="Use of taught and untaught concept words following MORE instruction versus typical instruction, grouped by grade-level and subject. 
For each set of terms, columns show cumulative frequency (total number occurrences) and prevalence (number of essays with at least one occurrence) rates 
across essays in each treatment group.", label="tab:cwords",
                    align="llllcccrrrl")
print(tab.cwords,type="latex",align="lllcccrrrl",caption.placement="top",include.rownames=F,
      add.to.row=list(pos=list(-1),command="\\hline\n \\multirow{2}{*}{Grade} & \\multirow{2}{*}{Subject} & \\multirow{2}{*}{Word type} & \\multicolumn{3}{c}{Frequency} 
                      & \\multicolumn{3}{c}{Prevalence}\\\\ \\cline{4-6}\\cline{7-9}\n"),
      hline.after=c(0,nrow(sout1)),
      file="Tables/cwords.tex")



####  CCS results by grade and subject ####

load("Results/CCS_results.RData")

write_tab = function(out1, fname, caption, threshold = 100){
  #out1 = out1 %>% filter(abs(diff.val)>=0.1) %>% select(-diff.val)
  out1 = out1 %>% select(-diff.val)
  
  rems = filter( out1, tot1 + tot0  < threshold )
  out1 = filter( out1, tot1 + tot0 >= threshold )
  phrases = paste( "`", rems$phrase, "'", sep="", collapse = ", " )
  phrases
  
  names(out1)=c("phrase", "n.mods", "MORE", "Control", "MORE", "Control","delta")
  rownames(out1)=NULL
  
  caption = paste0( caption, ". Other significant, but rare occuring phrases, are ", phrases, "." )
  
  tab_out= knitr::kable(out1,format="latex",linesep="",caption=caption, label=fname) %>%
    add_header_above(c(" ", " ", "Frequency"=2, "Prevalence"=3)) %>%
    kable_styling(latex_options = c("repeat_header"))
  
  sink(file=sprintf(paste0("Tables/",fname, ".tex")))
  print(tab_out)
  sink()
}

write_tab(out1=ccs_out[ccs_out$subject=="science" & ccs_out$grade==1,-c(1:2)],
          fname="ccs_sci_g1",caption="CCS results grade 1 science")
write_tab(out1=ccs_out[ccs_out$subject=="science" & ccs_out$grade==2,-c(1:2)],
          fname="ccs_sci_g2",caption="CCS results grade 2 science")

write_tab(out1=ccs_out[ccs_out$subject!="science" & ccs_out$grade==1,-c(1:2)],
          fname="ccs_soc_g1",caption="CCS results grade 1 social studies")
write_tab(out1=ccs_out[ccs_out$subject!="science" & ccs_out$grade==2,-c(1:2)],
          fname="ccs_soc_g2",caption="CCS results grade 2 social studies")




