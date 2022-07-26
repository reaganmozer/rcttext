setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
options(stringsAsFactors = F)

library(ggplot2)
library(plotrix)
library(tidyverse)
library(tada)


## Theme for all ggplot objects
library(ggthemes)
my_theme = theme_bw() + 
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        plot.title= element_text(hjust=0.5,size=13), 
        axis.title = element_text(size=13), 
        axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=12), 
        axis.line = element_line(colour = "black"),
        strip.text.x = element_text(size = 12), 
        strip.text.y=element_text(size=11))

theme_set( my_theme )


# Load results for all figures
load("Generated Data/all.info.RData")
all.info$grade=as.factor(all.info$grade)
levels(all.info$grade)=c("Grade 1", "Grade 2")
all.info$subject=as.factor(all.info$subject)
levels(all.info$subject)=c("Science", "Social Studies")

load("Results/LIWC_diffs_results.RData")
dd.plot = select(dd, grade, subject, name, delta, LL.std, UL.std, p.adj)
names(dd.plot)[-c(1:3)]=c("est","LL","UL","p.adj")

dd.plot$type = ifelse(dd.plot$name%in%c("WC","WPS","TTR","R", "Flesch", "XXX","Sixltr",'Flesch.Kincaid','Flesch',
                                        "Analytic","Authentic","Clout","Tone","ARI"), "Planned", "Unplanned")


dd.sci = dd.plot %>% filter(subject=="science")%>% select(-subject)
dd.soc = dd.plot %>% filter(subject!="science")%>% select(-subject)


#### MAP THE FUNCTION BELOW TO PLOT_TEXTFX IN TADA!
## Figure 1: Planned comparisons of LIWC features
par(mfrow=c(2,2),mar=c(4.1,8.9,2.5,1.1),mgp=c(2.5,0.5,0))
pdf(file="Figures/LIWC_diffs_planned.pdf", width=9, height=8)
par(mfrow=c(2,2),mar=c(4.1,8.9,2.5,1.1),mgp=c(2.5,0.5,0))
LIWC.plot.all(dd.sci[dd.sci$grade==1 & dd.sci$type=="Planned",], main="Grade 1 Science",xlim=c(-0.8,0.8))
LIWC.plot.all(dd.soc[dd.soc$grade==1 & dd.sci$type=="Planned",], main="Grade 1 Social Studies",xlim=c(-0.8,0.8))
LIWC.plot.all(dd.sci[dd.sci$grade==2 & dd.sci$type=="Planned",], main="Grade 2 Science",xlim=c(-0.8,0.8))
LIWC.plot.all(dd.soc[dd.soc$grade==2 & dd.sci$type=="Planned",], main="Grade 2 Social Studies",xlim=c(-0.8,0.8))
dev.off()
par(mfrow=c(1,1))

## Figure 2: Unplanned comparisons of LIWC features
par(mfrow=c(2,2),mar=c(4.1,6.9,2.5,1.1),mgp=c(2.5,0.5,0))
pdf(file="Figures/LIWC_diffs_unplanned.pdf", width=9, height=8)
par(mfrow=c(2,2),mar=c(4.1,6.9,2.5,1.1),mgp=c(2.5,0.5,0))
LIWC.plot.all(dd.sci[dd.sci$grade==1 & dd.sci$type!="Planned",], main="Grade 1 Science",xlim=c(-0.8,0.8))
LIWC.plot.all(dd.soc[dd.soc$grade==1 & dd.sci$type!="Planned",], main="Grade 1 Social Studies",xlim=c(-0.8,0.8))
LIWC.plot.all(dd.sci[dd.sci$grade==2 & dd.sci$type!="Planned",], main="Grade 2 Science",xlim=c(-0.8,0.8))
LIWC.plot.all(dd.soc[dd.soc$grade==2 & dd.sci$type!="Planned",], main="Grade 2 Social Studies",xlim=c(-0.8,0.8))
dev.off()
par(mfrow=c(1,1))



# Figure 3: CCS plots
load("Generated Data/meta.RData")
sums = meta %>% group_by(grade, subject) %>% summarise(ntreat = sum(more),
                                                       ncontrol=sum(1-more))

load("Results/CCS_results.RData")

ccs_out = merge(ccs_out, sums, by=c("subject","grade"))


g1.sci=ccs_out[ccs_out$subject=="science" & ccs_out$grade==1,-c(1:2)]
g2.sci=ccs_out[ccs_out$subject=="science" & ccs_out$grade==2,-c(1:2)]

g1.soc=ccs_out[ccs_out$subject!="science" & ccs_out$grade==1,-c(1:2)]
g2.soc=ccs_out[ccs_out$subject!="science" & ccs_out$grade==2,-c(1:2)]


pdf(file="Figures/CCS_diffs_science.pdf", width=9, height=5)
par(mfrow=c(1,2))
plot_ccs(g1.sci, main="G1 Science")
plot_ccs(g2.sci, main="G2 Science")
dev.off()


pdf(file="Figures/CCS_diffs_social.pdf", width=9, height=5)
par(mfrow=c(1,2))
plot_ccs(g1.soc, main="G1 Social", xadj=c(-0.075,0.1))
plot_ccs(g2.soc, main="G2 Social", xadj=c(-0.075,0.1))
dev.off()
par(mfrow=c(1,1))



## Figure 4: Distribution of similarity scores
levels(all.info$subject)=c("Science","Social Studies")
levels(all.info$grade)=c("Grade 1", "Grade 2")
ggplot(all.info, aes(x=1-tdm.raw.cosine, col=as.factor(more),fill=as.factor(more)))+
  geom_hline(yintercept=0, col="black")+
  facet_grid(grade~subject,scales="free_y")+
  geom_density(size=1.05, position="identity", alpha=0.2)+
  labs(x="Cosine similarity",y="Density")+
  scale_colour_discrete(name = "", labels = c("Control","Treatment"),aesthetics=c("colour","fill"))+
  scale_x_continuous(limits=c(-0.1,1.0))+
  theme(axis.text.x = element_text(size=11), legend.text = element_text(size=11),
        legend.position="bottom")
ggsave("Figures/cosine_sim.pdf", width=8, height=6)





