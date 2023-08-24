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
library(tada)

load("Generated Data/meta.RData")

# Create a data frame containing the concept words identified in the MORE study
create_cwords <- function() {
  grade = c(rep(1,23), rep(2,24))
  subject = c(rep("science", 11), rep("social",12), rep("science", 12), rep("social", 12))
  concept_word = c('survive', 'species', 'behavior', 'advantage', 'adaptation', 'habitat', 'physical_feature', 'potential', 'unique', 'camouflage', 'diversity',
                   'expedition', 'discovery', 'obstacle', 'indigenous', 'explorer', 'community', 'persistent', 'ancestor', 'navigation', 'settle', 'celebrate', 'route',
                   'extinct', 'fossil', 'brutal', 'evidence', 'theory', 'hunter', 'paleontologist', 'carnivore', 'hypothesis', 'organism', 'trait', 'reptile',
                   'inventor', 'experiment', 'prototype', 'discrimination', 'laboratory', 'engineer', 'ingenious', 'hire', 'approve', 'establish', 'manufacture', 'foundation')
  taught = c(rep("taught",7), rep("untaught",4),
             rep("taught",7), rep("untaught", 5),
             rep("taught", 7), rep("untaught", 5),
             rep("taught", 7), rep("untaught", 5))

  # Creating the data frame
  cwords = data.frame(grade, subject, concept_word, taught)

  return(cwords)
}


textfx_terms = function(x, Z, terms, ...){

  # Determine maximum number of ngrams to search based on the input terms
  ngrams.terms = stringi::stri_count_words(terms)
  max.ngrams = 1:max(ngrams.terms)

  # Convert ngrams to quanteda structure with underscores
  terms = gsub(" ", "_", terms, fixed=T)

  dfm = quanteda::dfm(quanteda::tokens_ngrams(quanteda::tokens(x,...),n=max.ngrams))
  dfm.terms = as.matrix(quanteda::dfm_match(dfm, terms))

  out = data.frame(Z=Z, dfm.terms, tot=rowSums(dfm.terms))
  out = out %>% group_by(Z) %>% summarise(n=n(), termfreq=sum(tot), docfreq=sum(tot>0), prop.docs=docfreq/n) %>% arrange(desc(Z))

  # Hypothesis test for difference in proportions between groups
  test = prop.test(x=out$docfreq, n=out$n)


  out1 = out %>% pivot_wider(names_from=Z, values_from=!Z) %>%
    mutate(diff = test$estimate[1]-test$estimate[2],
           p.value = test$p.value,
           LL = test$conf.int[1],
           UL = test$conf.int[2])

  return(out1)

}



cwords = create_cwords()

# Analyze separately for taught and untaught terms
tmp=cwords %>% group_by(grade,subject) %>% do(terms.taught=paste0(.$concept_word[.$taught=="taught"]),
                                              terms.untaught = paste0(.$concept_word[.$taught=="untaught"]))

text1 = merge(text, tmp, by=c("grade", "subject"))


res.taught = text1 %>% group_by(grade, subject) %>% do(tada::textfx_terms(.$text.sc, .$more, unique(unlist(.$terms.taught)))) %>% mutate(type="taught")
res.untaught = text1 %>% group_by(grade, subject) %>% do(tada::textfx_terms(.$text.sc, .$more, unique(unlist(.$terms.untaught)))) %>% mutate(type="untaught")


# combine results across taught and untaught terms and adjust for multiple comparisons
res = rbind(res.taught, res.untaught)
res$p.adj = p.adjust(res$p.value, "fdr")

res = res %>% select(grade, subject, type, everything())


save(res,file="Results/concept-tabs.RData")










