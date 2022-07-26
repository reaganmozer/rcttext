
clean_txt = function(x){
  require(tm)
  tmp = gsub(".", " . ", x, fixed=TRUE)
  tmp = gsub(",", " . ", tmp, fixed=TRUE)
  tmp = gsub("-", "  ", tmp, fixed=TRUE)
  tmp = stripWhitespace(removePunctuation(tolower(tmp)))
  return(tmp)
}

get_meta = function(grade=NULL,subject=NULL){
  load("Generated Data/meta.RData")
  if (!is.null(grade)){
    meta = meta[meta$grade==grade,]}
  if (!is.null(subject)){
    meta = meta[meta$subject==subject,]
  }
  return(meta)
}

get_essay_text = function(grade=NULL, subject=NULL){
  load("Generated Data/meta.RData")
  if (!is.null(grade)){
    text = text[text$grade==grade,]}
  if (!is.null(subject)){
    text = text[text$subject==subject,]}
  return(text)
}




get_impact_est = function(meta, yhat, interact=FALSE){
  # Characteristics of the sample
  meta$grade = as.factor(meta$grade)
  # Impute pretest score.
  meta$maprit_imp = ifelse(is.na(meta$s_maprit_1819w),meta$mean.pretest, meta$s_maprit_1819w)
  # Standardize pretest
  meta$maprit_imp_std = as.vector(scale(meta$maprit_imp))
  
  
  meta$score = yhat
  stats = meta %>% group_by( subject, grade, more ) %>% 
    summarise( mn = mean( score ),
               sd = sd( score ), 
               n = n() )
  
  avg_stats = stats %>% group_by( subject ) %>%
    summarise( sd_bar = sqrt( weighted.mean( sd^2, w=n ) ) )
  
  
  sci = meta[meta$subject=="science",]
  soc = meta[meta$subject!="science",]
  sci$score_std = sci$score/avg_stats$sd_bar[[1]]
  soc$score_std = soc$score/avg_stats$sd_bar[[2]]
  
  
  ##### Estimate main impacts #####
  
  out = c()
  if ( interact ) {
    sci$grade = as.factor( sci$grade )
    Mod.Sci = lm( score_std ~ 0 + grade + maprit_imp_std + more:grade, 
                  data=sci)
    vcov_clust = sandwich::vcovCL( Mod.Sci, sci$sch_id )
    est.sci=coeftest( Mod.Sci, vcov. = vcov_clust )
    
    soc$grade = as.factor( soc$grade )
    Mod.SS = lm( score_std ~ 0 + grade + maprit_imp_std + more:grade, 
                 data=soc)
    vcov_clust2 = sandwich::vcovCL( Mod.SS, soc$sch_id )
    est.soc=coeftest( Mod.SS, vcov. = vcov_clust2 )
    out=list(est.sci, est.soc)
    
    cat(texreg::screenreg(out, 
                          custom.model.names=c("Science", "Social Studies" ) ) )
                          # custom.coef.map = list("(Intercept)"=NA,
                          #                        "grade2"="Grade 2", 
                          #                        "maprit_imp"="Pre-test score",
                          #                        "more"="MORE",
                          #                        "grade2:more"="MORE x Grade 2")))
    
  } else {
    Mod.Sci = lm( score_std ~ as.factor(sch_id) + grade +  maprit_imp_std + more, data=sci)
    vcov_clust = sandwich::vcovCL( Mod.Sci, sci$t_id )
    est.sci=coeftest( Mod.Sci, vcov. = vcov_clust )
    
    Mod.SS = lm( score_std ~ as.factor(sch_id) + grade + maprit_imp_std + more, data=soc)
    vcov_clust2 = sandwich::vcovCL( Mod.SS, soc$t_id )
    est.soc=coeftest( Mod.SS, vcov. = vcov_clust2 )
    out=list(est.sci, est.soc)
    cat(texreg::screenreg(out, 
                          custom.model.names=c("Science", "Social Studies"),
                          custom.coef.map = list("(Intercept)"=NA,
                                                 "grade2"="Grade 2", 
                                                 "maprit_imp"="Pre-test score",
                                                 "more"="MORE")))
    
  }
  
  return(out)
}



