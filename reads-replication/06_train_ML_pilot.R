## Train an ML model to predict human-coded quality scores for the pilot data (G1 science essays from a different evaluation)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
options(stringsAsFactors = F)

options(java.parameters = "-Xmx1g")
library(bartMachine)
require(dplyr)
require(doParallel)
require(caret)
require(caretEnsemble)


train_ensemble = function( dat, n.tune=3, preProc=NULL, bounds=NULL) {



  train = dplyr::select(dat, Yobs, dplyr::everything(), -Z)


  doParallel::registerDoParallel(detectCores()-1)
  foreach::getDoParWorkers()


  ind = caret::createResample(train$Yobs, times=5)
  control = caret::trainControl(method="cv", number=5, index=ind,
                                 savePredictions="final",allowParallel=T,
                                 predictionBounds = bounds)

  # Fit our "ML" models
  coded.X = as.matrix(subset(train,select=-c(Yobs)))


  methods = c("cforest","foba","cubist","glmnet",
              "knn", "bstTree", "pcr",
              "rf", "RRFglobal", "rpart1SE",
              "svmPoly","svmRadial",
              "treebag")


  grid.gbm = expand.grid(n.trees=c(50,100), interaction.depth=c(1,2),
                         shrinkage=0.1, n.minobsinnode=10)
  grid.bb = expand.grid(mstop=50, maxdepth=c(1,2,3))
  methods.tl = list(#blackboost = caretModelSpec(method="blackboost", tuneGrid=grid.bb),
                    gbm = caretModelSpec(method="gbm",verbose=F #, tuneGrid=grid.gbm
                    ))

  mods = caretEnsemble::caretList(x=coded.X, y=train$Yobs,trControl=control, preProcess=preProc,
                                   methodList=methods,tuneLength=n.tune,
                                   tuneList=methods.tl)


  c0 = caret::trainControl(method="cv", number=5, index=ind, predictionBounds=bounds,
                           savePredictions="final",allowParallel=F)

  bart = caretEnsemble::caretList(x=coded.X, y=train$Yobs,  preProcess=preProc,
                                   methodList="bartMachine", trControl=c0, verbose=F, serialize=T,
                                   tuneGrid=data.frame(num_trees=c(50,100), k=2, alpha=0.95, beta=2, nu=3))

  #methods.tl2 = list(avNNet=caretModelSpec(method="avNNet",trace=F, linout=TRUE),
                     #nnet = caretModelSpec(method="nnet",trace=F, linout=TRUE,MaxNWts=maxn),
                     #pcaNNet=caretModelSpec(method="pcaNNet", trace=F, linout=TRUE))



  #nets = caretEnsemble::caretList(x=coded.X, y=train$Yobs, trControl=control,
          #                         preProcess="nzv", tuneLength=n.tune,
         #                          tuneList=methods.tl2)




  all.mods = c(mods, bart)

  tc.new=caret::trainControl(predictionBounds=bounds,
    method="cv",number=5)

  ens.mods = c( "gbm", "svmPoly","knn","cofrest","pcr",
               "svmRadial","foba","glmnet",
               "bartMachine")

  ens.mods = all.mods[tm::removeNumbers(names(all.mods))%in%ens.mods]

  ens = caretEnsemble::caretEnsemble(ens.mods, trControl=tc.new, tuneLength=n.tune*2)
  stack = caretEnsemble::caretStack(all.mods,trControl=tc.new, tuneLength=n.tune*2)
  stack.glm = caretEnsemble::caretStack(all.mods,trControl=tc.new,method="glmnet",
                                        tuneLength=n.tune*2)

  doParallel::stopImplicitCluster()


  fit = list(all.mods, ens, stack, stack.glm)
  names(fit)=c("all.mods", "ens", "stack",
               "stack.glm")
  return(fit)
}

load("Generated Data/all.pilot.RData")
dat = select(all.pilot,-ID,-Q1)
names(dat)[1:2]=c("Yobs","Z")


X0 = select(dat, -Yobs, -Z)
caret::findLinearCombos(X0)


# Preprocess the feature space to remove collinear features
nz = caret::nearZeroVar(X0,uniqueCut=2)
Xall = X0[,-c(nz)]

fc = caret::findCorrelation(cor(as.matrix(Xall)), cutoff=0.9, exact=T)
fc

X = Xall
X$WC.5root=X0$WC^(1/5)

nunique=apply(X,2,function(x)length(unique(x)))
nu = names(X[nunique<20])
tmp = sign(X[,names(X)%in%nu])
names(tmp)=paste0(names(tmp),".any")
X1 = X[,!names(X)%in%nu]

X = cbind(X, tmp)
flc = findLinearCombos(X)
X = X[,-flc$remove]
# Fit a model trained on the pilot data
# Warning! this takes a few minutes to run
table(dat$Yobs) # check bounds
set.seed(123)
dat1 = data.frame(Yobs=dat$Yobs, Z=dat$Z, X)
fit = train_ensemble(dat1, n.tune=4, preProc=NULL, bounds=c(0,11))
save(fit, file="Generated Data/pilotML_model.RData")

# Use the trained model to predict for the current data set
load("Generated Data/all.info.RData")
all = select(all.info, score, more, everything()) %>% rename(Yobs=score, Z=more)
rownames(all)=NULL

all0 = select(all, spellcheck:names(all)[ncol(all)])
all2 = all0^2
names(all2)=paste0(names(all2),".sq")
all.any = sign(all0)
names(all.any)=paste0(names(all.any),".any")
allX = cbind(all0, all2,all.any)
allX$WC.5root=all0$WC^(1/5)

allX = select(allX, names(X))

all.equal(names(allX),names(X))

# Generate predictions for case study sample
newX=as.matrix(allX)

yhat = predict(fit$all.mods, newX)
yhat.ens = predict(fit$ens, newX)
yhat.stack = predict(fit$stack, newX)
yhat.stack.glm = predict(fit$stack.glm, newX)

yhat.all = cbind(yhat, yhat.ens, yhat.stack, yhat.stack.glm)
colnames(yhat.all) = tm::removeNumbers(colnames(yhat.all))

sub = select(all, s_id, t_id, sch_id, subject, grade)
out = as.data.frame(cbind(sub, yhat.all))

all.ML.scores = out
round(cor(all$Yobs,yhat.all),3)


save(all.ML.scores, file="Generated Data/all.ML.scores.RData")
