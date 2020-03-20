##Alternative to ROC curve, choosing predict probability of GLM directly from resulting accuracy
set.seed(618)
testeloopacur <- data.frame(class=c(rep(0,6), rep(1,6)), variable=rnorm(12,10,1), acur=rep(NA,12))
model <- glm(class~variable, data = testeloopacur, family = binomial(link = "logit"))
testeloopacur$probpred <- predict(model, type="response")
class2 <- rep(NA,nrow(testeloopacur))

for(i in seq_along(testeloopacur$probpred)){
  class2 <- ifelse(testeloopacur$probpred<testeloopacur[i,]$probpred,0,1)
  testeloopacur[i,"acur"] <- mean(testeloopacur$class==class2)
}

cutpoint <- testeloopacur[testeloopacur$acur==max(testeloopacur$acur),]$probpred
testeloopacur$classpred <- ifelse(testeloopacur$probpred<cutpoint[1],0,1)
(accuracy <- mean(testeloopacur$class==testeloopacur$classpred))
##If using ROC curve:
require(pROC)
lameROC <- roc(response = testeloopacur$class, predictor = model$fitted.values)
(cutROC=coords(lameROC, "best", method="closest.topleft"))
testeloopacur$class1 <- ifelse(testeloopacur$probpred >= as.numeric(cutROC[1]), 1, 0)
(mean(testeloopacur$class1==testeloopacur$class))
