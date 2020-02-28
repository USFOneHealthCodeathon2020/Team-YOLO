### build two branches Building own model and Evaluate on default model.
library(MXM)
library(caret)
library(e1071)
# set.seed(1)
# test_id = sample(seq(1,54),6)

# train_id = seq(1,54)[-test_id]

# get OTU, age, ancestry, family, twin_mother, zygosity
ts_mmmb = ts_raw[,c(2:5464,5467,5483,5502,5503)]
ts = ts_mmmb[,1:5462]
ts2 = ts_mmmb[,5463:ncol(ts_mmmb)]
# replace 0s with random small numebrs
seed = 0
for (i in seq(1,ncol(ts))){
    for (j in seq(1,nrow(ts))){
        if (ts[j,i]==0){
            seed = seed +1
            set.seed(seed)
            ts[j,i] <- runif(1,min=0.00001,max=0.0001)
        }
    }
}

# Centered log-ratio normalization
ts_clr = clr(ts)
ts = cbind(ts_clr,ts2)
feat_clr = MMPC(ts_raw$obesity_bicat,ts)
ts_mmmb = cbind(ts[,feat_clr@selectedVars],ts_raw$obesity_bicat)
names(ts_mmmb)[ncol(ts_mmmb)] = 'obesity_bicat'

# Create different weights to deal with class-imbalance
model_weights = ifelse(ts_mmmb$obesity_bicat=='Lean',
                        (1/table(ts_mmmb$obesity_bicat)[1])*0.5,
                        (1/table(ts_mmmb$obesity_bicat)[2])*0.5)


# linear regression
mod_test_clr = glm(obesity_bicat~.,family='binomial',data=ts_mmmb)
#to get OR:
exp(coef(mod_test_clr$finalModel))

# linear SVM
# 10 fold cross validation with 3 repeats
set.seed(1)
ctrl = trainControl(method='repeatedcv',number=10,repeats=3)
grid = expand.grid(C=c(0.001,0.01,seq(0.1,1,0.1),1.5,2,3,5))
mod_test_clr = train(obesity_bicat~.,data=ts_mmmb,weights = model_weights,trControl=ctrl,method='svmLinear',tuneGrid = grid)

#KNN
set.seed(1)
ctrl = trainControl(method='repeatedcv',number=10,repeats=3)
grid = expand.grid(k = c(3,5,7,9,11,13))
mod_test_clr = train(obesity_bicat~.,data=ts_mmmb,weights = model_weights,trControl=ctrl,method = 'knn',tuneGrid=grid)

# adaboost, mtry: number of features to use
set.seed(2)
ctrl = trainControl(method='repeatedcv',number=10,repeats=3)
grid = expand.grid(mtry = c(2,4,6,8,10,15,20,26))
mod_test_clr = train(obesity_bicat~.,data=ts_mmmb,weights = model_weights,trControl=ctrl,methods = 'Adaboost.M2',tuneGrid = grid)

