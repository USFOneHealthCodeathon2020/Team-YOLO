# set.seed(1)
# test_id = sample(seq(1,54),6)

# train_id = seq(1,54)[-test_id]

# remove 0s
ts_mmmb = ts_raw[,c(2:5464,5467,5483,5500,5502,5503)]
ts = ts_mmmb[,1:5462]
ts2 = ts_mmmb[,5463:ncol(ts_mmmb)]
rm(ts_mmmb)
for (i in seq(1,ncol(ts))){
    for (j in seq(1,nrow(ts))){
        if (ts[j,i]==0){
            set.seed(1)
            ts[j,i] <- runif(1,min=0.00001,max=0.0001)
        }
    }
}

# feature selection
ts_clr = clr(ts)
ts = cbind(ts_clr,ts2)
feat_clr = MMPC(ts_raw$obesity_bicat,ts)
ts_mmmb = cbind(ts[,feat_clr@selectedVars],ts_raw$obesity_bicat)
names(ts_mmmb)[ncol(ts_mmmb)] = 'obesity_bicat'

# 10 fold cross validation with 3 repeats
set.seed(1)
ctrl = trainControl(method='repeatedcv',number=10,repeats=3)
grid = expand.grid(C=c(0.001,0.01,seq(0.1,1,0.1),1.5,2,3,5))
mod_test_clr = train(obesity_bicat~.,data=ts_mmmb,trControl=ctrl,method='svmLinear',tuneGrid = grid)
mod_test_clr
