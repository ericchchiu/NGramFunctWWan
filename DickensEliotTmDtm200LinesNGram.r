#just a preparation work. only a small amount of comments
#set working directory and load packages tm and quanteda
setwd(dirname(file.choose()))
getwd()
if (!require('tm')) install.packages('tm'); library('tm')
if (!require('quanteda')) install.packages('quanteda'); library('quanteda')

#input data and form seven dataframes
if(!file.exists('Gungor_2018_VictorianAuthorAttribution_data-train.csv')){
	download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/00454/dataset.zip', 'dataset.zip')
	unzip('dataset.zip')
	file.copy('./dataset/Gungor_2018_VictorianAuthorAttribution_data-train.csv', '.')
#if the working directory does not have the csv file, this if statement
#needs several minutes to run
}

dfVictorianEraAA <- read.table('Gungor_2018_VictorianAuthorAttribution_data-train.csv', header = TRUE, sep = (','))
dfCharles_Dickens3398_3597 <- data.frame(dfVictorianEraAA[3398:3597,], stringsAsFactors = FALSE)
dfGeorge_Eliot13670_13869 <- data.frame(dfVictorianEraAA[13670:13869,], stringsAsFactors = FALSE)

charBigrams <- function(x) as.character(tokens_ngrams(tokens(gsub(' ', '_', x), what = 'character'), n = 4, concatenator = ''))

dfCharles_Dickens3398_3597$ngram <- lapply(dfCharles_Dickens3398_3597$text, function(x) sapply(x, charBigrams))
dfGeorge_Eliot13670_13869$ngram <- lapply(dfGeorge_Eliot13670_13869$text, function(x) sapply(x, charBigrams))

dfCharles_Dickens3398_3597_corpus <- VCorpus(VectorSource(dfCharles_Dickens3398_3597$ngram))
dfCharles_Dickens3398_3597_corpus <- tm_map(dfCharles_Dickens3398_3597_corpus, stripWhitespace)
dfCharles_Dickens3398_3597_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfCharles_Dickens3398_3597_corpus, control=list(wordLengths = c(1, Inf)))))
dfGeorge_Eliot13670_13869_corpus <- VCorpus(VectorSource(dfGeorge_Eliot13670_13869$ngram))
dfGeorge_Eliot13670_13869_corpus <- tm_map(dfGeorge_Eliot13670_13869_corpus, stripWhitespace)
dfGeorge_Eliot13670_13869_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfGeorge_Eliot13670_13869_corpus, control=list(wordLengths = c(1, Inf)))))

common_cols <- Reduce(intersect, list(colnames(dfCharles_Dickens3398_3597_dtDf), colnames(dfGeorge_Eliot13670_13869_dtDf)))

BindAll7NoOfWdsInAll7UniqWdLst <- rbind(dfCharles_Dickens3398_3597_dtDf[common_cols],dfGeorge_Eliot13670_13869_dtDf[common_cols])

SevenAuthsTtl700OrMore <- BindAll7NoOfWdsInAll7UniqWdLst[, colSums(BindAll7NoOfWdsInAll7UniqWdLst) >= sum(colSums(BindAll7NoOfWdsInAll7UniqWdLst)*0.0005)] #308

SevenAuthsTtl700OrMore$textNo <- rep(1:100, each = 4)
dfAll7WdFeqDf <- aggregate(. ~ textNo, SevenAuthsTtl700OrMore, sum)
dfAll7WdFeqDf$textNo <- NULL

dfAll7WdFeqDf$Label = c(rep('CD', 50), rep('GE', 50))
dfAll7WdFeqDfLabled = dfAll7WdFeqDf[,c(309,1:308)]

# shuffling rows:
set.seed(12345)
rrowNos <- sample(nrow(dfAll7WdFeqDfLabled))
dfAll7WdFeqDfLabledRandm <- dfAll7WdFeqDfLabled[rrowNos,]
#normalisation:
data_norm <- function(x) {(x- min(x))/ (max(x)- min(x))}
dfAll7WdFeqDfLabledRandm_norm <- as.data.frame(lapply(dfAll7WdFeqDfLabledRandm[,-1], data_norm))
summary(dfAll7WdFeqDfLabledRandm_norm[,1:4]) #see whether normalised

#KNN!
if (!require('class')) install.packages('class'); library('class')
dfAll7WdFeqDfLabledRandm_norm_train <- dfAll7WdFeqDfLabledRandm_norm[1:80,]
dfAll7WdFeqDfLabledRandm_norm_test <- dfAll7WdFeqDfLabledRandm_norm[81:100,]
# initial k = sqrt(80) = 9 
whichOfThe7_pred <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfAll7WdFeqDfLabledRandm_norm_test, dfAll7WdFeqDfLabledRandm[1:80,1], k= 5) #5 is the best?
table(pred = whichOfThe7_pred, true_7Authors_KNN = dfAll7WdFeqDfLabledRandm[81:100,1])

#SVM!
if (!require('e1071')) install.packages('e1071'); library('e1071')
# simple: no tunning
whichOfThe7_svm_model <- svm(dfAll7WdFeqDfLabledRandm_norm_train, dfAll7WdFeqDfLabledRandm[1:80,1], type = 'C')
pred <- predict(whichOfThe7_svm_model, dfAll7WdFeqDfLabledRandm_norm_test)
table(pred, true_7Authors_SVM = dfAll7WdFeqDfLabledRandm[81:100,1]) #all correct

#use tunning to find costs
dfAll7WdFeqDfLabledRandmLabel1To280AsFactors = as.factor(dfAll7WdFeqDfLabledRandm[1:80,1])
set.seed(12345)
svm_tune <- tune(svm, train.x = dfAll7WdFeqDfLabledRandm_norm_train,
						train.y = dfAll7WdFeqDfLabledRandmLabel1To280AsFactors,
						kernel = 'linear',
						#type = 'C',
						ranges = list(cost = c(.001,.01,.1,1,5,10,100)))
print(svm_tune) 
svm_tune$best.model
#besides best cost, also best number of support vectors, etc.
pred_svm_after_tune <- predict(svm_tune$best.model, dfAll7WdFeqDfLabledRandm_norm_test)
table(pred = pred_svm_after_tune, true_7Authors_tunedSVM = dfAll7WdFeqDfLabledRandm[81:100,1]) #all correct


#dfCharles_Dickens3398_3597_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfCharles_Dickens3398_3597_corpus, control=list(wordLengths = c(1, Inf)))))

#charBigrams <- function(x) as.character(tokens_ngrams(tokens(gsub(' ', '_', x), what = 'character'), n = 4, concatenator = ''))

#dfCharles_Dickens3398_3597_corpus <- VCorpus(VectorSource(dfCharles_Dickens3398_3597$ngram))


