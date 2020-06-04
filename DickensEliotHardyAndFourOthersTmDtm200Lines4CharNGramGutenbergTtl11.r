#7 authors. for more details of the code below, see:
#DickensEliotHardyAndFourOthersTmDtm200LinesEachAndEliotImmBlr80End80GutenbergATaleOf2C3Mddlem5Tess3.r
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
dfCharles_Dickens3398_3597 <- dfVictorianEraAA[3398:3597,]
dfFergue_Hume12558_12757 <- dfVictorianEraAA[12558:12757,]
dfGeorge_Eliot13670_13869 <- dfVictorianEraAA[13670:13869,]
dfHelen_Mathers18009_18208 <- dfVictorianEraAA[18009:18208,]
dfLucas_Malet33860_34059 <- dfVictorianEraAA[33860:34059,]
dfMarie_Corelli34563_34762 <- dfVictorianEraAA[34563:34762,]
dfThomas_Hardy48023_48222 <- dfVictorianEraAA[48023:48222,]

charBigrams <- function(x) as.character(tokens_ngrams(tokens(gsub(' ', '_', x), what = 'character'), n = 4, concatenator = ''))

dfCharles_Dickens3398_3597$ngram <- lapply(dfCharles_Dickens3398_3597$text, function(x) sapply(x, charBigrams))
dfFergue_Hume12558_12757$ngram <- lapply(dfFergue_Hume12558_12757$text, function(x) sapply(x, charBigrams))
dfGeorge_Eliot13670_13869$ngram <- lapply(dfGeorge_Eliot13670_13869$text, function(x) sapply(x, charBigrams))
dfHelen_Mathers18009_18208$ngram <-  lapply(dfHelen_Mathers18009_18208$text, function(x) sapply(x, charBigrams))
dfLucas_Malet33860_34059$ngram <- lapply(dfLucas_Malet33860_34059$text, function(x) sapply(x, charBigrams))
dfMarie_Corelli34563_34762$ngram <-  lapply(dfMarie_Corelli34563_34762$text, function(x) sapply(x, charBigrams))
dfThomas_Hardy48023_48222$ngram <- lapply(dfThomas_Hardy48023_48222$text, function(x) sapply(x, charBigrams))

dfCharles_Dickens3398_3597_corpus <- VCorpus(VectorSource(dfCharles_Dickens3398_3597$ngram))
dfCharles_Dickens3398_3597_corpus <- tm_map(dfCharles_Dickens3398_3597_corpus, stripWhitespace)
dfFergue_Hume12558_12757_corpus <- VCorpus(VectorSource(dfFergue_Hume12558_12757$ngram))
dfFergue_Hume12558_12757_corpus <- tm_map(dfFergue_Hume12558_12757_corpus, stripWhitespace)
dfGeorge_Eliot13670_13869_corpus <- VCorpus(VectorSource(dfGeorge_Eliot13670_13869$ngram))
dfGeorge_Eliot13670_13869_corpus <- tm_map(dfGeorge_Eliot13670_13869_corpus, stripWhitespace)
dfHelen_Mathers18009_18208_corpus <- VCorpus(VectorSource(dfHelen_Mathers18009_18208$ngram))
dfHelen_Mathers18009_18208_corpus <- tm_map(dfHelen_Mathers18009_18208_corpus, stripWhitespace)
dfLucas_Malet33860_34059_corpus <- VCorpus(VectorSource(dfLucas_Malet33860_34059$ngram))
dfLucas_Malet33860_34059_corpus <- tm_map(dfLucas_Malet33860_34059_corpus, stripWhitespace)
dfMarie_Corelli34563_34762_corpus <- VCorpus(VectorSource(dfMarie_Corelli34563_34762$ngram))
dfMarie_Corelli34563_34762_corpus <- tm_map(dfMarie_Corelli34563_34762_corpus, stripWhitespace)
dfThomas_Hardy48023_48222_corpus <- VCorpus(VectorSource(dfThomas_Hardy48023_48222$ngram))
dfThomas_Hardy48023_48222_corpus <- tm_map(dfThomas_Hardy48023_48222_corpus, stripWhitespace)

#form dtm. Each line(1000 words) a document
dfCharles_Dickens3398_3597_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfCharles_Dickens3398_3597_corpus)))
dfFergue_Hume12558_12757_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfFergue_Hume12558_12757_corpus)))
dfGeorge_Eliot13670_13869_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfGeorge_Eliot13670_13869_corpus)))
dfHelen_Mathers18009_18208_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfHelen_Mathers18009_18208_corpus)))
dfLucas_Malet33860_34059_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfLucas_Malet33860_34059_corpus)))
dfMarie_Corelli34563_34762_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfMarie_Corelli34563_34762_corpus)))
dfThomas_Hardy48023_48222_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfThomas_Hardy48023_48222_corpus)))

#retain columns of words which can found in every of the seven authors' texts
common_cols <- Reduce(intersect, list(colnames(dfCharles_Dickens3398_3597_dtDf), colnames(dfFergue_Hume12558_12757_dtDf), colnames(dfGeorge_Eliot13670_13869_dtDf), colnames(dfHelen_Mathers18009_18208_dtDf), colnames(dfLucas_Malet33860_34059_dtDf), colnames(dfMarie_Corelli34563_34762_dtDf), colnames(dfThomas_Hardy48023_48222_dtDf)))
BindAll7NoOfWdsInAll7UniqWdLst <- rbind(dfCharles_Dickens3398_3597_dtDf[common_cols], dfFergue_Hume12558_12757_dtDf[common_cols], dfGeorge_Eliot13670_13869_dtDf[common_cols], dfHelen_Mathers18009_18208_dtDf[common_cols], dfLucas_Malet33860_34059_dtDf[common_cols], dfMarie_Corelli34563_34762_dtDf[common_cols],
dfThomas_Hardy48023_48222_dtDf[common_cols])

SevenAuthsTtl5TenKthOrMore <- BindAll7NoOfWdsInAll7UniqWdLst[, colSums(BindAll7NoOfWdsInAll7UniqWdLst) >= sum(colSums(BindAll7NoOfWdsInAll7UniqWdLst)*0.0005)] #297

SevenAuthsTtl5TenKthOrMore$textNo <- rep(1:350, each = 4)
dfAll7WdFeqDf <- aggregate(. ~ textNo, SevenAuthsTtl5TenKthOrMore, sum)
dfAll7WdFeqDf$textNo <- NULL

#add labels and move the label column to the first column
dfAll7WdFeqDf$Label = c(rep('CD', 50), rep('FH', 50), rep('GE', 50), rep('HM', 50), rep('LM', 50), rep('MC', 50), rep('TH', 50)) #297+1 = 298
dfAll7WdFeqDfLabled <- dfAll7WdFeqDf[,c(ncol(dfAll7WdFeqDf),1:ncol(dfAll7WdFeqDf)-1)]

# shuffling rows:
set.seed(12345)
rrowNos <- sample(nrow(dfAll7WdFeqDfLabled))
dfAll7WdFeqDfLabledRandm <- dfAll7WdFeqDfLabled[rrowNos,]
#normalisation:
data_norm <- function(x) {(x- min(x))/ (max(x)- min(x))}
dfAll7WdFeqDfLabledRandm_norm <- as.data.frame(lapply(dfAll7WdFeqDfLabledRandm[,-1], data_norm))
summary(dfAll7WdFeqDfLabledRandm_norm[,1:4]) #see whether normalised

#KNN!
#number of k: usually start from sqrt of data points of
#the training set. So sqrt(280): 18
#then trial and error
if (!require('class')) install.packages('class'); library('class')
dfAll7WdFeqDfLabledRandm_norm_train <- dfAll7WdFeqDfLabledRandm_norm[1:280,]
dfAll7WdFeqDfLabledRandm_norm_test <- dfAll7WdFeqDfLabledRandm_norm[281:350,]
whichOfThe7_pred <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfAll7WdFeqDfLabledRandm_norm_test, dfAll7WdFeqDfLabledRandm[1:280,1], k= 18) #17, 19 are good
table(pred = whichOfThe7_pred, true_7Authors_KNN = dfAll7WdFeqDfLabledRandm[281:350,1])#mistake rate 3/70

#SVM!
if (!require('e1071')) install.packages('e1071'); library('e1071')
# simple: no tunning
whichOfThe7_svm_model <- svm(dfAll7WdFeqDfLabledRandm_norm_train, dfAll7WdFeqDfLabledRandm[1:280,1], type = 'C')
pred <- predict(whichOfThe7_svm_model, dfAll7WdFeqDfLabledRandm_norm_test)
table(pred, true_7Authors_SVM = dfAll7WdFeqDfLabledRandm[281:350,1]) #mistake rate 1/70

#use tunning to find costs
dfAll7WdFeqDfLabledRandmLabel1To280AsFactors = as.factor(dfAll7WdFeqDfLabledRandm[1:280,1])
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
table(pred = pred_svm_after_tune, true_7Authors_tunedSVM = dfAll7WdFeqDfLabledRandm[281:350,1]) #all correct

#---------------------------------------------------------------
#Charles Dickens's masterpiece A Tale of Two Cities, George Eliot's Middlemarch (300000+ words), and Thomas Hardy's Tess of the d'Urbervilles

dfGutenberg2CitiesMddlemarchTess <- read.table('GutenbergATaleOf2C3Middlemarch5TessOfTheDUrb3Each4000Words.csv', header = TRUE, sep = (','), comment.char = "#")
#dfGutenberg2CitiesMddlemarchTess <- rbind(dfGutenbergATaleOf2C, dfGutenbergMiddlemarch, dfGutenbergTessOfTheDUrb)

dfGutenberg2CitiesMddlemarchTess$ngram <- lapply(dfGutenberg2CitiesMddlemarchTess$text, function(x) sapply(x, charBigrams))
dfGutenberg2CitiesMddlemarchTess_corpus <- VCorpus(VectorSource(dfGutenberg2CitiesMddlemarchTess$ngram))

dfGutenberg2CitiesMddlemarchTess_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfGutenberg2CitiesMddlemarchTess_corpus,)))
#dfGutenberg2CitiesMddlemarchTess_dtDf$textNo <- NULL
dfGutenberg2CitiesMddlemarchTess_dtDf[setdiff(colnames(dfAll7WdFeqDfLabled), colnames(dfGutenberg2CitiesMddlemarchTess_dtDf))] <- 0
dfGutenberg2CitiesMddlemarchTessWdFeqDf <- dfGutenberg2CitiesMddlemarchTess_dtDf[colnames(dfAll7WdFeqDfLabled)]
dfGutenberg2CitiesMddlemarchTessWdFeqDf$Label <- NULL #delete Label col
dfGutenberg2CitiesMddlemarchTessWdFeqDf_addMaxMin = rbind(dfGutenberg2CitiesMddlemarchTessWdFeqDf, apply(dfAll7WdFeqDfLabledRandm[,-1], 2, max), apply(dfAll7WdFeqDfLabledRandm[,-1], 2, min))
dfGutenberg2CitiesMddlemarchTessWdFeqDf_normNotReal = (dfGutenberg2CitiesMddlemarchTessWdFeqDf_addMaxMin[1,] - dfGutenberg2CitiesMddlemarchTessWdFeqDf_addMaxMin[13,]) / (dfGutenberg2CitiesMddlemarchTessWdFeqDf_addMaxMin[12,] - dfGutenberg2CitiesMddlemarchTessWdFeqDf_addMaxMin[13,])
normGeEtc = function(x, y) {
for (i in 2: (nrow(y)-2)) {
x = rbind(x, (y[i,] - y[nrow(y),]) / (y[(nrow(y)-1),] - y[nrow(y),]))
}
return(x)
}
dfGutenberg2CitiesMddlemarchTessWdFeqDf_normNotReal <- normGeEtc(dfGutenberg2CitiesMddlemarchTessWdFeqDf_normNotReal, dfGutenberg2CitiesMddlemarchTessWdFeqDf_addMaxMin)

#KNN! 
set.seed(12345)
pred_knn_3TwoCities5Mddlemarch3Tess <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfGutenberg2CitiesMddlemarchTessWdFeqDf_normNotReal, dfAll7WdFeqDfLabledRandm[1:280,1], k= 18)
table(pred = pred_knn_3TwoCities5Mddlemarch3Tess, true_3TwoCities5Mddlemarch3Tess_KNN = dfGutenberg2CitiesMddlemarchTess$label) #not accurate 16 best 4/11 wrong!

#svm_no_tune
pred_svm_3TwoCities5Mddlemarch3Tess <- predict(whichOfThe7_svm_model, dfGutenberg2CitiesMddlemarchTessWdFeqDf_normNotReal)
table(pred = pred_svm_3TwoCities5Mddlemarch3Tess, true_3TwoCities5Mddlemarch3Tess_SVM = dfGutenberg2CitiesMddlemarchTess$label) #all correct
#svm_tuned
pred_svm_after_tune_3TwoCities5Mddlemarch3Tess <- predict(svm_tune$best.model, dfGutenberg2CitiesMddlemarchTessWdFeqDf_normNotReal)
table(pred = pred_svm_after_tune_3TwoCities5Mddlemarch3Tess, true_3TwoCities5Mddlemarch3Tess_tunedSVM = dfGutenberg2CitiesMddlemarchTess$label) #all correct



