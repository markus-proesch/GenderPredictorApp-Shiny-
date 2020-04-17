# Gender Predictor Application in R Shiny 

Based on 30 audio responses I built an interactive application that can predict the gender of the respondent. Using
Naive Bayes Classifier to predict either "male" = 1 and "female" = 0. The data had to be cleaned, restructured (from unstructured to structured data) and put into a tidy format before casting it into a DocumentTermMatrix to greate a sparcity matrix. The Naive Bias Classifier is a supervised learning method which creates a probability of each word "belonging" to a male or female respondant. The model is sophisticated enough to predict each word individually, for every question seperately. So if a respondant gives the same answer to multiple questions it can give different predictions per question. The use cases and opporunities are endless to predict different outcomes based on written or auido data. 

Check out the application yourself: https://markusproesch.shinyapps.io/Gender_Predictor/
