library(tidyverse)
library(magrittr)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(textdata)
library(wordcloud)
library(ggraph)
library(widyr)
library(igraph)
library(shiny)
library(shinydashboard)
library(ROCR)
library(stopwords)
library(textreadr)
library(quanteda)
library(isoband)
library(reticulate)


# Data and data structuring needed to create the Shiny App.
dataset_1 <- read_document(file="Collected data.txt")
dataset_2 <- read_document(file="NLP_Simulation.txt")
class_combo <- c(dataset_1, dataset_2) # Combining the two text files with the survey responses

a <- 62  #number of observation
b <- 6 #number of questions
my_df <- as.data.frame(matrix(nrow=a, ncol=b)) #Creating an empty dataframe

# for loop in for loop to fill the dataframe in the right way
for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- class_combo[i*b+z-b]
  }#closing z loop
}#closing i loop

# separating the questions into data frames to do individual analysis
male_female <- my_df$V1 
haircut     <- my_df$V2
car         <- my_df$V3
vacation    <- my_df$V4
friends     <- my_df$V5
superbowl   <- my_df$V6

# adding location variables to the different questions
haircut_1 <- data_frame(text = haircut, question = 'Question 2', 
                        male_female = male_female, 
                        ID = seq(from = 1, to = 62, by = 1))
car_1 <- data_frame(text = car, question = 'Question 3', 
                    male_female = male_female, 
                    ID = seq(from = 1, to = 62, by = 1))
vacation_1 <- data_frame(text = vacation, question = 'Question 4', 
                         male_female = male_female, 
                         ID = seq(from = 1, to = 62, by = 1))
friends_1 <- data_frame(text = friends, question = 'Question 5', 
                        male_female = male_female, 
                        ID = seq(from = 1, to = 62, by = 1))
superbowl_1 <- data_frame(text = superbowl, question = 'Question 6', 
                          male_female = male_female, 
                          ID = seq(from = 1, to = 62, by = 1))

# combine all data frames into one with all location variables
all_q = bind_rows(list(haircut_1, car_1, vacation_1, friends_1, superbowl_1))

# NOT tokinized data frame
all_q_orig <- all_q %>%
  group_by(ID,question, male_female)%>%
  ungroup()

all_by_id <- all_q %>%
  group_by(ID)

# add some customized stop words to pre loaded package
#stop_words <- bind_rows(data_frame(word = c('um', 'uh'),lexicon = c('CUST', 'CUST')), stop_words)

# tokinized data frame with all location variables 
all_q_tidy <- all_q_orig %>%
  unnest_tokens(word, text)

##########################################################################################################
## SHINY App development 
##########################################################################################################

ui <- dashboardPage(skin = 'green',
  dashboardHeader(title = 'Gender Predictor'),
  dashboardSidebar(
    sidebarMenu( # Adding tabs to the side menu in dashboard
      menuItem('Model', tabName = 'model',   icon = icon('table')),
      menuItem('TF_IDF', tabName = 'tf_idf', icon = icon('bar-chart-o')),
      menuItem('Bigram', tabName = 'bigram', icon = icon('refresh')))),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'model',
              fluidRow(
                box(width = 10, h4(strong('Gender predictor (Bayes Naive Classifier)',)),
                    p('Based on a 5-question survey this model can predict if YOU, the respondant is male or female, right away!'),
                    p('The model was built with audio data from 30 respondants of 20+ nationalities. If you want to explore the "backside" and see the words that effect the prediction check out the rest of the dashboard with the tabs on the left. Also, the intention is not to offend anyone with a binary scale of gender, but for simplicity the model only has "male" and "female" outcomes !' ),
                    h2(' '),
                    h4('\n Try it yourself! Fill out the questions and see the prediction in the panel on the right')
                )),
                box(title = 'Questions',
                    p(' '),
                    textInput(inputId = 'question2', label = h4('Q1. How often do you cut your hair?'),  width = '700px'),
                    textInput(inputId = 'question3', label = h4('Q2. What is important when you buy a car?'),  width = '700px'),
                    textInput(inputId = 'question4', label = h4('Q3. What is the perfect vacation to you?'),  width = '700px'),
                    textInput(inputId = 'question5', label = h4('Q4. What do you like to do with friends?'),  width = '700px'),
                    textInput(inputId = 'question6', label = h4('Q5. How did you watch Super Bowl this year?'),  width = '700px'),
                    status = 'info', solidHeader = TRUE, width = 8),
                box(tableOutput(outputId = 'predict'), width = 3, title = 'Prediction', p('0 = female | 1 = male'), status = 'success', solidHeader = TRUE)
              ),
      tabItem(tabName = 'tf_idf',
              fluidRow(
                box(selectInput(inputId = 'tf_idf_mf', label = 'Male or female',
                                choices = c('Male' = '1',
                                            'Female' = '0')), width = 5),
                box(selectInput(inputId = 'question', label = 'Which question would you like to see the TF-IDF of?', 
                                choices = c('How often do you cut your hair?' = 'Question 2',
                                            'What is important when you buy a car?' = 'Question 3',
                                            'What is perfect vacation to you?' = 'Question 4',
                                            'What do you like to do with friends?' = 'Question 5',
                                            'How will you be watching Super Bowl this year?' = 'Question 6'))),
                box(plotOutput(outputId = 'tf_idf_plot'), width = 8, height = 500, status = 'primary', solidHeader = TRUE, 
                    title = 'Term frequency inverse document frequency'),
                box(tableOutput(outputId = 'tfidftable'), title = 'TF-IDF table', status = 'primary', solidHeader = TRUE, width = 3)
              )),
      tabItem(tabName = 'bigram',
              fluidRow(
                box(selectInput(inputId = 'q_bigram', label = 'Which question would you like to see the TF-IDF of?', 
                                choices = c('How often do you cut your hair?' = 'Question 2',
                                            'What is important when you buy a car?' = 'Question 3',
                                            'What is the perfect vacation to you?' = 'Question 4',
                                            'What do you like to do with friends?' = 'Question 5',
                                            'How will you be watching Super Bowl this year?' = 'Question 6')), width = 4),
                box(plotOutput(outputId = 'bigram_out'), status = 'primary', solidHeader = TRUE, title = 'Bigram network', width = 9, height = 500)
              ))
      )))



server <- function(input, output) {
  
  output$freq_plot <- renderPlot({
    all_q_tidy%>%
      count(word, male_female, question)%>%
      filter(question == input$q_2)%>% # input values is changing the Question the graph will plot
      mutate(word = reorder(word,n))%>%
      filter(n > input$n_slider)%>%
      ggplot(aes(word, n, fill = male_female))+
      geom_col(show.legend = F)+
      xlab(NULL)+
      ylab('Frequency')+
      facet_wrap(~ male_female, scales = 'free_y')+
      coord_flip()
    
  })
  
  output$tf_idf_plot <- renderPlot({
    all_q_tidy%>%
      filter(male_female == input$tf_idf_mf)%>% #input value will change if I see the plot for male or female respondants 
      count(word, question, sort = T)%>%
      bind_tf_idf(word, question, n)%>%
      group_by(question)%>%
      top_n(5)%>%
      ungroup()%>%
      mutate(word = reorder(word, tf_idf))%>%
      ggplot(aes(word, tf_idf, fill = question))+
      geom_col(show.legend = F)+
      labs(x=NULL, y='TF_IDF')+
      facet_wrap(~question, ncol = 2, scales = 'free')+
      coord_flip()
    
    
  })
  
  output$tfidftable <- renderTable({
    # Putting together the tf-idf table 
    tf_idf_table <- all_q_tidy%>%
      filter(male_female == input$tf_idf_mf)%>%
      count(word, question, sort = T)%>%
      bind_tf_idf(word, question, n)%>%
      group_by(question)%>%
      filter(question == input$question)%>%
      arrange(desc(tf_idf))
    
    tf_idf_df <- data.frame(tf_idf_table[1:10 ,c(1,2,6)]) # transforming the output to a data frame so renderTable() can display it
    
    print(tf_idf_df)
  })
  
  output$bigram_out <- renderPlot({
    # Separate the original data set into bigrams 
    survey_bigram <- all_q_orig%>%
      unnest_tokens(bigram, text, token = 'ngrams', n = 2)%>%
      separate(bigram, c('word1','word2'), sep = " ")%>%
      filter(!word1 %in% stop_words$word)%>%
      filter(!word2 %in% stop_words$word)
    
    
    bigram_count <- survey_bigram%>%
      filter(question == input$q_bigram)%>% #input values is changing the Question the graph will plot
      count(word1, word2, sort = TRUE)
    
    set.seed(2020)
    arr <- grid::arrow(type = 'closed', length = unit(.1, 'inches'))
    
    # Bigram network displaying
    ggraph(bigram_count, layout = 'fr')+
      geom_edge_link(aes(edge_alpha=0.6),show.legend = F, arrow = arr, 
                     end_cap = circle(0.07, 'inches'))+
      geom_node_point(color = 'lightblue', size = 2)+
      geom_node_text(aes(label = name), vjust=1,hjust=1)+
      theme_void()
    
  })
  
  output$predict <- renderTable({
    
    validate(
      need(nchar(input$question2) > 7,
           'Please input more text in Q1 for the prediction to work properly')
    )
    validate(
      need(nchar(input$question3) > 7,
           'Please input more text in Q2 for the prediction to work properly')
    )
    validate(
      need(nchar(input$question4) > 7,
           'Please input more text in Q3 for the prediction to work properly')
    )
    validate(
      need(nchar(input$question5) > 7,
           'Please input more text in Q4 for the prediction to work properly')
    )
    validate(
      need(nchar(input$question6) > 7,
           'Please input more text in Q5 for the prediction to work properly')
    )
    
    # Adding the new observation to be predicted
    added_data = tibble(text = c(input$question2,input$question3,
                                 input$question4, input$question5, input$question6), 
                        question = c('Question 2', 'Question 3', 'Question 4',
                                     'Question 5', 'Question 6'), ID = 63)
    
    all_by_id <- bind_rows(all_by_id, added_data)
    
    # All questions data frame tokenized
    all_id_tidy <- all_by_id %>%
      filter(question == 'Question 2')%>%
      unnest_tokens(word, text)
    
    dfm_cast <- all_id_tidy%>%
      count(ID, word)%>%
      cast_dfm(ID, word, n)
    
    dfm_cast.train <- dfm_cast[1:62,]
    dfm_cast.test <- dfm_cast[63:63,]
    
    #building the Naive Bayers model:
    naive_bayes_classifier_2 <- textmodel_nb(dfm_cast.train, as.numeric(male_female[1:62])) # The male/female answer to the questions will be here. if first 3 were 1, then 1,1,1
    summary(naive_bayes_classifier_2)
    
    #predicting the new observation
    pred_2 <- predict(naive_bayes_classifier_2, dfm_cast.test)
    
    pred_2 <- as.numeric(pred_2)- 1
    
    if ((pred_2) == 1){
      pred_2_2 <- ('male')}
    else{
      pred_2_2 <- ('female')
    }
    
    
    ############################################################################################################
    
    all_id_tidy <- all_by_id %>%
      filter(question == 'Question 3')%>%
      unnest_tokens(word, text)
    
    dfm_cast <- all_id_tidy%>%
      count(ID, word)%>%
      cast_dfm(ID, word, n)
    
    dfm_cast.train <- dfm_cast[1:62,]
    dfm_cast.test <- dfm_cast[63:63,]
    
    #building the Naive Bayers model:
    naive_bayes_classifier_3 <- textmodel_nb(dfm_cast.train, as.numeric(male_female[1:62])) # The male/female answer to the questions will be here. if first 3 were 1, then 1,1,1
    summary(naive_bayes_classifier_3)
    
    #predicting the new observation
    pred_3 <- predict(naive_bayes_classifier_3, dfm_cast.test)
    
    pred_3 <- as.numeric(pred_3)- 1
    
    if ((pred_3) == 1){
      pred_3_3 <- ('male')}
    else{
      pred_3_3 <- ('female')
    }
    
    ############################################################################################################
    
    all_id_tidy <- all_by_id %>%
      filter(question == 'Question 4')%>%
      unnest_tokens(word, text)
    
    dfm_cast <- all_id_tidy%>%
      count(ID, word)%>%
      cast_dfm(ID, word, n)
    
    dfm_cast.train <- dfm_cast[1:62,]
    dfm_cast.test <- dfm_cast[63:63,]
    
    #building the Naive Bayers model:
    naive_bayes_classifier_4 <- textmodel_nb(dfm_cast.train, as.numeric(male_female[1:62])) # The male/female answer to the questions will be here. if first 3 were 1, then 1,1,1
    summary(naive_bayes_classifier_4)
    
    #predicting the new observation
    pred_4 <- predict(naive_bayes_classifier_4, dfm_cast.test)
    
    pred_4 <- as.numeric(pred_4)- 1
    
    if ((pred_4) == 1){
      pred_4_4 <- ('male')}
    else{
      pred_4_4 <- ('female')
    }
    
    ##########################################################################################################
    
    all_id_tidy <- all_by_id %>%
      filter(question == 'Question 5')%>%
      unnest_tokens(word, text)
    
    dfm_cast <- all_id_tidy%>%
      count(ID, word)%>%
      cast_dfm(ID, word, n)
    
    dfm_cast.train <- dfm_cast[1:62,]
    dfm_cast.test <- dfm_cast[63:63,]
    
    #building the Naive Bayers model:
    naive_bayes_classifier_5 <- textmodel_nb(dfm_cast.train, as.numeric(male_female[1:62])) # The male/female answer to the questions will be here. if first 3 were 1, then 1,1,1
    summary(naive_bayes_classifier_5)
    
    #predicting the new observation
    pred_5 <- predict(naive_bayes_classifier_5, dfm_cast.test)
    
    pred_5 <- as.numeric(pred_5)- 1
    
    if ((pred_5) == 1){
      pred_5_5 <- ('male')}
    else{
      pred_5_5 <- ('female')
    }
    
    ##########################################################################################################
    
    all_id_tidy <- all_by_id %>%
      filter(question == 'Question 6')%>%
      unnest_tokens(word, text)
    
    dfm_cast <- all_id_tidy%>%
      count(ID, word)%>%
      cast_dfm(ID, word, n)
    
    dfm_cast.train <- dfm_cast[1:62,]
    dfm_cast.test <- dfm_cast[63:63,]
    
    #building the Naive Bayers model:
    naive_bayes_classifier_6 <- textmodel_nb(dfm_cast.train, as.numeric(male_female[1:62])) # The male/female answer to the questions will be here. if first 3 were 1, then 1,1,1
    summary(naive_bayes_classifier_6)
    
    pred_6 <- predict(naive_bayes_classifier_6, dfm_cast.test)
    
    pred_6 <- as.numeric(pred_6)-1
    
    if ((pred_6) == 1){
      pred_6_6 <- ('male')}
    else{
      pred_6_6 <- ('female')
    }
    
    # Result line of predictive table 
    pred_sum <- (pred_2 + pred_3 + pred_4 + pred_5 + pred_6)
    
    if (pred_sum <= 2){
      gender_pred <- ("FEMALE")
    } else {
      gender_pred <- ("MALE")
    }
    
    # Creating the final prediction table 
    Questions <- matrix(c('Q1','Q2','Q3','Q4','Q5', 'Result', pred_2, pred_3,
                          pred_4, pred_5,pred_6, pred_sum, pred_2_2, pred_3_3,
                          pred_4_4, pred_5_5,pred_6_6, gender_pred), ncol = 3, byrow = F)
    colnames(Questions) <- c('Question', 'Result', 'Gender')
    df <- data.frame(Questions)
    
    print(df)
    
  })
}

shinyApp(ui = ui, server = server)


