library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(scales)
library(reshape)

data <- read.csv("bioworld_data.csv")

server <- function(input, output) {
  
  output$test <- renderTable({
    data[grep(input$userselect, data$user), ]
  })
  
  output$text <- renderText({
    paste("Following is the data for user", input$userselect)
  })
  
  output$test1 <- renderTable({
    d <- data[grep(input$userselect, data$user), ]
    d[grep(input$caseselect, d$case), ]
  })
  
  output$text1 <- renderText({
    paste("Following is the data for case", input$caseselect)
  })
  
  output$goal_orientation_1 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    acc <- "Performance avoidance"
    addClass("goal1", "big")
    addClass("goal1", "indent")
    if(s_data[,"goal_orientation_pa"] == 1){
      removeClass("goal1", "blue")
      removeClass("goal1", "black")
      addClass("goal1", "blue")
    }
    else {
      removeClass("goal1", "blue")
      removeClass("goal1", "black")
      addClass("goal1", "black")
    }
    paste(acc)
  })
  
  output$goal_orientation_2 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    acc <- "Performance prove"
    addClass("goal2", "big")
    addClass("goal2", "indent")
    if(s_data[,"goal_orientation_pp"] == 1){
      removeClass("goal2", "blue")
      removeClass("goal2", "black")
      addClass("goal2", "blue")
    }
    else {
      removeClass("goal2", "blue")
      removeClass("goal2", "black")
      addClass("goal2", "black")
    }
    paste(acc)
  })
  
  output$goal_orientation_3 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    acc <- "Mastery oriented"
    addClass("goal3", "big")
    addClass("goal3", "indent")
    if(s_data[,"goal_orientation_m"] == 1){
      removeClass("goal3", "blue")
      removeClass("goal3", "black")
      addClass("goal3", "blue")
    }
    else {
      removeClass("goal3", "blue")
      removeClass("goal3", "black")
      addClass("goal3", "black")
    }
    paste(acc)
  })
  
  output$learner1.1 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    if (s_data[,"goal_orientation_pa"] == 1){
      paste("Performance-avoid learner: ")
      
    }
  })
  
  output$learner1 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    if (s_data[,"goal_orientation_pa"] == 1){
      paste("You desire to learn because you do not want to disappoint others or look stupid to others. You may choose to avoid a difficult task because you would not likely perform well. This motivation may be a problem because you are opting out of positive learning opportunities."
      )
    }
  })
  
  output$learner2.1 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    if (s_data[,"goal_orientation_pp"] == 1){
      paste("Performance-prove learner: ")
    }
  })
  
  output$learner2 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    if (s_data[,"goal_orientation_pp"] == 1){
      paste("You are driven to show others that you are competent and that you understand the material. You are intrigued to learn because you want to prove to others. Often this motivation may lead to positive learning outcomes, but your decisions are heavily influence by other people's expectations. "
      )
    }
  })
  
  output$learner3.1 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    if(s_data[,"goal_orientation_m"] == 1){
      paste("Mastery-oriented learner: ")
    }
  })
  
  output$learner3 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    if(s_data[,"goal_orientation_m"] == 1){
      paste("You are intrigued to learn because you enjoy learning and you desire to understand more about the material. You intend to master the material and this motivation is helpful to your learning. You can continue to focus on your learning and progress."
      )
    }
    
  })
  
  output$emotion1.1 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    if (s_data[,"feq_class"] == "Pride"){
      paste("Pride:")
      
    }
  })
  
  output$emotion1 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    if (s_data[,"feq_class"] == "Pride"){
      paste("You have received feedback with your performance and the expert solution of the case. We then asked you several questions about your attitude towards feedback and you seem to pride in completing this case and receiving feedback about your performance. It is positive to be encouraged by your performance on this case. Keep it up! 
"
      )
    }
  })
  
  output$emotion2.1 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    if (s_data[,"feq_class"] == "Pride/Relief"){
      paste("Pride / Relief")
    }
  })
  
  output$emotion2 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    if (s_data[,"feq_class"] == "Pride/Relief"){
      paste("You have received feedback with your performance and the expert solution of the case. We then asked you several questions about your attitude towards feedback and you seem to take pride in completing this case and receiving feedback about your performance. You also seem relieved at the feedback you received. It is positive to be encouraged by your performance on this case. You may want to focus on reflecting on how to apply the feedback received from the expert solution and not be discouraged by the negative feedback. Keep it up!   
"
      )
    }
  })
  
  output$emotion3.1 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    if(s_data[,"feq_class"] == "Shame/Relief"){
      paste("Relief / Shame")
    }
  })
  
  output$emotion3 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    if(s_data[,"feq_class"] == "Shame/Relief"){
      paste("When you reviewed your feedback, you indicated that you were relieved. You also seem to be embarrassed with the results and perhaps a bit discouraged. Try to focus on your learning progress and be encouraged by how much you’ve learned, instead of focusing on the outcome. Keep up the good work!
"
      )
    }
    
  })
  
  output$s_total_time <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_total_time <- s_data[,19]
    # mins <- s_total_time / 60
    mins <- s_total_time %/% 60
    seconds <- s_total_time %% 60
    paste(mins, "minutes ", seconds, "seconds")
  })
  
  
  
  # output$area <- renderPlot({
  #   s_data <- data[grep(input$userselect, data$user), ][3,]
  #   s_data_area <- s_data[,3:6]
  #   barplot(as.matrix(s_data_area, mode = "numeric"), 
  #           main = "Time Spent in Each Area", 
  #           xlab = "Area", 
  #           ylab = "Time in seconds", 
  #           names.arg = c("Chart", "History", "Library", "Summarize"), 
  #           col = "#2980b9",
  #           width = 3,
  #           xlim = c(0,20))
  # })
  
  output$s_area <- renderPlot({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_data_area <- s_data[,3:6]
    Area <- rep(c("Chart", "History", "Library", "Summarize"), 2)
    Time <- as.vector(s_data_area[1,], mode = "numeric")
    graph_data <- data.frame(Area, Time)
    ggplot(graph_data, aes(x = Area, y = Time, label = Time)) + 
      geom_bar(stat = "identity", fill="#25af98", width = 0.6, position = position_dodge(width = 0.9)) +
      geom_text(size = 6, vjust =-0.2) +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16)) +
      theme(plot.margin = unit(c(1,1,1,1), "cm")) +
      theme(plot.background = element_rect(fill = "transparent", color = NULL, inherit.blank = TRUE),
            panel.background = element_blank(),
            # panel.background = element_rect(fill = "#FBFBFB", color = NULL, inherit.blank = TRUE),
            panel.grid.major.x = element_blank()
      ) +
      ylab("Time in seconds")
  },bg="transparent")
  
  
  output$most_area <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_data_area <- s_data[,3:6]
    area <- colnames(s_data_area)[apply(s_data_area,1,which.max)]
    acc <- ""
    if (area == "time_interval_area_history"){
      acc <- "You spent the MOST of time in History"
    }
    if (area == "time_interval_area_chart"){
      acc <- "You spent the MOST of time in Chart"
    }
    if (area == "time_interval_area_library"){
      acc <- "You spent the MOST of time in Library"
    }
    if (area == "time_interval_area_summarize"){
      acc <- "You spent the MOST of time in Summarize"
    }
    paste(acc)
  })
  
  output$most_area_1 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_data_area <- s_data[,3:6]
    area <- colnames(s_data_area)[apply(s_data_area,1,which.max)]
    acc <- ""
    if (area == "time_interval_area_history"){
      acc <- "In the History area, you typically spent time on adding evidence and evaluated what evidences are required for supporting your hypotheses. You selected the most probable hypothesis to make your diagnosis. "
    }
    if (area == "time_interval_area_chart"){
      acc <- "In the Chart area, you added evidence from the patient history and evaluated your confidence in the hypotheses selected. You also ordered lab tests to gather information about the most appropriate diagnosis. "
    }
    if (area == "time_interval_area_library"){
      acc <- "In the Library area, you evaluated your confidence in your selected hypotheses and prioritized your hypotheses to identify your final diagnosis. You also accessed the additional information for supporting your final diagnosis and submitted your decision. "
    }
    if (area == "time_interval_area_summarize"){
      acc <- "In the Summarize area, you reviewed the evidence, symptoms, and vital signs before submitting your final diagnosis. You also reviewed your hypothesis and justified the probability of a hypothesis based on the outcome of a lab test. "
    }
    paste(acc)
  })
  
  
  
  output$least_area <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_data_area <- s_data[,3:6]
    area <- colnames(s_data_area)[apply(s_data_area,1,which.min)]
    acc <- ""
    if (area == "time_interval_area_history"){
      acc <- "You spent the LEAST of time in History"
    }
    if (area == "time_interval_area_chart"){
      acc <- "You spent the LEAST of time in Chart"
    }
    if (area == "time_interval_area_library"){
      acc <- "You spent the LEAST of time in Library"
    }
    if (area == "time_interval_area_summarize"){
      acc <- "You spent the LEAST of time in Summarize"
    }
    paste(acc)
  })
  
  
  output$least_area_1 <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_data_area <- s_data[,3:6]
    area <- colnames(s_data_area)[apply(s_data_area,1,which.min)]
    acc <- ""
    if (area == "time_interval_area_history"){
      acc <- "In this area, consider spending more time on adding evidence and evaluating what evidences are required for supporting your hypotheses. You can spend more time considering the most probable hypothesis to support your diagnosis. "
    }
    if (area == "time_interval_area_chart"){
      acc <- "In this area, consider spending more time on adding evidence from the patient history and evaluating your confidence in the hypothesis selected. You can spend more time ordering more lab tests to gather information about the most appropriate diagnosis."
    }
    if (area == "time_interval_area_library"){
      acc <- "In this area, consider spending more time on evaluating your confidence in your selected hypotheses and prioritizing your hypotheses to identify your final diagnosis. You can also access additional information to support your final diagnosis. "
    }
    if (area == "time_interval_area_summarize"){
      acc <- "In this area, consider spending more time on reviewing the evidence, symptoms, and vital signs before submitting your final diagnosis. You can also review your hypothesis and justify the probability of a hypothesis based on the outcome of a lab test."
    }
    paste(acc)
  })
  
  output$s_action <- renderPlot({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_data_action <- s_data[,8:18]
    Action <- rep(c("linking evidence","adding evidence", "unlinking evidence","lab test", "categorize", "change hypothesis", 
                  "prioritize", "delete hypothesis", "select hypothesis", "check summary", "help"), 2)
    Time <- as.vector(s_data_action[1,], mode = "numeric")
    graph_data <- data.frame(Action, Time)
    ggplot(graph_data, aes(x = Action, y = Time, label = Time)) + 
      geom_bar(stat = "identity", fill="#25af98", width = 0.6, position = position_dodge(width = 0.9)) +
      geom_text(size = 5, vjust = 0) +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16)) +
      theme(plot.margin = unit(c(1,1,1,1), "cm")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(plot.background = element_rect(fill = "transparent", color = NULL, inherit.blank = TRUE),
            panel.background = element_blank(),
            # panel.background = element_rect(fill = "#FBFBFB", color = NULL, inherit.blank = TRUE),
            panel.grid.major.x = element_blank()
            ) +
      ylab("Time in seconds")
  },
  bg="transparent")
  
  # blank theme for pie plot
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank()
      # ,plot.title=element_text(size=14, face="bold")
    )
  
  output$s_evidence <- renderPlot({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    correct <- s_data[,20]
    s_data_evidence <- data.frame(group = c("Evidence relates to correct diagnosis","Evidence does not relate to correct diagnosis")
                                  , percentage = c(correct, 100 - correct))
    
    ggplot(s_data_evidence, aes(x="", y = percentage, label = percentage, fill= group)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      theme(plot.margin = unit(c(0,0,0,0), "cm")) +
      blank_theme +
      theme(axis.text.x=element_blank()) +
      geom_text(aes(y = percentage/3 + c(0, cumsum(percentage)[-length(percentage)]), 
                    label = percent(percentage/100)), size=7) +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_blank()) +
      # ggtitle("Percentage of Evidence Correct") +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.1, size = 18))

  },
  bg="transparent")
  
  
  
  output$s_labtests <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_total_labtests <- s_data[,21]
    paste(s_total_labtests, "lab tests")
  })
  
  
  output$s_confidence <- renderPlot({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    confidence <- s_data[,22]
    s_data_confidence <- data.frame(group = c("Lack of confidence in hypothesis", "Confidence in hypothesis")
                                  , percentage = c(100 - confidence, confidence))
    
    ggplot(s_data_confidence, aes(x="", y = percentage, label = percentage, fill= group)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      theme(plot.margin = unit(c(1,1,1,1), "cm")) +
      blank_theme +
      theme(axis.text.x=element_blank()) +
      geom_text(aes(y = percentage/3 + c(0, cumsum(percentage)[-length(percentage)]), 
                    label = percent(percentage/100)), size=7) +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_blank()) +
      # ggtitle("Confidence in Hypothesis") +
      theme(plot.title = element_text(hjust = 0.6,  size = 18))
  },
  bg="transparent")
  
  output$s_diff_easy <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_perceived_difficulty <- s_data[,23]
    addClass("s_diff_easy_col", "big")
    addClass("s_diff_easy_col", "indent")
    removeClass("s_diff_easy_col", "too_easy")
    removeClass("s_diff_easy_col", "black")
    if (s_perceived_difficulty == "Too Easy") {
      addClass("s_diff_easy_col", "too_easy")
    } else {
      addClass("s_diff_easy_col", "black")
    }
    paste("Too Easy")
  })
  
  output$s_diff_good <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_perceived_difficulty <- s_data[,23]
    addClass("s_diff_good_col", "big")
    addClass("s_diff_good_col", "indent")
    removeClass("s_diff_good_col", "good_review")
    removeClass("s_diff_good_col", "black")
    if (s_perceived_difficulty == "Good Review") {
      addClass("s_diff_good_col", "good_review")
    } else {
      addClass("s_diff_good_col", "black")
    }
    paste("Good Review")
  })
  
  output$s_diff_chal <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_perceived_difficulty <- s_data[,23]
    addClass("s_diff_chal_col", "big")
    addClass("s_diff_chal_col", "indent")
    removeClass("s_diff_chal_col", "challenging")
    removeClass("s_diff_chal_col", "black")
    if (s_perceived_difficulty == "Challenging") {
      addClass("s_diff_chal_col", "challenging")
    } else {
      addClass("s_diff_chal_col", "black")
    }
    paste("Challenging")
  })
  
  output$s_diff_diff <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_perceived_difficulty <- s_data[,23]
    addClass("s_diff_diff_col", "big")
    addClass("s_diff_diff_col", "indent")
    removeClass("s_diff_diff_col", "difficult")
    removeClass("s_diff_diff_col", "black")
    if (s_perceived_difficulty == "Difficult") {
      addClass("s_diff_diff_col", "difficult")
    } else {
      addClass("s_diff_diff_col", "black")
    }
    paste("Difficult")
  })
  
  
  output$s_correctness <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_diagnosis_correctness <- s_data[,24]
    outcome = ""
    if(s_diagnosis_correctness == "1"){
      outcome <- "Correct"
    }else{
      outcome <- "Incorrect"
    }
    paste(outcome)
  })
  
  output$emotion_pride <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    acc <- "Pride"
    addClass("col_pride", "big")
    addClass("col_pride", "indent")
    if(s_data[,"feq_class"] == "Pride"){
      removeClass("col_pride", "blue")
      removeClass("col_pride", "black")
      addClass("col_pride", "blue")
    }
    else {
      removeClass("col_pride", "blue")
      removeClass("col_pride", "black")
      addClass("col_pride", "black")
    }
    
    paste(acc)
  })
  
  output$emotion_pri_rel <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    acc <- "Pride / Relief"
    addClass("col_pride_rel", "big")
    addClass("col_pride_rel", "indent")
    if(s_data[,"feq_class"] == "Pride/Relief"){
      removeClass("col_pride_rel", "blue")
      removeClass("col_pride_rel", "black")
      addClass("col_pride_rel", "blue")
    }
    else {
      removeClass("col_pride_rel", "blue")
      removeClass("col_pride_rel", "black")
      addClass("col_pride_rel", "black")
    }
    
    paste(acc)
  }
  )
  
  output$emotion_rel_shame <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    acc <- "Relief / Shame"
    addClass("col_rel_shame", "big")
    addClass("col_rel_shame", "indent")
    if(s_data[,"feq_class"] == "Shame/Relief"){
      removeClass("col_rel_shame", "blue")
      removeClass("col_rel_shame", "black")
      addClass("col_rel_shame", "blue")
    }
    else {
      removeClass("col_rel_shame", "blue")
      removeClass("col_rel_shame", "black")
      addClass("col_rel_shame", "black")
    }
    
    paste(acc)
  }
  )
  
  output$result_feedback <- renderUI({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    diagnosis_correctness <- s_data[,24]
    perceived_difficulty <- s_data[,23]
    acc <- ""
    acc1 <- ""
    acc2<- ""
    if (diagnosis_correctness == "1" && (perceived_difficulty == "Challenging" || perceived_difficulty == "Difficult")){
      acc <- "You were correct in your diagnosis! You expressed that this was a challenge/difficult case and you were accurate in your diagnosis decision. Let us examine what next steps you can take to continue learning in the next case."
      acc1 <- "-	I will try to not give up in my next case."
      acc2 <- "-	I will set new goals to challenge myself in my learning."
    }
    if (diagnosis_correctness == "1" && (perceived_difficulty == "Good Review" || perceived_difficulty == "Too Easy")){
      acc <- "You were correct in your diagnosis! You expressed that this was an/a easy/good review and you were accurate in your diagnosis decision. Let us examine what next steps you can take to continue learning in the next case."
      acc1 <- "-	I will continue to try my best with the next case and learn something new"
      acc2 <- "-	I will set new goals to challenge myself in my learning"
    }
    if (diagnosis_correctness == "0" && (perceived_difficulty == "Challenging" || perceived_difficulty == "Difficult")){
      acc <- "For this case, you were incorrect in your diagnosis. You thought that the case was difficulty and I'm glad you continued with the diagnosis process. It's not easy but let us examine what next steps you can take to improve your experience with the next case."
      acc1 <- "-  I can try to not be discouraged when a task is difficult"
      acc2 <- "-	I will examine all types of evidences and try to learn from this experience"
    }
    if (diagnosis_correctness == "0" && (perceived_difficulty == "Good Review" || perceived_difficulty == "Too Easy")){
      acc <- "For this case, you were incorrect in your diagnosis. Let us examine what next steps you can take to bridge this discrepancy in your perception and diagnosis outcome."
      acc1 <- "-	I can set goals specific to evaluating the evidence"
      acc2 <- "-	I can spend more time reviewing the information to make a confident diagnostic decision"
    }
    HTML(paste(acc, acc1, acc2, sep = '<br/>'))
  })
  
  output$s_feedback_time <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_total_feedback_time <- s_data[,29]
    paste(s_total_feedback_time, "seconds")
  })
  
  output$s_feedback_accept <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_total_feedback_accept <- s_data[,30]
    paste(s_total_feedback_accept, "times")
  })
  
  output$s_feedback_modify <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_total_feedback_modify <- s_data[,31]
    paste(s_total_feedback_modify, "times")
  })
  
  output$s_feedback_reject <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep(input$caseselect, s_data$case), ]
    s_total_feedback_reject <- s_data[,32]
    paste(s_total_feedback_reject, "times")
  })
  
  
  #=====COMPARE=====
  output$compare_areas <- renderPlot({
    
    # areas <- data[grep(input$userselect, data$user), ][,1:6]#include cynthia
    areas <- data[grep(input$userselect, data$user), ][2:3,1:6]
    
    areasdf <- areas[,c("case",
                        "time_interval_area_chart", 
                        "time_interval_area_history", 
                        "time_interval_area_library", 
                        "time_interval_area_summarize")]
    areasdf <- (rename(areasdf, c("case"="Case", 
                                  'time_interval_area_chart' ="Chart", 
                                  'time_interval_area_history'="History",
                                  'time_interval_area_library'="Library", 
                                  'time_interval_area_summarize'="Summarize")))
    
    # areasdf$Case <- factor(areasdf$Case, levels = areasdf$Case[c(3,1,2)])#include cynthia
    areasdf$Case <- factor(areasdf$Case, levels = areasdf$Case[c(2,1)])
    
    
    dfm <- melt(areasdf, id.vars = 1)
    
    ggplot(dfm,aes(x = variable,y = value)) + 
      geom_bar(aes(fill = Case),stat = "identity",position = "dodge") +
      theme(plot.background = element_rect(fill = "transparent", color = NULL, inherit.blank = TRUE),
            panel.background = element_blank(),
            panel.grid.major.x = element_blank()
      ) +
      xlab("Area") + 
      ylab("Time in seconds") + 
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14, family = 'Helvetica')) +
      scale_fill_manual(values=c("#80CFB7", "#DFE075", "#F5A3A3"))
    
  }, 
  bg="transparent")
  
  
  output$compare_actions <- renderPlot({
    
    # areas <- data[grep(input$userselect, data$user), ]
    areas <- data[grep(input$userselect, data$user), ][2:3,]
    
    areasdf <- areas[,c("case",
                        "time_interval_linking_evidence", 
                        "time_interval_adding_evidence", 
                        "time_interval_unlinking_evidence", 
                        "time_interval_lab_test",
                        "time_interval_categorize",
                        "time_interval_change_hypothesis",
                        "time_interval_prioritize",
                        "time_interval_delete_hypothesis",
                        "time_interval_select_hypothesis",
                        "time_interval_check_summary",
                        "time_interval_help")]
    areasdf <- (rename(areasdf, c("case" = "Case",
                                   "time_interval_linking_evidence" = 'Link evidence', 
                                   "time_interval_adding_evidence" = 'Add evidence', 
                                   "time_interval_unlinking_evidence" = 'Unlink evidence', 
                                   "time_interval_lab_test" = 'Lab tests',
                                   "time_interval_categorize" = 'Categorize',
                                   "time_interval_change_hypothesis" = 'Change hypothesis',
                                   "time_interval_prioritize" = 'Prioritize',
                                   "time_interval_delete_hypothesis" = 'Delete hypothesis',
                                   "time_interval_select_hypothesis"= 'Select hypothesis',
                                   "time_interval_check_summary" = 'Check summary',
                                   "time_interval_help" = 'Help')
                       ))
    
    # areasdf$Case <- factor(areasdf$Case, levels = areasdf$Case[c(3,1,2)]) #include cynthia
    areasdf$Case <- factor(areasdf$Case, levels = areasdf$Case[c(2,1)])
    
    
    dfm <- melt(areasdf, id.vars = 1)
    
    ggplot(dfm,aes(x = variable,y = value)) + 
      geom_bar(aes(fill = Case),stat = "identity",position = "dodge") +
      theme(plot.background = element_rect(fill = "transparent", color = NULL, inherit.blank = TRUE),
            panel.background = element_blank(),
            panel.grid.major.x = element_blank()
      ) +
      xlab("Action") + 
      ylab("Time in seconds") + 
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14, family = 'Helvetica')) +
      scale_fill_manual(values=c("#80CFB7", "#DFE075", "#F5A3A3")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  }, 
  bg="transparent")
  
  
  #lab tests
  output$compare_lab_tests <- renderText({
    labtests <- data[grep(input$userselect, data$user), ][,21]
    s <- labtests[3]
    c <- labtests[2]
    acc <- ""
    acc <- paste(s, "vs", c)
    })
  
  
  blank_theme_1 <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank()
      # plot.title=element_text(size=14, face="bold")
    )
  
  
  output$pie_compare_evidence <- renderPlot({
    s_data <- data[grep(input$userselect, data$user), ]
    
    s_data_1 <- s_data[grep("Susan", s_data$case), ]
    correct_1 <- s_data_1[,20]
    s_data_evidence_1 <- data.frame(case = c("Susan","Susan"), type = c("% Evidence relates to correct diagnosis","% Evidence does not relate to correct diagnosis"), percentage = c(correct_1, 100-correct_1))
    
    s_data_2 <- s_data[grep("Cynthia", s_data$case), ]
    correct_2 <- s_data_2[,20]
    s_data_evidence_2 <- data.frame(case = c("Cynthia", "Cynthia"), type = c("% Evidence relates to correct diagnosis","% Evidence does not relate to correct diagnosis"), percentage = c(correct_2, 100-correct_2))
    
    evidence <- rbind(s_data_evidence_1, s_data_evidence_2)
    
    ggplot(evidence, aes(x= case, y = percentage, label = percentage, fill= factor(type))) +
      geom_bar(width = 0.5, stat = "identity") +
      blank_theme_1 +
      # theme(axis.text.x=element_blank()) +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_blank()) +
      theme(axis.text=element_text(size=12, face = "bold")) +
      # ggtitle("Comparison for percentage of Evidence Correct") +
      theme(plot.title = element_text(vjust = 0.1, size = 18)) +
      geom_text(size = 6, vjust = 1) +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16)) +
      theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
      ylab("Percentage") 
    # + theme(legend.title = element_text(size=10), legend.text=element_text(size=9))
    # panel.background = element_rect(fill = "#FBFBFB", color = NULL, inherit.blank = TRUE),
  },
  bg="transparent")
  
  output$pie_compare_confidence <- renderPlot({
    s_data <- data[grep(input$userselect, data$user), ]
    
    s_data_1 <- s_data[grep("Susan", s_data$case), ]
    confident_1 <- s_data_1[,22]
    s_data_confidence_1 <- data.frame(case = c("Susan","Susan"), type = c("% Confident in hypothesis","% Lack of confidence in hypothesis"), percentage = c(confident_1, 100-confident_1))
    
    s_data_2 <- s_data[grep("Cynthia", s_data$case), ]
    confident_2 <- s_data_2[,22]
    s_data_confidence_2 <- data.frame(case = c("Cynthia", "Cynthia"), type = c("% Confident in hypothesis","% Lack of confidence in hypothesis"), percentage = c(confident_2, 100-confident_2))
    
    confidence <- rbind(s_data_confidence_2, s_data_confidence_1)
    
    ggplot(confidence, aes(x= case, y = percentage, label = percentage, fill= factor(type))) +
      geom_bar(width = 0.5, stat = "identity") +
      blank_theme_1 +
      # theme(axis.text.x=element_blank()) +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_blank()) +
      theme(axis.text=element_text(size=12, face = "bold")) +
      # ggtitle("Comparison for percentage of Evidence Correct") +
      theme(plot.title = element_text(vjust = 0.1, size = 18)) +
      geom_text(size = 6, vjust = 4) +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16)) +
      theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
      ylab("Percentage") +
      scale_fill_manual(values=c("#00BFC4", "#F7756C", "#00BFC4"))
  },
  bg="transparent") 
  
  
  #compare emotions
  output$compare_emotions <- renderText({
    s_data <- data[grep(input$userselect, data$user), ][,28][c(3,2)]
    acc <- ""
    acc <- paste(s_data[1], "vs", s_data[2])
    })
  
  #compare difficulty
  output$compare_difficulty <- renderText({
    s_data <- data[grep(input$userselect, data$user), ][,23][c(3,2)]
    acc <- ""
    acc <- paste(s_data[1], "vs", s_data[2])
  })
  
  #compare diagnosis
  output$compare_diagnosis <- renderText({
    s_data <- data[grep(input$userselect, data$user), ][,24][c(3,2)]
    s <- ""
    c <- ""
    
    if (s_data[1] == 1) {
      s <- "Correct"
    } else {
      s <- "Incorrect"
    }
    
    if (s_data[2] == 1) {
      c <- "Correct"
    } else {
      c <- "Incorrect"
    }
    
    acc <- ""
    acc <- paste(s, "vs", c)
  })
  
  
  output$s_feed_time <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep("Susan", s_data$case), ]
    s_t <- s_data[,29]
    mins <- s_t %/% 60
    remainder <- s_t %% 60
    paste(mins, "minutes ", remainder, "seconds")
  })
  
  output$c_feed_time <- renderText({
    s_data <- data[grep(input$userselect, data$user), ]
    s_data <- s_data[grep("Cynthia", s_data$case), ]
    s_t <- s_data[,29]
    mins <- s_t %/% 60
    remainder <- s_t %% 60
    paste(mins, "minutes ", remainder, "seconds")
  })
  
  output$com_accept <- renderText({
    sub_data1 <- data[grep(input$userselect, data$user), ]
    sub_data2 <- data[grep(input$userselect, data$user), ]
    s_data <- sub_data1[grep("Susan", sub_data1$case), ]
    c_data <- sub_data2[grep("Cynthia", sub_data2$case), ]
    s_accept <- as.numeric(s_data[,30])
    c_accept <- as.numeric(c_data[,30])
    acc <- ""
    if(c_accept < s_accept){
      acc <- "LESS than in the Susan case"
    }
    if(c_accept > s_accept){
      acc <- "MORE than in the Susan case"
    }
    if(c_accept == s_accept){
      acc <- "THE SAME as in the Susan case"
    }
    paste(acc)
  })
  
  
  output$com_accept <- renderText({
    sub_data1 <- data[grep(input$userselect, data$user), ]
    sub_data2 <- data[grep(input$userselect, data$user), ]
    s_data <- sub_data1[grep("Susan", sub_data1$case), ]
    c_data <- sub_data2[grep("Cynthia", sub_data2$case), ]
    s_accept <- as.numeric(s_data[,30])
    c_accept <- as.numeric(c_data[,30])
    acc <- ""
    if(c_accept < s_accept){
      acc <- "LESS than in the Susan case"
    }
    if(c_accept > s_accept){
      acc <- "MORE than in the Susan case"
    }
    if(c_accept == s_accept){
      acc <- "THE SAME as in the Susan case"
    }
    paste(acc)
  })
  
  
  output$com_mod <- renderText({
    sub_data1 <- data[grep(input$userselect, data$user), ]
    sub_data2 <- data[grep(input$userselect, data$user), ]
    s_data <- sub_data1[grep("Susan", sub_data1$case), ]
    c_data <- sub_data2[grep("Cynthia", sub_data2$case), ]
    s_mod <- as.numeric(s_data[,31])
    c_mod <- as.numeric(c_data[,31])
    acc <- ""
    if(c_mod < s_mod){
      acc <- "LESS than in the Susan case"
    }
    if(c_mod > s_mod){
      acc <- "MORE than in the Susan case"
    }
    if(c_mod == s_mod){
      acc <- "THE SAME as in the Susan case"
    }
    paste(acc)
  })
  
  output$com_reject <- renderText({
    sub_data1 <- data[grep(input$userselect, data$user), ]
    sub_data2 <- data[grep(input$userselect, data$user), ]
    s_data <- sub_data1[grep("Susan", sub_data1$case), ]
    c_data <- sub_data2[grep("Cynthia", sub_data2$case), ]
    s_reject <- as.numeric(s_data[,32])
    c_reject <- as.numeric(c_data[,32])
    acc <- ""
    if(c_reject < s_reject){
      acc <- "LESS than in the Susan case"
    }
    if(c_reject > s_reject){
      acc <- "MORE than in the Susan case"
    }
    if(c_reject == s_reject){
      acc <- "THE SAME as in the Susan case"
    }
    paste(acc)
  })
  
}