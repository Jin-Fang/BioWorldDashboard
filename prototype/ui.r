library(shiny)
library(shinydashboard)
library(shinyjs)
library(readr)

data <- read.csv("bioworld_data.csv")

user_list <- sort(unique(data$user))
case_list <- unique(data$case)[c(3,1,2)]

shinyUI(fluidPage(
  
  #use shinyJs
  useShinyjs(),
  
  #CSS instructions for colour classes
  inlineCSS(list(.black   = "color: #B3C4C9",
                 .blue = "color: #25af98",
                 .big = "font-size: 18px",
                 .indent = "text-indent: 50px",
                 # .too_easy = "color: #5F9D05",
                 # .good_review = "color: #98CB00",
                 # .challenging = "color: #FFA900",
                 # .difficult = "color: #BA0700",
                 .too_easy = "color: #25af98",
                 .good_review = "color: #25af98",
                 .challenging = "color: #25af98",
                 .difficult = "color: #25af98",
                 .font = "font-family: Hiragino Kaku Gothic Pro, Helvetica"
  )),
  
  
  #new UI layout below
  dashboardPage(
    
    dashboardHeader(title = "YOUR BIOWORLD DASHBOARD", titleWidth = "300px"),
    
    dashboardSidebar(
      width = "300px",
      sidebarMenu(
        menuItem("WELCOME", tabName = "welcome",
                 icon = icon("hand-peace-o")),
        menuItem("SET YOUR GOALS", tabName = "goal_setting",
                 icon = icon("bullseye")),
        menuItem("ABOUT YOU AS A LEARNER", tabName = "goal_orientation",
                 icon = icon("bullseye")),
        # menuItem("Select user and case", tabName = "select_user_and_case", 
        #          icon = icon("address-card")),
        menuItem("VIEW YOUR CASE REPORTS", tabName = "view_cases_report",
                 icon = icon("area-chart")),
        menuItem("COMPARE YOUR CASE REPORTS", tabName = "compare_cases",
                 icon = icon("area-chart")),
        selectInput("userselect", "USER:", choice = user_list),
        selectInput("caseselect", "CASE:", choice = case_list)
      )
    ),
    dashboardBody(
      # theme
      includeCSS("www/stuff.css"),
      
      tabItems(
        # welcome page
        tabItem(tabName = "welcome",
                h3("Welcome to Your Learner Profile Dashboard!", style = "color: #25af98"),
                p("This is your personal learner dashboard.", style = "font-size: 16px"),
                p("We will be presenting you with some information about your learning experience while working through the BioWorld Cases.", style = "font-size: 16px"),
                p("Please visit the dashboard after each case to view your updated learner profile.", style = "font-size:16px"), 
                tags$p(""),
                br()
        ),
        
        # select user and case page
        tabItem(tabName = "select_user_and_case",
                selectInput("userselect", "Please select a user", choice = user_list),
                br(),
                textOutput("text"),
                tableOutput("test"),
                br(),
                selectInput("caseselect", "Please select a case you want to view", choice = case_list),
                textOutput("text1"),
                tableOutput("test1"),
                br()
        ),
        
        # goal setting page
        tabItem(tabName = "goal_setting",
                p("Setting goals can help with your performance."),
                checkboxGroupInput("checkGroup", 
                                   label = h5("Select the top three goals you would like to focus on while working through the BioWorld cases:"),
                                   choices = list("Correctly diagnose the case" = 1, 
                                                  "Order an appropriate number of lab tests" = 2, 
                                                  "Select an appropriate amount of evidence" = 3,
                                                  "Spend an appropriate amount of time on the feedback provided after the case is done" = 4,
                                                  "Ask for an appropriate number of consultations" = 5,
                                                  "Visit the Library for more research about the case" = 6,
                                                  "Generate more hypotheses" = 7),width = "100%"),
                actionButton("Submit", "Submit"),
                p(""),
                br(),
                p("What are some steps you would like to take to reach these goals?"),
                textInput("fake_text_goal", 
                          label = NULL,
                          value = "",
                          width = "100%"),
                actionButton("Submit", "Submit"),
                p(""),
                br(),
                h5("Here are some ideas to help you: "),
                p("1. Correctly diagnose the case", style = "text-indent: 5px;"),
                p("-	Take some more time to review the patient history (Problem)", style = "text-indent: 30px;"),
                p("-	Order a wide selection of lab tests to narrow down the specific diagnosis", style = "text-indent: 30px;"),
                p("-	Examine the evidence again before I make the final diagnosis", style = "text-indent: 30px;"),
                
                p("2. Order an appropriate number of lab tests", style = "text-indent: 5px;"),
                p("-	Only order the tests that I consider to be useful for the diagnosis", style = "text-indent: 30px;"),
                p("-	Review the most probable hypothesis to identify which lab tests to select", style = "text-indent: 30px;"),
                
                p("3. Select an appropriate amount of evidence", style = "text-indent: 5px;"),
                p("-	Revise number of evidences selected after rating probable hypotheses", style = "text-indent: 30px;"),
                p("-	Select only differentiating pieces of evidences ", style = "text-indent: 30px;"),
                p("-	Review the problem again ", style = "text-indent: 30px;"),
                
                p("4. Spend an appropriate amount of time on the feedback provided after the case is done", style = "text-indent: 5px;"),
                p("-	Revise number of evidences selected after rating probable hypotheses", style = "text-indent: 30px;"),
                p("-	Select only differentiating pieces of evidences ", style = "text-indent: 30px;"),
                p("-	Review the problem again ", style = "text-indent: 30px;"),
                
                p("5. Ask for an appropriate number of consultations", style = "text-indent: 5px;"),
                p("-  Seek help immediately when unsure about diagnosis decision", style = "text-indent: 30px;"),
                
                p("6. Visit the Library for more research about the case, if necessary", style = "text-indent: 5px;"),
                p("- Look up unfamiliar words for better contextual understanding", style = "text-indent: 30px;"),
                p("- Review similar cases to compare evidence required to make diagnosis", style = "text-indent: 30px;"),
                
                p("7. Generate more hypotheses", style = "text-indent: 5px;"),
                p("- Identify all possible hypotheses related to the selected evidences", style = "text-indent: 30px;"),
                p("- Rank order the selected hypotheses", style = "text-indent: 30px;")
                ),
        
        # goal orientation page
        tabItem(tabName = "goal_orientation",
                p("Based on the questionnaire you filled out prior to starting any cases on BioWorld, it appears that your performance orientation is:"),
                span(id = "goal1", textOutput("goal_orientation_1")),
                span(id = "goal2", textOutput("goal_orientation_2")),
                span(id = "goal3", textOutput("goal_orientation_3")),
                br(),
                
                span(textOutput("learner1.1"),style = "font-size:17px; color: #25af98"),
                textOutput("learner1"),
                span(textOutput("learner2.1"),style = "font-size:17px; color: #25af98"),
                textOutput("learner2"),
                span(textOutput("learner3.1"),style = "font-size:17px; color: #25af98"),
                textOutput("learner3"),
                br(),
                
                h5("Does this descriptive accurately describe your learning preference?"),
                selectInput("accuracy_select", 
                            NULL, 
                            choice = c("Not accurate at all",
                                       "Somewhat accurate",
                                       "Accurate",
                                       "Very accurate")
                ),
                br(),
                h5("How do you think this will influence your decision of the goals you will set?"),
                selectInput("influence_select", 
                            NULL, 
                            choice = c("I will set goals that will help me to understand the information I'm not familiar with",
                                       "I will set goals that help me to get the diagnosis correct",
                                       "I will set goals that will help me to look smart",
                                       "I will set goals that will help me to not be stuck on the diagnosis"),
                            width = "100%"
                ),
                br(),
                h5("What steps will you take to achieve your goals?"),
                selectInput("achieve_select", 
                            NULL, 
                            choice = c("I will choose tasks that I know are difficult, but will help me to learn",
                                       "I will stop thinking about my diagnosis outcome and focus on understanding the material",
                                       "I will monitor my own progress and try not to compare to others"),
                            width = "100%"
                ),
                br(),
                actionButton("Submit", "Submit"),
                br()
                ),
        
        # view cases report page
        tabItem(tabName = "view_cases_report",
                tabsetPanel(
                  tabPanel("Time Spent",
                           h4("Total Time Spent"),
                           p("Each area allowed you to gather evidence, organize information, conduct research, and synthesize a diagnostic decision."),
                           p("Here is your total time spent within BioWorld during your diagnosis of this case: "),
                           span(textOutput("s_total_time"), style = "color:#25af98; font-size:18px; text-indent: 50px"),
                           br(),
                           
                           h4("Time Spent in Areas"),
                           p("Here is how much time you spent in each of the following areas within BioWorld during Case 1:"),
                           plotOutput("s_area"),
                           br(),
                           span(textOutput("most_area"), style = "color:#25af98; font-size:17px;
                                font-family: 'Hiragino Kaku Gothic Pro', Helvetica, sans-serif;"),
                           textOutput("most_area_1"),
                           br(),
                           span(textOutput("least_area"), style = "color:#25af98; font-size:17px;
                                font-family: 'Hiragino Kaku Gothic Pro', Helvetica, sans-serif;"),
                           textOutput("least_area_1"),
                           br(),
                           h4("Reflection"),
                           p("Are these descriptions accurate in describing your actions in Bioworld?"),
                           selectInput("area_accuracy", 
                                       NULL, 
                                       choice = c("Not accurate at all",
                                                  "Somewhat accurate",
                                                  "Accurate",
                                                  "Very accurate")
                           ),
                           p("Would you like to focus on spending more time in another area?"),
                           selectInput("another_area", 
                                       NULL, 
                                       choice = c("Yes","No")
                           ),
                           p("If Yes, which area would you like to focus on?"),
                           selectInput("area_focus", 
                                       NULL, 
                                       choice = c("History","Chart","Library","Summarize")
                           ),
                           actionButton("Submit", "Submit"),
                           p(""),
                           br(),
                           h4("Time Spent on Actions"),
                           p("Here is how much time you spent on the following actions while trying to diagnose the patient in Case 1:"),
                           plotOutput("s_action"),
                           br(),
                           h4("Reflection"),
                           p("Was there any particular evidence that made you certain you would be matched with the expert solution?"),
                           textInput("match_expert", 
                                     label = NULL, 
                                     value = "",
                                     width = "100%"),
                           p("How do you think you could improve on the next case with selecting appropriate evidence?"),
                           p("Here are some suggestions: ", style = "text-indent: 5px;"),
                           p("-	Evaluate my specific goals for the next case to focus on selecting evidence", style = "text-indent: 30px;"),
                           p("-	Spending more time on reading the patient history ", style = "text-indent: 30px;"),
                           p("-	Selecting more lab tests to identify which unrelated evidences to unlink", style = "text-indent: 30px;"),
                           textInput("evidence_improve", 
                                     label = NULL, 
                                     value = "",
                                     width = "100%"),
                           actionButton("Submit", "Submit"),
                           p(""),
                           br()
                  ),
                  
                  tabPanel("Evidence + Lab Tests", 
                           br(),
                           h4("Percentage of Evidence Correct"),
                           plotOutput("s_evidence"),
                           h4("Lab Tests"),
                           p("In this case, the number of lab tests you have ordered is:"),
                           textOutput("s_labtests"),
                           tags$head(tags$style({"#s_labtests{text-indent: 50px;
                             font-size: 16px;
                             color: #25af98;
                             font-family: Hiragino Kaku Gothic Pro, Helvetica}"
                             })),
                           br(),
                           p("Based on your observation of the evidence, you have selected a variety of lab tests to justify and support your diagnosis. "),
                           p("Do you think all of these lab tests were necessary to make your diagnostic decision?"),
                           selectInput("labtest_required", 
                                       NULL, 
                                       choice = c("Yes","No")
                           ),
                           p("If No, which lab tests would you have excluded?"),
                           textInput("change_labtest", 
                                     label = NULL, 
                                     value = "",
                                     width = "100%"),
                           actionButton("Submit", "Submit"),
                           p(""),
                           br()
                  ),
                  
                  tabPanel("Review",
                           br(),
                           h4("Confidence in Hypothesis"),
                           plotOutput("s_confidence"),
                           h4("Perceived Difficulty"),
                           p("You have indicated that you found this case to be:"),
                           
                           span(id = "s_diff_easy_col", textOutput("s_diff_easy")),
                           span(id = "s_diff_good_col", textOutput("s_diff_good")),
                           span(id = "s_diff_chal_col", textOutput("s_diff_chal")),
                           span(id = "s_diff_diff_col", textOutput("s_diff_diff")),
                           # span(id = "s_difficulty_col", textOutput("s_difficulty")),
                           br(),
                           p("Your confidence in your final hypothesis and perceived difficulty of the case are important factors to consider. "),
                           br(),
                           span(p("Why were you confident/not confident in your final hypothesis?"),style = "font-size:14px; font-weight: bold"),
                           p("Here are some ideas: "),
                           p("-	I had strong evidence to indicate that my hypothesis was correct.", style = "text-indent: 30px;"),
                           p("-	Results from the lab tests supported my main hypothesis.", style = "text-indent: 30px;"),
                           p("-	There were many possible evidences that confused me.", style = "text-indent: 30px;"),
                           p("- I was unfamiliar with the symptoms of target disease.", style = "text-indent: 30px;"),
                           textInput("confidence_reason", 
                                     label = NULL, 
                                     value = "",
                                     width = "100%"),
                           actionButton("Submit", "Submit"),
                           p(""),
                           span(p("What will you do next to support your confidence in selecting your hypothesis?"),style = "font-size:14px; font-weight: bold"),
                           p("For example: "),
                           p("-	Set goals related to accruing relevant evidence", style = "text-indent: 30px;"),
                           p("-	Set goals related to ordering the appropriate lab tests", style = "text-indent: 30px;"),
                           p("-	Consult the Library to understand the disease and patient case", style = "text-indent: 30px;"),
                           p("-	Seek for help before selecting and submitting final diagnosis. ", style = "text-indent: 30px;"),
                           textInput("support_confidence", 
                                     label = NULL, 
                                     value = "",
                                     width = "100%"),
                           actionButton("Submit", "Submit"),
                           p(""),
                           br(),
                           h4("Diagnosis"),
                           p("Your diagnosis of the patient was:"),
                           span(id = "s_correctness_col", textOutput("s_correctness"),style = "color:#25af98; font-size:18px"),
                           htmlOutput("result_feedback"),
                           br(),
                           span(p("Can you tell us what part was most challenging for you?"), style = "font-size:14px; font-weight: bold"),
                           textInput("most_challenge", 
                                     label = NULL, 
                                     value = "",
                                     width = "100%"),
                           actionButton("Submit", "Submit"),
                           p(""),
                           span(p("What would you do next if you face another challenging case?"), style = "font-size:14px; font-weight: bold"),
                           p("Here are some suggestions: "),
                           p("-	Select attainable goals to achieve for the next case.", style = "text-indent: 30px;"),
                           p("-	Spend more time reviewing the patient history and ordering appropriate lab tests", style = "text-indent: 30px;"),
                           p("-	Corroborate all evidences and results from lab tests before making final diagnostic decision", style = "text-indent: 30px;"),
                           p("-	Attempt the case with a mindset that it’s a good learning experience", style = "text-indent: 30px;"),
                           textInput("next_challenge", 
                                     label = NULL, 
                                     value = "",
                                     width = "100%"),
                           actionButton("Submit", "Submit"),
                           p(""),
                           br()
                  ),
                  
                  tabPanel("Feedback", 
                           h4("Emotions"),
                           p("Your overall emotions once you received your results were: "),
                           span(id = "col_pride", textOutput("emotion_pride")),
                           span(id = "col_pride_rel", textOutput("emotion_pri_rel")),
                           span(id = "col_rel_shame", textOutput("emotion_rel_shame")),
                           br(),
                           
                           span(textOutput("emotion1.1"),style = "font-size:17px; color: #25af98; 
                                font-family: 'Hiragino Kaku Gothic Pro', Helvetica, sans-serif;"),
                           span(textOutput("emotion1"), style = "
                                font-family: 'Hiragino Kaku Gothic Pro', Helvetica, sans-serif;"),
                           span(textOutput("emotion2.1"),style = "font-size:17px; color: #25af98; 
                                font-family: 'Hiragino Kaku Gothic Pro', Helvetica, sans-serif;"),
                           span(textOutput("emotion2"), style = "
                                font-family: 'Hiragino Kaku Gothic Pro', Helvetica, sans-serif;"),
                           span(textOutput("emotion3.1"),style = "font-size:17px; color: #25af98;
                                font-family: 'Hiragino Kaku Gothic Pro', Helvetica, sans-serif;"),
                           span(textOutput("emotion3"), style ="
                                font-family: 'Hiragino Kaku Gothic Pro', Helvetica, sans-serif;"),
                           br(),
                           
                           h4("Time Spent on Feedback"),
                           p("The total time you spent going through feedback is: "),
                           textOutput("s_feedback_time"),
                           br(),
                           h4("Reflection"),
                           p("Is this an accurate reflection of your response to feedback?"),
                           selectInput("feedback_accuracy", 
                                       NULL, 
                                       choice = c("Not accurate at all",
                                                  "Somewhat accurate",
                                                  "Accurate",
                                                  "Very accurate")
                           ),
                           actionButton("Submit", "Submit"),
                           p(""),
                           br(),
                           h4("Attitudes Towards Feedback"),
                           p("Your attitudes towards the feedback that was being provided to you throughout the case can be viewed as following: "),
                           h5("You've accepted feedback: "),
                           textOutput("s_feedback_accept"),
                           h5("You've modified/manipulated feedback: "),
                           textOutput("s_feedback_modify"),
                           h5("You've rejected feedback: "),
                           textOutput("s_feedback_reject"),
                           tags$head(tags$style({"#s_feedback_time, #s_feedback_accept, #s_feedback_modify, #s_feedback_reject{text-indent: 50px;
                             font-size: 16px;
                             color: #25af98;
                             font-family: Hiragino Kaku Gothic Pro, Helvetica}"
                             }))
                  ),
                  
                  tabPanel("Planning", 
                           checkboxGroupInput("checkGroup_planning", 
                                              label = h5("For your upcoming case, please pick the top three goals you would like to concentrate on:"),
                                              choices = list("Correctly diagnose the case" = 1, 
                                                             "Order an appropriate number of lab tests" = 2, 
                                                             "Ask for an appropriate amount of evidence" = 3,
                                                             "Spend an appropriate amount of time on the feedback provided after the case is done" = 4,
                                                             "Ask for an appropriate number of consultations" = 5,
                                                             "Visit the Library for more research about the case" = 6,
                                                             "Generate more hypotheses" = 7),width = "100%"
                                              ),
                           actionButton("Submit", "Submit"),
                           p(""),
                           br(),
                           h5("Briefly explain why you chose to concentrate on each of the following goals for your next case:"),
                           p("Order an appropriate number of lab tests"),
                           textInput("fake_text_1", 
                                     label = NULL, 
                                     value = "",
                                     width = "100%"),
                           p("Ask for an appropriate amount of evidence"),
                           textInput("fake_text_2", 
                                     label = NULL, 
                                     value = "",
                                     width = "100%"),
                           p("Spend an appropriate amount of time on the feedback provided after the case is done"),
                           textInput("fake_text_3", 
                                     label = NULL, 
                                     value = "",
                                     width = "100%"),
                           actionButton("Submit", "Submit"),
                           p(""),
                           br()
                           )
                  )
                ),
        
        # compare cases page
        tabItem(tabName = "compare_cases",
                h4("Comparison of Time Spent in Areas"),
                plotOutput("compare_areas"),
                p("You spent most of your time in History for both cases. It has been evident that students who spend a lot of time in the History area tend to get the correct diagnosis! "),
                p("You spent the least amount of time in the Library for both cases. This might mean you knew enough about the diagnosis and did not need to spend time looking for more information in the library
"),
                p("You spent 204 more seconds on case number #2. This might be due to the fact that the second case was more challenging, or because you re-aligned your goals to spend more time on solving the cases. "),
                p(""),br(), br(),
                
                h4("Comparison of Time Spent on Actions"),
                plotOutput("compare_actions"),
                p("You spent more time on lab tests in the Cynthia case (Case #2) than you did in Case #1. This might be due to the difficulty of the case, which required you to order more tests. For Case #1 you spent the most time checking your summary, which is usually a good way of reviewing your work prior to submission! Perhaps you can do that for your following cases as well! 
"),
                p(""),br(), br(),
                
                #lab tests
                h4("Number of Lab Tests"),
                p("Susan case vs Cynthia Case", style = "text-indent: 50px"),
                span(textOutput("compare_lab_tests"), style = "
                     font-family: Hiragino Kaku Gothic Pro;
                     font-size: 18px;
                     color: #25af98;
                     text-indent: 50px"),
                p(""),br(), br(),
                
                h4("Comparison of Percentage of Evidence Correct"),
                plotOutput("pie_compare_evidence"),
                p(""),br(),br(),
                
                h4("Comparison of Confidence in Hypothesis"),
                plotOutput("pie_compare_confidence"),
                p("You show higher confidence when solving case #2 despite finding it to be more challenging! That’s great! 
"),
                p(""),br(), br(),
                
                #difficulty
                h4("Comparison of Difficulty and Diagnosis"),
                p("Difficulty: Susan case vs Cynthia Case", style = "text-indent: 50px"),
                span(textOutput("compare_difficulty"), style = "
                     font-family: Hiragino Kaku Gothic Pro;
                     font-size: 18px;
                     color: #25af98;
                     text-indent: 50px"),
                br(),
                #diagnosis
                # h4("Comparison of Diagnosis"),
                p("Diagnosis: Susan case vs Cynthia Case", style = "text-indent: 50px"),
                span(textOutput("compare_diagnosis"), style = "
                     font-family: Hiragino Kaku Gothic Pro;
                     font-size: 18px;
                     color: #25af98;
                     text-indent: 50px"),
                br(),
                p("You correctly diagnosed Case #1 and you also found it to be a Good Review for yourself. Based on that, it makes sense that you showed emotions of Pride and Relief as you were capable of understanding the information and diagnosing the patient correctly. For case #2, you did not correctly diagnose the patient, but that is okay as you did find the case to be challenging. Your emotions of shame and relief might be due to your incorrect diagnosis, but it was a challenging case, so no need to feel ashamed! 
                  "),
                p(""),br(), br(),
                
                #emotions
                h4("Comparison of Emotions"),
                p("Susan case vs Cynthia Case", style = "text-indent: 50px"),
                span(textOutput("compare_emotions"), style = "
                     font-family: Hiragino Kaku Gothic Pro;
                     font-size: 18px;
                     color: #25af98;
                     text-indent: 50px"),
                
                p(""),br(), br(),
                
                #time spent
                h4("Time Spent on Feedback"),
                p("Susan Taylor Case: ", style = "text-indent: 50px"),
                span(textOutput("s_feed_time"),style = "font-size:18px; color: #25af98;
                     text-indent: 50px"),
                br(),
                p("Cynthia Case: ", style = "text-indent: 50px"),
                span(textOutput("c_feed_time"),style = "font-size:18px; color: #25af98;
                     text-indent: 50px"),
                p(""),br(), br(),
                
                #attitude
                h4("Attitude Towards the Feedback"),
                p("In Cynthia Case, you ACCEPTED feedback: ", style = "text-indent: 50px"),
                span(textOutput("com_accept"),style = "font-size:18px; color: #25af98;
                     text-indent: 50px"),
                br(),
                p("In Cynthia Case, you MODIFIED/MANIPULATED feedback: ", style = "text-indent: 50px"),
                span(textOutput("com_mod"),style = "font-size:18px; color: #25af98;
                     text-indent: 50px"),
                br(),
                p("In Cynthia Case, you REJECTED feedback: ", style = "text-indent: 50px"),
                span(textOutput("com_reject"),style = "font-size:18px; color: #25af98;
                     text-indent: 50px"),
                p(""),br(), br()
                
                
        )
      )
    )
  )
)
)