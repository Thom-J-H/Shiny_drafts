library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(DT)

#####Import Data
dat <- read_csv("cces_sample_coursera.csv")
dat <- dat %>% 
  select(c("pid7","ideo5","newsint","gender","educ","CC18_308a","region"))
dat <- drop_na(dat)
dat <- dat %>%  mutate(gender2 = case_when(gender == 1 ~ "Male",
                                           gender == 2 ~ "Female"))

dat <- dat %>%  mutate(id_lab = case_when(ideo5 == 1 ~ "Very liberal",
                                          ideo5 == 2 ~ "Liberal",
                                          ideo5 == 3 ~ "Moderate",
                                          ideo5 == 4 ~ "Conservative",
                                          ideo5 == 5 ~ "Very conservative"))
x_ticks <- 1:7
y_ticks <- 1:7
x_tix2 <- 1:4
edu_tx <- 1:6


ui <- navbarPage("Second Assignment", 
                 theme = shinytheme("sandstone"),
                tabPanel("Ideology",
                         sidebarPanel(
                           sliderInput(inputId = "ideo_point",
                                       label = "Select Political Ideology Cohort",
                                       min = 1,
                                       max = 5,
                                       value = 3,
                                       step = 1),
              
                           HTML("<p><strong>Coded as follows from the survey:</strong></p>
                                <ol>
                                <li>Very liberal</li>
                                <li>Liberal</li>
                                <li>Moderate</li>
                                <li>Conservative</li>
                                <li>Very conservative</li>
                                </ol>"),
                           HTML("<p><small>Data sample from <a href='https://cces.gov.harvard.edu/'>CESS</a> & Collin Paschall.  Please see <strong>Notes</strong>.</small></p>")
                         ),
                         
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Self-Described Party Affiliation", 
                                      plotOutput("party_aff") ),
                             tabPanel("Donald Trump's Performance",
                                      plotOutput("trump"))
                           ) # clost tabset
                         ) # close main
                         ), # close tabpanel #`1`
                
                tabPanel("Gender",
                         sidebarPanel(
                           checkboxGroupInput("gender", "Select Gender:",
                                              choices = c("Male" = 1, 
                                                          "Female" = 2), 
                                              selected = 2),
                           HTML(" <p><small>Data sample from <a href='https://cces.gov.harvard.edu/'>CESS</a> & Collin Paschall. Please see <strong>Notes</strong>.</small></p>")
                         ),
                         mainPanel(
                           tags$style(type = "text/css", 
                                              ".shiny-output-error { visibility: hidden; }", 
                                              ".shiny-output-error:before { visibility: hidden; }" ),
                           plotlyOutput('gender_ed') )
                         ),
                
                tabPanel("Raw Data",
                         sidebarPanel(
                           selectInput(
                             "reg", "Region:", choices =c(1:4),
                             multiple = TRUE
                           ),
                           HTML("<strong>Region Codes:</strong> 
                                <ol>
                                <li>Northwest</li>
                                <li>Midwest</li>
                                <li>South</li>
                                <li>West</li>
                                </ol>
                                <p><small>Data sample from <a href='https://cces.gov.harvard.edu/'>CESS</a> & Collin Paschall.  Please see <strong>Notes</strong> for a complete code book to this data subset.  Thank you.</small></p>")
                         ),
                         mainPanel(dataTableOutput("rawd", height = 500))
                         ),
                tabPanel("Notes",
                         column(7, 
                               h3("Code Book"),
    
                               HTML("This project uses a selection of data from the <a href='https://cces.gov.harvard.edu/'>Cooperative Election Study</a>
                                    as supplied by my instructor Collin Paschall. The code book for data used as follows: &nbsp; <br />&nbsp; "),
                                    
                               
                               tabsetPanel(type = "tabs",
                                           tabPanel("ideo5", 
                                                    HTML("&nbsp;<br /> 
                                                    <strong>ideo5 ~</strong> Ideology:
                                <ol>
                                <li>Very liberal</li>
                                <li>Liberal</li>
                                <li>Moderate</li>
                                <li>Conservative</li>
                                <li>Very conservative</li>
                                </ol>")),
                                           tabPanel("pid7", 
                                                    HTML("&nbsp;<br /> 
                                                    <strong>pid7</strong> ~ Party Affiliation:
                                <ol>
                                <li>Strong Democrat</li>
                                <li>Not very strong Democrat</li>
                                <li>Lean Democrat</li>
                                <li>Independent</li>
                                <li>Lean Republican</li>
                                <li>Not very strong Republican</li>
                                <li>Strong Republican</li>
                                </ol>")),
                                           tabPanel("region",
                                                    HTML("&nbsp;<br />  
                                                    <strong>region</strong>  ~ Region:
                                <ol>
                                <li>Northwest</li>
                                <li>Midwest</li>
                                <li>South</li>
                                <li>West</li>
                                </ol>")),
                                           tabPanel("educ",
                                                    HTML("&nbsp;<br />  
                                                    <strong>educ</strong> ~ Education Level:
                                <ol>
                                <li>No high school</li>
                                <li>High school graduate</li>
                                <li>Some college</li>
                                <li>2-year</li>
                                <li>4-year</li>
                                <li>Post-grad</li>
                                </ol>")),
                                           tabPanel("CC18_308a",
                                                    HTML("&nbsp;<br />
                                                    <strong>CC18_308a</strong>  ~ Job approval – President Trump:
                                <ol>
                                <li>Strongly approve</li>
                                <li>Somewhat approve</li>
                                <li>Somewhat disapprove</li>
                                <li>Strongly disapprove</li>
                                </ol>  ")),
                                           tabPanel("gender",
                                                    HTML("&nbsp;<br /> 
                                                    <strong>gender</strong>  ~ Gender:
                                <ol>
                                <li>Male</li>
                                <li>Female</li>
                                </ol>")),
                                           tabPanel('newsint',
                                                    HTML("&nbsp;<br /> 
                                                    <strong>newsint</strong>  ~ News Interest:
                                <ol>
                                <li>Most of the time</li>
                                <li>Some of the time</li>
                                <li>Only now and then</li>
                                <li>Hardly at all</li>
                                </ol>"))
                                    
                                    
                                    ) ),
                         column(4, offset = 1,
                                h3("About & Extras"),
                                HTML("<p>This assignment was submitted by TJH  on 15 December 2021 for <a href ='https://www.coursera.org/learn/data-viz-shiny-dashboards/home/welcome'>Publishing Viz in R w/ Shiny</a> by Collin Paschall @ Coursera. So far, very useful. I'd rate the specialization 5 &#9733&#9733&#9733&#9733&#9733 out of 5. Thank you, Collin.</p>
                                     <p>My version of the assignment includes a few extras.  I used the library <a href ='https://github.com/tidyverse/glue'>glue</a> to dynamically change the titles (text output) for the ggplots.  If you don't know <a href ='https://github.com/tidyverse/glue'>glue</a>, I'd recommend checking it out.</p>
                                     <p>I also added color to all visualizations when relevant to make them more readable, and I customized the X and Y axes for similar reasons. It's just an assignment -- but good habits are good habits.</p>
                                     <p>Thank you for reviewing!</p>")) 
                         )
)
  
  
  server<- function(input,output){
    
    output$party_aff <- renderPlot({
      
      dat %>% 
        filter(ideo5 == input$ideo_point) %>% 
        ggplot( aes(x =pid7, y = ideo5, fill = pid7)) + 
        geom_bar(stat="identity") +
        scale_fill_continuous(type = "gradient",
                              low="blue",  high="red") +
        theme(legend.position = "none") +
        labs(fill = "Party \nAffiliation", 
             y = "Count",
             x = "From Strong Democrat to Strong Republican: Party Affiliation",
             title = paste0("Ideology Cohort #", {input$ideo_point}, ": Self-Described Party Affiliation")) +
        scale_x_continuous(breaks = x_ticks,
                           labels=c("Strong\nDemocrat", "Moderate\nDemocrat", 
                                    "Lean\nDemocrat", "Independent", "Lean\nRepublican", 
                                    "Moderate\nRepublican", "Strong\nRepublican" ) )
      
    }, res = 96)

   output$trump <- renderPlot({ 
     dat %>%
       filter(ideo5 == input$ideo_point) %>% 
       ggplot( aes(x = CC18_308a, y = pid7 , fill = CC18_308a)) +
       geom_bar(stat="identity") +
       labs(fill = "", 
            y = "Count",
            x = "Trump's Performance: Strongly Approve to Strongly Disapprove",
            title = paste0("Ideology Cohort #", {input$ideo_point},": Donald Trump's Performance ") ) +
       scale_x_continuous(breaks = x_tix2,
                          labels=c("Strongly\nApprove", "Somewhat\nApprove",
                                   "Somewhat\nDisapprove", "Strongly\nDispprove") ) +
       scale_fill_viridis_c() +
       guides(fill = "none")
     }, res = 96 )
   
   output$gender_ed<- renderPlotly({
     
     draft <- dat %>%
       filter(gender %in% input$gender) %>%
       ggplot( aes(x = educ, y = pid7, color = gender2) ) +
       geom_jitter(alpha = 0.7) +
       geom_smooth(method = "lm") +
       scale_color_manual(values = c("darkblue", "green")) +
       guides(color = "none") +
       scale_y_continuous(breaks = x_ticks,
                          labels=c("Strong Dem", "Mod. Dem", 
                                   "Lean Dem", "Indy", "Lean Rep", 
                                   "Mod. Rep", "Strong Rep" )) +
       scale_x_continuous(breaks = edu_tx, labels = c("No high\nschool", "HS grad", 
                                                      "Some\nCollege", "Two-Year\ngrad",
                                                      "Four-Year\ngrad", "Post-grad") ) +
       theme(axis.text.x = element_text(angle = 45, vjust = 0.4, hjust = 0.2)) +
       labs(x = "Level of Education", y = "Party Affiliation",
            title = "Party Affiliation ~ Education Level") +
       guides(color = "none")
     
      ggplotly(draft) %>% hide_guides()
     
   })
   
   output$rawd <- DT::renderDataTable({ 
     dat %>%  
       select(-c(gender2, id_lab)) %>% 
       filter(region %in% input$reg)
     

                                     })

    
  } 

shinyApp(ui,server)

