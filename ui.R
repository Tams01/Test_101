#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#getwd()
library(shiny)
library(readxl)
library(glue)
library(reactable)
library(purrr)
library(textdata)
library(plotly)
library(shiny)
library(hwordcloud)
library(tidytext)
library(tm)
library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
#library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
options(spinner.color="#006272")
library(timevis)
library(data.table)
library(lubridate)
#install.packages("WDI")
header <- 
    dashboardHeader( title = HTML("AURA"), 
                     disable = FALSE, 
                     titleWidth  = 230,
                     dropdownMenuCustom( type = 'message',
                                         customSentence = customSentence,
                                         messageItem(
                                             from = "TR_SharedMailbox@mbie.govt.nz",#'Feedback and suggestions',
                                             message =  "",#paste0("TR_SharedMailbox@mbie.govt.nz" ),
                                             icon = icon("envelope"),
                                             href = "mailto:hello@tfw.com.ng"
                                         ),
                                         icon = icon('comment')
                     ),
                     dropdownMenuCustom( type = 'message',
                                         customSentence = customSentence_share,
                                         icon = icon("share-alt"),
                                         messageItem(
                                             from = 'Twitter',
                                             message = "",
                                             icon = icon("twitter"),
                                             href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&text=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                         ),
                                         messageItem(
                                             from = 'Facebook',
                                             message = "",
                                             icon = icon("facebook"),
                                             href = "https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                                         ),
                                         messageItem(
                                             from = 'Google+',
                                             message = "",
                                             icon = icon("google-plus"),
                                             href = "https://plus.google.com/share?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                                         ),
                                         messageItem(
                                             from = 'Sina Weibo',
                                             message = "",
                                             icon = icon("weibo"),
                                             href = "http://service.weibo.com/share/share.php?url=http://example.com&appkey=&title=New%20Zealand%20Trade%20Intelligence%20Dashboard%20http%3A%2F%2Ftradeintelligence.mbie.govt.nz&pic=&ralateUid=&language=zh_cn"
                                         ),
                                         messageItem(
                                             from = 'Pinterest',
                                             message = "",
                                             icon = icon("pinterest-p"),
                                             href = "http://pinterest.com/pin/create/button/?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&media=&description=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                         ),
                                         messageItem(
                                             from = 'LinkedIn',
                                             message = "",
                                             icon = icon("linkedin"),
                                             href = "http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&title=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                         ),
                                         messageItem(
                                             from = 'Tumblr',
                                             message = "",
                                             icon = icon("tumblr"),
                                             href = "http://www.tumblr.com/share?v=3&u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&t=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                         )
                     )
                     
    )


sidebar <- dashboardSidebar(
    dashboardSidebar(width = 230, 
                     sidebarMenu(id = "sidebar",
                                 style = "position: relative; overflow: visible;",
                                 #The first menu Item -- Overview page
                                 menuItem( "Overview", tabName = 'overview', icon = icon('dashboard')),
                                 
                                 #Conditional panel, that will contain the aesthetics of tab 1
                                 conditionalPanel("input.sidebar === 'overview'",
                                                  sliderInput("DatesMerge",
                                                              "Dates:",
                                                              min = as.Date("2020-10-20","%Y-%m-%d"),
                                                              max = as.Date("2020-12-01","%Y-%m-%d"),
                                                              value =as.Date("2020-10-20"),
                                                              timeFormat="%Y-%m-%d")
                                                  ,
                                                  actionButton('btn_build_overview_report', 
                                                               paste0('Build Report'),
                                                               icon = icon('wrench'))))))

body <- dashboardBody( tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #006272;
                       }
                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #006272;
                       }
                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #006272;
                       }
                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #006272;
                                 }
                       ')
),
# 
tags$style( HTML("hr {border-top: 1px solid #000000;}") ),

tags$style(HTML("

                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#222d32
                    }

                    .box.box-solid.box-primary{
                    border-bottom-color:#222d32;
                    border-left-color:#222d32;
                    border-right-color:#222d32;
                    border-top-color:#222d32;
                    background:#222d32
                    }

                    ")),

## to not show error message in shiny
tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),


tabItems(
    #Layout for overview tab
    tabItem(tabName = "overview", 
            fluidPage(
                ## contents for the dashboard tab
                # div(id = 'main_wait_message',
                #     h1('Note, initial load may take up to 10 seconds.',
                #        style = "color:darkblue" , align = "center" ) ,
                #     tags$hr()
                # ),
                fluidRow(
                    valueBoxOutput("Number of Tweets", width = 3),
                    valueBoxOutput("Number of Retweets", width = 3),
                    valueBoxOutput("Number of Handles", width = 3),
                    valueBoxOutput("Number of Mentions", width = 3)),
                
                #Second row of value boxes
                fluidRow(
                    valueBoxOutput("Day", width = 3),
                    valueBoxOutput("Morning Tweets", width = 3),
                    valueBoxOutput("Afternoon Tweets", width = 3),
                    valueBoxOutput("TweetsToday", width = 3)
                ), 
                fluidRow( column( width = 6,h4("Word Cloud", align = 'center'),
                                  box(width = NULL, height = 450,
                                      withSpinner(hwordcloudOutput('wordcloud')) )),
                          
                          
                          column( width = 6,h4("Top 10 words", align = 'center'),
                                  box(width = NULL, height = 450,
                                      tabsetPanel(type = "pills", id = "top_panel", 
                                                  tabPanel("Top 10 words", 
                                                           withSpinner(highchartOutput('top10'))
                                                  ),
                                                  tabPanel("Top 10 handles",
                                                           withSpinner(highchartOutput("overviewhandles")))
                                      )
                                  )
                          )
                          ,
                          # column( width = 4, h4("Am/PM", align = 'center'), highchartOutput("Am/PM"))
                ),
                fluidRow(#column(width = 6, h4("Top handles", align = 'center'), withSpinner(highchartOutput("overviewhandles"))),
                    column(width = 12, h4("Tweet Trend", align = 'center'),
                           
                           box(width = NULL, height = 800, id = "Urgent",
                               tabsetPanel(type = "pills", id = "Reviews",
                                           tabPanel("Date Trend", 
                                                    withSpinner(reactableOutput("tweet_table"))),
                                           tabPanel("Polarity Trend",
                                                    withSpinner(plotlyOutput("Polarity"))))
                           )        
                    )
                    
                )))))

#?selectizeInput
ui <- dashboardPage(header , sidebar, body)



