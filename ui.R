library(shiny)
library(shinydashboard)
getwd()
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Online News Popularity", titleWidth = 500,
                    tags$li(class ='dropdown',tags$a(href = "http://www.mashable.com", icon("newspaper"),"Mashable.com" , target = "_blank"))),
    dashboardSidebar("sidebar home",
                     sidebarMenu(
                       menuItem("Introduction", tabName = "intro", icon = icon("file")),
                       menuItem("Database" , tabName = "data", icon = icon("database") , badgeLabel = "edited" , badgeColor = "green"),
                       menuItem("Charts" , tabName = "Charts" , icon = icon("chart-line"),
                                menuSubItem("pie-charts" , tabName = "pie_c" , icon = icon("chart-line")),
                                menuSubItem("scatter-charts" , tabName = "scatter_c" , icon = icon("chart-line")),
                                menuSubItem("box-plots" , tabName = "box_plot_c" , icon = icon("chart-line")))
                     )),
    dashboardBody("shiny dashboard",
                  tabItems(
                    tabItem(tabName = "intro" , 
                            h3(p("INTRODUCTION:")),
                            p("Mashable Inc. is a digital media website founded in 2005. It has over 9.5 million Twitter followers and over 6.5 million likes on Facebook.
The Online News Popularity dataset summarizes a heterogeneous set of features about articles published by Mashable in a period of two years. The articles were published by Mashable (www.mashable.com) and their content as the rights to reproduce it belongs to them. Hence, this dataset does not share the original content but some statistics associated with it. The original content be publicly accessed and retrieved using the provided urls. THe dataset used in this dashboard is taken from https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity.
"),
                            p(""),
                            h3(p("PROBLEM STATEMENT:")),
                            p("Based on the features given in the dataset we aim to classify the articles as either popular(1) or unpopular(0). The classification (1/0) is done on the basis of the median value of shares of each article i.e 1400."),
                            p(""),
                            h3(p("NUMBER OF ATTRIBUTES:")),
                            p("58 predictive attributes, 2 non-predictive, 1 goal field"),
                            p("")),
                    tabItem(tabName = "data" , dataTableOutput("mydata")),
                    tabItem(tabName = "pie_c" , 
                            fluidRow(
                              tabBox(id = "weekday-chart" , 
                                     tabPanel("pie" , plotlyOutput("plot1")),
                                     tabPanel("bar" , plotlyOutput("plot2"))),
                              tabBox(id = "channel-chart" , 
                                     tabPanel("pie" , plotlyOutput("plot3")),
                                     tabPanel("bar" , plotlyOutput("plot4"))),
                              tabBox(id = "target-chart" , 
                                     tabPanel("weekday" , plotlyOutput("plot9")),
                                     tabPanel("channel" , plotlyOutput("plot10")))
                             )),
                    tabItem(tabName = "scatter_c",
                            fluidRow(box(title = "title~subjectivity", plotlyOutput("plot5", height = 250)),
                                     box(title = "content~subjectivity", plotlyOutput("plot6", height = 250)),
                                     box(title = "title~polarity", plotlyOutput("plot7", height = 250)),
                                     box(title = "content~polarity", plotlyOutput("plot8", height = 250))
                       )),
                    tabItem(tabName = "box_plot_c",
                            fluidRow(box(title = "No. of words in content", plotlyOutput("plot11", height = 250)),
                                     box(title = "No. of unique words in content", plotlyOutput("plot12", height = 250)),
                                     box(title = "No. of links in content", plotlyOutput("plot13", height = 250)),
                                     box(title = "No. of links related to Mashable articles", plotlyOutput("plot14", height = 250)),
                                     box(title = "No. of images in content", plotlyOutput("plot15", height = 250)),
                                     box(title = "No. of videos in content", plotlyOutput("plot16", height = 250)),
                                     box(title = "Max shares of the least popular keyword", plotlyOutput("plot17", height = 250)),
                                     box(title = "Min shares of the least popular keyword", plotlyOutput("plot18", height = 250)),
                                     box(title = "Avg shares of the least popular keyword", plotlyOutput("plot19", height = 250))
                  ))
  ))
))
  
