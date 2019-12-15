library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)


final_news = read.csv("D:\\BSE DATASCIENCE PROJECT TRACK\\final project folder\\final_data_for_visualisation.csv")
#final_news = subset(final_news, select = -c(X.1,X))
final_news = as.data.frame(final_news)

agg_weekday = as.data.frame(aggregate(final_news$shares, by = list(Category = final_news$combined_weekday), FUN = sum))
agg_channel = as.data.frame(aggregate(final_news$shares, by = list(Category = final_news$combined_channel), FUN = sum))
count_weekday = as.data.frame(aggregate(final_news$target, by = list(Category = final_news$combined_weekday, final_news$target), FUN = length))
count_channel = as.data.frame(aggregate(final_news$target, by = list(Category = final_news$combined_channel, final_news$target), FUN = length))


shinyServer(function(input,output, session){
  output$mydata = renderDataTable({
   final_news[,1:59] 
  })
  output$plot1 = renderPlotly({
    plot_ly(data = agg_weekday,labels =~Category , values =~x , type = 'pie') %>% layout(title="Total Shares on Each Weekday")
  })
  output$plot2 = renderPlotly({
    plot_ly(data = final_news , 
            x=~combined_weekday,
            y=~shares,
            color =~combined_weekday,
            type= 'bar')  %>%
      layout(title = "Total Shares on Each Weekday", xaxis = list(title = "Days of the week"), yaxis = list(title = "No. of shares"))
  })
  output$plot3 = renderPlotly({
    plot_ly(data = agg_channel,labels =~Category , values =~x , type = 'pie') %>% layout(title="Total Shares on Each Channel")
  })
  output$plot4 = renderPlotly({
    plot_ly(data = final_news , 
            x=~combined_channel,
            y=~shares,
            color =~ combined_channel,
            type= 'bar') %>%
      layout(title = "Total Shares on Each Channel", xaxis = list(title = "Channel Name"), yaxis = list(title = "No. of shares"))
    
    })
  output$plot5 = renderPlotly({
    plot_ly(data = final_news , 
            x =~n_tokens_title,
            y =~title_subjectivity,
            type = "scatter", 
            mode = "markers",
            color = ~n_tokens_title) 
  })
  output$plot6 = renderPlotly({
    plot_ly(data = final_news , 
            x =~n_tokens_content,
            y =~global_subjectivity,
            type = "scatter", 
            mode = "markers",
            color = ~n_tokens_content) 
  })
  output$plot7 = renderPlotly({
    plot_ly(data = final_news , 
            x =~n_tokens_title,
            y =~title_sentiment_polarity,
            type = "scatter", 
            mode = "markers",
            color = ~n_tokens_title) 
  })
  output$plot8 = renderPlotly({
    plot_ly(data = final_news , 
            x =~n_tokens_content,
            y =~global_sentiment_polarity,
            type = "scatter", 
            mode = "markers",
            color = ~n_tokens_content) 
  })
  output$plot9 = renderPlotly({
    ggplot(data = count_weekday, aes(x = Category, y = x, fill = Group.2, group = Group.2)) + geom_bar(stat = "identity", position = "dodge") + guides(fill = FALSE) + labs(title = "Count of Target for Each Weekday") + xlab("Weekdays") + ylab("Count of Target")
  })
  output$plot10 = renderPlotly({
    ggplot(data = count_channel, aes(x = Category, y = x, fill = Group.2, group = Group.2)) + geom_bar(stat = "identity", position = "dodge") + guides(fill = FALSE) + labs(title = "Count of Target for Each Channel") + xlab("Channel") + ylab("Count of Target")
  })
  output$plot11 = renderPlotly({
    plot_ly(data = final_news,
            y=~n_tokens_content,
            type = "box"
    )
  })
  output$plot12 = renderPlotly({
    plot_ly(data = final_news,
            y=~n_unique_tokens,
            type = "box")
  })
  output$plot13 = renderPlotly({
    plot_ly(data = final_news,
            y=~num_hrefs,
            type = "box")
  })
  output$plot14 = renderPlotly({
    plot_ly(data = final_news,
            y=~num_self_hrefs,
            type = "box")
  })
  output$plot15 = renderPlotly({
    plot_ly(data = final_news,
            y=~num_imgs,
            type = "box")
  })
  output$plot16 = renderPlotly({
    plot_ly(data = final_news,
            y=~num_videos,
            type = "box")
  })
  output$plot17 = renderPlotly({
    plot_ly(data = final_news,
            y=~kw_max_min,
            type = "box")
  })
  output$plot18 = renderPlotly({
    plot_ly(data = final_news,
            y=~kw_min_min,
            type = "box")
  })
  output$plot19 = renderPlotly({
    plot_ly(data = final_news,
            y=~kw_max_avg,
            type = "box")
  })
})

