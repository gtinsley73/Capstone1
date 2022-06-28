#app.R
#https://rshiny.dsa.missouri.edu/students/gpt8g6/DATA-SCI-8090/Capstone-Presentation-Alt/


#load libraries to be used
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(wordcloud)
library(quantmod)
library(DT)

#load data and any carpentry

#Load bitcoin data

bit <- read_csv('yfinance_data.csv')
#bit <- subset(Date >= '2017-12-31')
bbands <- BBands(bit[,c("High","Low","Close")])
bitbb <- subset(cbind(bit, data.frame(bbands[,1:3])), Date >= "2015-01-01")
bitbb$direction <- ifelse(bitbb$Close - bitbb$Open > 0, "Increasing", "Decreasing")

#Load and prep model data for plotly

models <- read_csv('models.csv')

shift <- function(x,n){
    c(x[-(seq(n))], rep(NA,n))}

reg <- models[models$Model == "regression",]
reg$Row.Num <- seq.int(nrow(reg))
reg$last.price <- shift(reg$Close, 1)
reg$prediction <- reg$last.price * (1 + reg$Error)
reg$Date <- as.Date(strptime(reg$Date, format='%Y-%m-%d'))
regdt <- reg %>% select(Date, Close, prediction)
regdt$Close <- round(regdt$Close, digit=3)
regdt$prediction <- round(regdt$prediction, digit=3)

reg1 <- models[models$Model == "random_forest",]
reg1$Row.Num <- seq.int(nrow(reg1))
reg1$last.price <- shift(reg1$Close, 1)
reg1$prediction <- reg1$last.price * (1 + reg1$Error)
reg1$Date <- as.Date(strptime(reg1$Date, format='%Y-%m-%d'))
regdt1 <- reg1 %>% select(Date, Close, prediction)
regdt1$Close <- round(regdt1$Close, digit=3)
regdt1$prediction <- round(regdt1$prediction, digit=3)

reg2 <- models[models$Model == "rnn",]
reg2$Row.Num <- seq.int(nrow(reg2))
reg2$last.price <- shift(reg2$Close, 1)
reg2$prediction <- reg2$last.price * (1 + reg2$Error)
reg2$Date <- as.Date(strptime(reg2$Date, format='%Y-%m-%d'))
regdt2 <- reg2 %>% select(Date, Close, prediction)
regdt2$Close <- round(regdt2$Close, digit=3)
regdt2$prediction <- round(regdt2$prediction, digit=3)

#load sentiment analysis data

btc_df <- read_csv('btc_price_sentiment.csv')
btc_df_vol <- read_csv('btc_df_vol.csv')
btc_df_vol_lag <- read_csv('btc_df_vol_lag.csv')

#Load token info from reddit posts

token <- read_csv('bitcoin_top_tokens.csv')
token_p <- token %>% filter(sentiment_tag == "positive")
token_n <- token %>% filter(sentiment_tag == "negative")
token_neu <- token %>% filter(sentiment_tag == "neutral") %>% filter(token != "bitcoin")
token_n$token[token_n$token=="fuck"] <- "f**k"
token_n$token[token_n$token=="shit"] <- "sh*t"


#create user interface
ui <- dashboardPage(
        dashboardHeader(title = "Group 07 Capstone Project 2022", titleWidth= 350),
        dashboardSidebar(
            width = 200,
            sidebarMenu(
                    id = "sidebar",
                    menuItem("Introduction", tabName = "intro"),
                    menuItem("Market Data", tabName ="visuals1", startExpanded = FALSE,
                         icon = icon("btc")),
                    menuItem("Reddit Analysis", tabName="visuals2",
                         icon = icon("reddit")),
                    menuItem("Predictive Modeling", tabName = "models",
                         icon = icon("cube")
                            )
                            
        )),
        dashboardBody(
            tabItems(
                tabItem(tabName="intro",
                    fluidRow(
                        box(
                            width = 12,
                            h2(tags$b("Introduction"), align="center"),
                            br(),
                            p("The purpose of this dashboard is to understand the relationship between Bitcoin price data and Bitcoin subreddit sentiment score and volume of submissions."),
                            br(),
                            p("The price data for Bitcoin can be found on yahoo finance ", 
                            a("here", href = "https://finance.yahoo.com/quote/BTC-USD/history/", target="_blank"), 
                                "and the specific subreddit used ", 
                                tags$a("here.", href ="https://www.reddit.com/r/Bitcoin/", target="_blank")),
                            br(),
                            p("The inspiration for this dashboard came from the idea of social media influencing the volatility of crypto prices. A specific example: Dogecoin prices and Twitter tweets."),
                            br(),
                            p("Is there a relationship between the price of Bitcoin and the Bitcoin subreddit sentiment score? Based on the correlation of the two, yes there is. However, the sentiment score slightly lags the price and therefore cannot be used to predict the price. This is also true for the submission volume, where the lag is greater (about 6 months)."),
                            br(),
                            p("Natural Language Processing (NLP) was used to analyze the posts and comments in order to score sentiment."),
                            br(),
                            p("Possible Ethical and/or Bias concerns stemmed from the narrow scope of sentiment data. Due to the constraints of computational resources, sources for the sentiment data were limited to a single specific subreddit."
                            )
                        ))),
                 
                tabItem(tabName ="info1",
                    fluidRow(
                        box(
                            width = 12,
                            h3(tags$b("Bitcoin Analysis:")),
                            br(),
                            p("Bitcoin prices increased to around $20k in 2018 for the first time. It was not until the beginning of 2021 that prices drastically increased, with a peak around $60k. Prices continue to fluctuate in this range as 2022 goes on. The volume continuously increases over time.")
                        ))),
                    
                tabItem(tabName = "visuals1",
#Create first row boxes    
                        fluidRow(
                            infoBoxOutput("infobox1"),
                            infoBoxOutput("infobox2"),
                            infoBoxOutput("infobox3")
                        ),
#Create second row tabbed box to hold market charts
                        fluidRow(class = "myRow1",
                                 tags$style(".nav-tabs {background: #ecf0f5;}"),
                            tabBox(
                                  
# The id lets us use input$tabset1 on the server to find the current tab
                                  id = "tabset1", 
                                  height = 530, 
                                  width = 12,
                                  tabPanel(tags$b("Candlestick Chart"),
                                           class = "myRow1",
                                           plotlyOutput("crypto1"),
                                          fluidRow(
                                              box(
                                                  width= 12,
                                                  p("Bitcoin prices increased to around $20k in 2018 for the first time. It was not until the beginning of 2021 that prices drastically increased, with a peak around $60k. Prices continue to fluctuate in this range as 2022 goes on. The volume continuously increases over time."))))
                            
                            ),
                           tags$head(tags$style(".myRow1{height:350px;}"))
                                           
                        )#)
                     ),
                    
                    tabItem(tabName = "visuals2",
                       fluidRow(
                           valueBox(
                               "726,306",
                               tags$b("# Submissions"),
                               icon= icon("id-card-o"),
                               color = "aqua",
                               width = 4),
                           valueBox(
                               "9,524,259",
                               tags$b("# Comments"),
                               icon = icon("reply-all"),
                               color = "blue",
                               width = 4),
                           valueBox(
                               "0.114097",
                               tags$b("Avg Sentiment Score"),
                               icon = icon("thumbs-up"),
                               color = "teal",
                               width = 4)
                           ),
                        fluidRow(class = 'myRow2',
                            tabBox(
# The id lets us use input$tabset2 on the server to find the current tab
                                id = "tabset2", 
                                height = 550, 
                                width = 12,
                                tabPanel(tags$b("Price Change/Sentiment Trends"),
                                           plotlyOutput("crypto3"),
                                           class = 'myRow2',
                                           height = 350,
                                           width=12,
                                              p("Looking at the comparison between the sentiment score and the price change %, the sentiment closely follows the price with a correlation of 0.68. However, the sentiment lags the price by 10 days (increasing the correlation to 0.68) also making the sentiment a poor predictor variable of price.")),
                                
                                    tabPanel(tags$b("Price/Submission Volume Trends"),
                                           class = 'myRow2',
                                           height = 350,
                                           radioButtons("select1", label = NULL,
                                                        choices = list("Actuals"=1, 
                                                        "Submission Volume Trend (-180 day lag)"=2),
                                                        selected = 1, inline=TRUE),
                                                        plotlyOutput("crypto4"),
                                           width=12,
                                               p("Comparing the submission volume to the price change %, there is a visual trend in the data, but the correlation is not significant at 0.2. Upon further analysis, the volume lags the price by 180 days increasing the correlation to 0.7. This indicates that the volume is not a good predictor for price.")#))
                                          )      
                                          
                               
                            ),
                           tags$head(tags$style(".myRow2{height:300px;}"))  
                        ),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        fluidRow(class = 'myRow3',
                            box(
                                title= tags$b("Top Words in Positive Posts",
                                              style = "text-align:center"),
                                width=4,
                                height=455,
                                plotOutput("wordcloud1"),
                                class = 'myRow3'
                            ),
                            box(
                                title = tags$b("Top Words in Neutral Posts",
                                               align = "center"),
                                width=4,
                                height=455,
                                plotOutput("wordcloud2"),
                                class = 'myRow3'
                            ),
                            box(
                                title= tags$b("Top Words in Negative Posts",
                                              align = "center"), 
                                width=4,
                                height=455,
                                plotOutput("wordcloud3"),
                                class = 'myRow3'
                            ),
                       tags$head(tags$style(".myRow3{height:370px;}"))         
                     )),
                    tabItem(tabName ="info2",
                            fluidRow(
                                box(
                                    width = 12,
                                    h3(tags$b("Reddit Analysis:")),
                                    br(),
                                    p("Comparing the submission volume to the price change %, there is a visual trend in the data, but the correlation is not significant at 0.2. Upon further analysis, the volume lags the price by 180 days increasing the correlation to 0.7. This indicates that the volume is not a good predictor for price."),
                                    br(),
                                    p("Looking at the comparison between the sentiment score and the price change %, the sentiment closely follows the price with a correlation of 0.68. However, the sentiment lags the price by 10 days (increasing the correlation to 0.68) also making the sentiment a poor predictor variable of price.")
                                ))),
                            
                    tabItem(tabName = "models",
                           fluidRow(
                               tabBox(
                                   id='tabset3',
                                   height = 530,
                                   width = 12,
                                   tabPanel(
                                       tags$b("Linear Regression"),
                                       fluidRow(
                                           box(
                                               plotlyOutput("reg"),
                                               width=8,
                                               height= 400),
                                           box(
                                               dataTableOutput("table1"),
                                               width=4,
                                               height= 400)),
                                       fluidRow(
                                           valueBox(
                                                   "0.03159",
                                                   tags$b("MAPE"),
                                                   icon = icon("thermometer-2"),
                                                   color = "light-blue",
                                                   width = 6),
                                           valueBox(
                                                   "0.04189",
                                                   tags$b("RMSE"),
                                                   icon = icon("random"),
                                                   color = "light-blue",
                                                   width = 6)),
                                       fluidRow(
                                           box(
                                               width=12,
                                               title=h4(tags$b("Conclusion:")),
                                            p("The Linear Regression model is a simple approach that models a best fit line for the data. This model slightly underperformed the baseline with an MAPE of 0.03159 and an RMSE of 0.04189."),
                                           p("* The baseline model is the median % change of Bitcoin multiplied by the last close price. The MAPE (Mean Absolute Percentage Error)  is 0.03071 and the RMSE (Root Mean Square Deviation) is 0.04182")
                                           )
                                       
                                   )),
                                   tabPanel(
                                       tags$b("Decision Forest"),
                                       fluidRow(
                                           box(
                                               plotlyOutput("reg1"),
                                               width=8,
                                               height= 400),
                                           box(
                                               dataTableOutput("table2"),
                                               width=4,
                                               height= 400)),
                                       fluidRow(
                                           valueBox(
                                                   "0.03352",
                                                   tags$b("MAPE"),
                                                   icon = icon("thermometer-2"),
                                                   color = "light-blue",
                                                   width = 6),
                                           valueBox(
                                                   "0.04274",
                                                   tags$b("RMSE"),
                                                   icon = icon("random"),
                                                   color = "light-blue",
                                                   width = 6)),
                                       fluidRow(
                                           box(
                                               width=12,
                                               title=h4(tags$b("Conclusion:")),
                                           p("The Decision Forest model is a model that creates many decision trees using random samples in the data. A grid search was performed to determine best fit parameters for the data. This model performed the worst out of the three models tested with an MAPE of 0.03352 and an RMSE of 0.04274. It also underperformed when compared to the baseline model. Since the input is just a vector of the last N prices, this limitation may explain why the model didn’t perform well."),
                                           p("* The baseline model is the median % change of Bitcoin multiplied by the last close price. The MAPE (Mean Absolute Percentage Error)  is 0.03071 and the RMSE (Root Mean Square Deviation) is 0.04182")
                                           
                                           )
                                   )),   
                                   tabPanel(tags$b("Recurrent Neural Network"),
                                       fluidRow(
                                           box(
                                               plotlyOutput("reg2"),
                                               width=8,
                                               height= 400),
                                           box(
                                               dataTableOutput("table3"),
                                               width=4,
                                               height= 400)),
                                       fluidRow(
                                           valueBox(
                                                   "0.03059",
                                                   tags$b("MAPE"),
                                                   icon = icon("thermometer-2"),
                                                   color = "light-blue",
                                                   width = 6),
                                           valueBox(
                                                   "0.04124",
                                                   tags$b("RMSE"),
                                                   icon = icon("random"),
                                                   color = "light-blue",
                                                   width = 6)),
                                       fluidRow(
                                              box(
                                                 width = 12,
                                                title = h4(tags$b("Conclusion:")),
                                              p( "The Long Short-Term Memory Recurrent Neural Network (LSTM RNN) model is relatively more complex than the first two models. The RNN is used to create large neural networks that have multiple layers that are activated based on the data it’s being trained on. The specific parameters were optimized with a grid search. This model slightly outperformed the baseline with an MAPE of 0.03059% and an RMSE of 0.04124 and was the best performing of all observed models."),
                                              p("* The baseline model is the median % change of Bitcoin multiplied by the last close price. The MAPE (Mean Absolute Percentage Error) is 0.03071 and the RMSE (Root Mean Square Deviation) is 0.04182")


                                               
                                           )    
                                   ))    
                                           
                                   )))),
tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }')))    
))
   
#create server function to initiate ui functionality    
server <- (function(input, output, session) {
        
       
# plot candlestick chart
        output$crypto1 <- renderPlotly({

            
i <- list(line = list(color = 'darkblue'))
d <- list(line = list(color = 'darkorange'))            
            


fig <- bitbb %>% plot_ly(x = ~Date, type="candlestick",
          open = ~Open, close = ~Close,
          high = ~High, low = ~Low, name = "BTC",
          increasing = i, decreasing = d,
          source='A') 
fig <- fig %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
            line = list(color = '#ccc', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) 
fig <- fig %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
            line = list(color = '#ccc', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") 
fig <- fig %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F) 
fig <- fig %>% layout(yaxis = list(title = "Price"))
                      

# plot volume bar chart
fig2 <- bitbb 
fig2 <- fig2 %>% plot_ly(x=~Date, y=~Volume, type='bar', name = "BTC Volume",
          color = ~direction, colors = c('darkblue','darkorange'),hoverinfo='none') 
fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"), showgrid = F)

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'left', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=1,
                  label='1 YR',
                  step='year',
                  stepmode='backward'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward')
           ))

# subplot with shared x axis
fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE)
fig <- fig %>% layout(title = paste("Bitcoin: 2015-01-01 - 2022-04-05"),
         xaxis = list(title="",rangeselector = rs, rangeslider = list(visible=F), showgrid=F),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent')) 
         

fig    
    
    })

#plot Sentiment Trend/Price Change % Trend    
       output$crypto3 <- renderPlotly({
        
        fig <- plot_ly(
                      data = btc_df,
                      x = ~Date, 
                      y = ~scaled_sentiment,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Reddit Sentiment Trend',
                      line = list(color="darkorange"),
                      height = 350
                     ) %>%
                      add_trace(y = ~scaled_change, 
                                name = 'Price Change % Trend', 
                                line = list(color = 'darkblue'))
        
        fig <- fig %>%
               layout(
                       xaxis = list(title="",
                           type = 'date', 
                                    showgrid=F), 
                       yaxis =list(title="Sentiment Score", 
                                   showgrid=F),
                       hovermode = "x"    
               )

        fig
    })

#Plot Word Clouds
    output$wordcloud1 <- renderPlot({
        
        
            
        fig <- wordcloud(word=token_p$token, freq=token_p$frequency, min.freq = .000001, 
          max.words=500, scale=c(6,0.75), random.order=FALSE, rot.per=0.35, colors=brewer.pal(6,"Blues"))
        
        fig
    })
        
    output$wordcloud2 <- renderPlot({
        
        
            
        fig <- wordcloud(word=token_neu$token, freq=token_neu$frequency, min.freq = .000001, 
          max.words=500, scale=c(6,0.75), random.order=FALSE, rot.per=0.35, colors=brewer.pal(6,"Greys"))
        
        fig
    })
    
    output$wordcloud3 <- renderPlot({
        
        fig <- wordcloud(word=token_n$token, freq=token_n$frequency, min.freq = .000001,
          max.words=500, scale=c(6,0.75), random.order=FALSE, rot.per=0.35, colors=brewer.pal(6,"Reds"))
        
        fig
    })
    
#Plot Submission Volume Trend/Price Change % Trend w/ selection option for display
    output$crypto4 <- renderPlotly({
        
        if (input$select1 == 1)
            {
            
            fig = plot_ly(
                          data = btc_df_vol,
                          x = ~Date, 
                          y = ~scaled_volume,
                          type = 'scatter',
                          mode = 'lines',
                          name = 'Reddit Submission Volume Trend',
                          height = 350,  
                          line = list(color='darkorange'
                              )
                            ) %>%
                          add_trace(y = ~scaled_change, name = 'Price Change % Trend', line = list(color='darkblue'

                          ))

            fig <- fig %>% layout(
                                xaxis = list(title="",
                                showgrid = F,
                                type = 'date'
                                  ),
                                yaxis = list(title="Scaled Volume",
                                             showgrid = F),
                                hovermode = "x"
                               )

                   fig
       } else
            {
            fig = plot_ly(
                          data = btc_df_vol_lag,
                          x = ~Date, 
                          y = ~lag_volume_scaled,
                          type = 'scatter',
                          mode = 'lines',
                          name = 'Reddit Submission Volume Trend',
                          height = 350,
                          line = list(color='darkorange'
                          )
                    ) %>%
                    add_trace(y = ~scaled_change, name = 'Price Change % Trend', line = list(color='darkblue'

                  ))

            fig <- fig %>%
                          layout(
                                xaxis = list(title="",
                                showgrid = F,
                                type = 'date'
                              ),
                                yaxis = list(title="Scaled Volume",
                                             showgrid = F),
                                hovermode = "x"
                              )

                    fig
                   }
        })

#Plot the Models
    output$reg <- renderPlotly({
        
        fig = plot_ly(
    data = reg,
    x = ~Date, 
    y = ~prediction, 
    type = 'scatter',
    mode = 'lines',
    name = 'Predicted', 
    line = list(color= 'darkorange', width=0.75),
    marker = list(color = 'darkorange', opacity=0)
) %>% add_trace(
    y = ~Close,
    name = "Actual",
    type = 'scatter',
    mode = 'lines',
    line = list(color= 'darkblue'),
    marker = list(color = 'darkblue', opacity=0)
) %>% layout(
    yaxis = list(title="Price", rangemode = "tozero"),
    xaxis = list(title="", 
                 rangemode = "tozero",showgrid=F),
            hovermode='x'
   
)

fig
     })   


output$reg1 <- renderPlotly({
    
fig = plot_ly(
    data = reg1,
    x = ~Date, 
    y = ~prediction, 
    type = 'scatter',
    mode = 'lines',
    name = 'Predicted', 
    line = list(color= 'darkorange', width=0.75),
    marker = list(color = 'darkorange', opacity=0)
) %>% add_trace(
    y = ~Close,
    name = "Actual",
    type = 'scatter',
    mode = 'lines',
    line = list(color= 'darkblue'),
    marker = list(color = 'darkblue', opacity=0)
) %>% layout(
    yaxis = list(title="Price", rangemode='tozero'), #overlaying = "y", side = "right"),
    xaxis = list(title="", 
                 rangemode = "tozero",showgrid=F),
            hovermode='x'
    
)

fig
    
})

output$reg2 <- renderPlotly({
    
fig = plot_ly(
    data = reg2,
    x = ~Date, 
    y = ~prediction, 
    type = 'scatter',
    mode = 'lines',
    name = 'Predicted', 
    line = list(color= 'darkorange', width=0.75),
    marker = list(color = 'darkorange', opacity=0)
) %>% add_trace(
    y = ~Close,
    name = "Actual",
    type = 'scatter',
    mode = 'lines',
    line = list(color= 'darkblue'),
    marker = list(color = 'darkblue', opacity=0)
) %>% layout(
    yaxis = list(title="Price", rangemode = 'tozero'), #overlaying = "y", side = "right"),
    xaxis = list(title="", 
                 rangemode = "tozero",showgrid=F),
            hovermode='x'
    
)

fig
    
})

#Plot the tables with Actual and Predicted Prices
    output$table1 <- DT::renderDataTable({
        datatable(regdt, options=list(dom='ftpi',pageLength = 5)) 
        
        })

    output$table2 <- DT::renderDataTable({
        datatable(regdt1, options=list(dom='ftpi',pageLength = 5)) 
        
        })
    
    output$table3 <- DT::renderDataTable({
        datatable(regdt2, options=list(dom='ftpi',pageLength = 5)) 
        
        })
  
#Show infoboxes with dynamic input from hover on Market Charts   
    output$infobox1 <- renderInfoBox({
        
    d <- event_data("plotly_hover", source='A')
    dt <- if (is.null(d)) max(bit$Date) else d$x
    
    infoBox(paste0(dt, " Price"), h3(format(round(bit$Close[bit$Date==dt],3),big.mark=",", scientific=FALSE)),icon=icon('btc'),
            color="orange", width=4, fill=TRUE)
        })
    
    output$infobox2 <- renderInfoBox({
        
    d <- event_data("plotly_hover", source='A')
    dt <- if (is.null(d)) max(bit$Date) else d$x
    
    infoBox(paste0("Percent Change"), h3(format(round(bit$Percent.Diff[bit$Date==dt]*100,3))),icon=icon('percent'),
            color="light-blue", width=4, fill=TRUE)
        })
    
    output$infobox3 <- renderInfoBox({
        
    d <- event_data("plotly_hover", source='A')
    dt <- if (is.null(d)) max(bit$Date) else d$x
    
    infoBox(paste0("Volume"), h3(format(round(bit$Volume[bit$Date==dt],3),big.mark=",", scientific=FALSE)),icon=icon('bar-chart'),
            color="orange", width=4, fill=TRUE)
        })
    
    
})

#Initiate App
shinyApp(ui, server)