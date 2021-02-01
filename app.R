library(shiny)
library(shinythemes)
library(shinycustomloader)
library(tidyverse)
library(DT)
library(caret)
library(datarium)
library(plotly)

Channels <- c('YouTube', 'Facebook', 'Newspaper')

ui <- fluidPage(
    tags$head(includeHTML('google-analytics.html')),
    
    theme = shinytheme('united'),
    
    titlePanel('Build Your Advertising Model'),
    p('Optimize your advertising money to maximize sales.'),
    # hr(),
    
    sidebarLayout(
        sidebarPanel(
            h3('Build Your Model'),
            
            checkboxGroupInput(
                inputId = 'channels',
                label = '1. Advertising channels:',
                choices = Channels,
                selected = NULL,
                inline = TRUE
            ),
            
            sliderInput(
                inputId = 'split',
                label = '2. Percentage of past sales data to use to build model:',
                post = '%',
                min = 5,
                max = 95,
                step = 5,
                value = 50
            ),
            
            # p('Out of the past sales data, select the size of the "training set" to train and build a new model. The rest will be used as "test set" to test your new model later on.'),
            
            hr(),
            
            h3('How to Use?'),
            
            p(strong('Data'), ' - Check the past sales records.'),
            p(strong('Explore'), ' - Explore how the channels are related to sales.'),
            p(strong('Model'), ' - Build a new advertising model.'),
            p(strong('Test'), ' - Test the advertising model.'),
            p(strong('Predict'), ' - Use the model to predict new sales.'),
            
            hr(),
            a('Made by Ricky Soo | Feedback welcomed', href = 'https://github.com/rickysoo', target = '_blank')
        ),
        
        mainPanel(
            
            # fluidRow(
            #     column(
            #         width = 4,
            #         checkboxGroupInput(
            #             inputId = 'channels',
            #             label = 'Feature set',
            #             choices = Channels,
            #             selected = 'YouTube',
            #             inline = TRUE
            #         )
            #     ),
            #     
            #     column(
            #         width = 4,
            #         
            #         sliderInput(
            #             inputId = 'split',
            #             label = 'Training set',
            #             min = 0.05,
            #             max = 0.95,
            #             step = 0.05,
            #             value = 0.80
            #         )
            #     )
            # ),
            
            tabsetPanel(
                id = 'tab',
                
                tabPanel(
                    title = 'Data',
                    value = 'Data',

                    br(),
                    p('In this do-it-yourself case study, you will use past sales data to explore how advertising channels are related to sales, and then use your judgment to build a new advertising model in order to optimize your money for better sales in the future.'),
                    p('In the 200 advertising campaigns below, advertising money was spent on 3 channels (YouTube, Facebook and newspaper) to bring in sales for that campaign.'),
                    
                    br(),
                    withLoader(DTOutput('data'), loader = 'pacman')
                ),
                
                tabPanel(
                    title = 'Explore',
                    value = 'Explore',
                    
                    br(),
                    p('Money spent on advertising does not necessarily bring in sales. That\'s why we should evaluate past performance and understand how each advertising channel is related to sales.'),
                    p('Move your mouse over the colored dots below to find out how much money was spent on the channel and how much sales was generated in each campaign. The blue lines are "regression lines" showing the pattern of the relationship in each channel.'),
                    
                    br(),
                    withLoader(plotlyOutput('explore_plot', height = 600), loader = 'pacman')
                ),
                
                tabPanel(
                    title = 'Model',
                    value = 'Model',
                    
                    br(),
                    p('Now it\'s time to build a new advertising model for better sales in the future. Head over to the left side and choose your preferred advertising channels and the size of training set to use.'),
                    p('As you make your choices, a new model is built on-the-fly and the results are shown below. Try your best to understand the estimates and how they will contribute to the sales generated.'),
                    
                    br(),
                    withLoader(tableOutput('model_table'), loader = 'pacman'),
                    withLoader(textOutput('model_text'), loader = 'pacman'),
                    
                    br(),
                    p('Below is the technical output of the model training using linear regression:'),
                    withLoader(verbatimTextOutput('model_verbatim'), loader = 'pacman')
                ),
                
                tabPanel(
                    title = 'Test',
                    value = 'Test',
                    
                    br(),
                    p('Now let\'s test your model using the "test set" you set aside (the part of the past sales data not used as "training set").'),

                    withLoader(htmlOutput('test_text'), loader = 'pacman'),
                    withLoader(plotlyOutput('test_plot', height = 600), loader = 'pacman')
                ),
                
                tabPanel(
                    title = 'Predict',
                    value = 'Predict',
                    
                    br(),
                    p('With the new advertising model, now you can predict the sales of a campaign by adjusting the expenses on the advertising channels!'),
                    p('Well done for building your own model! Wish you success applying the same skills on your business and other areas of life.'),
                    
                    fluidRow(
                        column(
                            width = 4,
                            sliderInput(
                                inputId = 'youtube',
                                label = 'YouTube',
                                pre = '$',
                                min = 0,
                                max = 400,
                                step = 1,
                                value = 100
                            )
                        ),
                        column(
                            width = 4,
                            sliderInput(
                                inputId = 'facebook',
                                label = 'Facebook',
                                pre = '$',
                                min = 0,
                                max = 400,
                                step = 1,
                                value = 100
                            )
                        ),
                        column(
                            width = 4,
                            sliderInput(
                                inputId = 'newspaper',
                                label = 'Newspaper',
                                pre = '$',
                                min = 0,
                                max = 400,
                                step = 1,
                                value = 100
                            )        
                        )
                    ),
                    
                    br(),
                    p('The table below shows the advertising expenses on each channel, the estimated sales generated from each channel, and the total sales expected. "General" represents the baseline sales to be made even if no money is spent on any advertising.'),
                    withLoader(tableOutput('predict_table'), loader = 'pacman'),
                    
                    p('The plot below helps you to visualize the advertising expenses from each channel, and how each channel contributes to the total sales.'),
                    withLoader(plotlyOutput('predict_plot', height = 600), loader = 'pacman')
                )
            )
        )
    )
)

server <- function(input, output, session) {
    values <- reactiveValues(
        model = NULL,
        samples = NULL
    )
    
    load_data <- reactive({
        data('marketing', package = 'datarium')
        colnames(marketing) = c(Channels, 'Sales')
        marketing$Sales <- marketing$Sales * 100
        marketing <- mutate(marketing, Campaign = row_number())
        marketing[c('Campaign', Channels, 'Sales')]
    })  
    
    load_selected_data <- reactive({
        data <- load_data()
        
        for (Channel in Channels) {
            if (!Channel %in% input$channels) {
                data <- select(data, -Channel)
            }
        }
        
        data
    })
    
    load_data_long <- reactive({
        data <- load_data() %>%
            pivot_longer(all_of(Channels), names_to = 'Channel', values_to = 'Expenses')
        
        data$Channel <- factor(data$Channel, levels = Channels)
        data
    })
    
    output$data <- renderDT({
        datatable(
            load_data(),
            rownames = FALSE,
            class = 'cell-border compact stripe',
            extensions = c('Responsive'),
        ) %>%
            formatRound(c('Sales'), digits = 2) %>%
            formatStyle(c('Sales'), fontWeight = 'bold')
    }) 
    
    # feature_correlation <- function(feature) {
    #     data <- load_selected_data()
    # 
    #     CorrTest <- cor.test(data[[feature]], data$Sales)
    #     print(CorrTest)
    #     Plabel <- ifelse(CorrTest$p.value < 0.001, 'p < 0.001', paste0('p = ', round(CorrTest$p.value, 3)))
    #     Plabel
    # }
    
    output$explore_plot <- renderPlotly({
        # if (length(input$channels) == 0) {
        #     return (NULL)
        # }
        
        data <- load_data_long()
        # filter(Channel %in% input$channels)
        
        # feature <- input$explore_feature
        # 
        # if (feature == '') {
        #     return (NULL)
        # 
        # }
        
        # feature <- 'Expenses'
        
        # CorrTest <- cor.test(data[[feature]], data$Sales)
        # Plabel <- ifelse(CorrTest$p.value < 0.001, 'p < 0.001', paste0('p = ', round(CorrTest$p.value, 3)))
        
        # ggplot(data, aes(.data[[feature]], Sales)) +
        viz <- ggplot(data, aes(x = Expenses, y = Sales)) +
            geom_point(aes(color = Channel)) +
            geom_smooth(formula = y ~ x, method = 'loess', se = FALSE, color = 'blue') +
            facet_grid(rows = vars(Channel), scales = 'free_x') +
            ggtitle(paste0('Advertising Expenses and Sales')) +
            # geom_text(aes(max(Expenses) * 0.05, max(Sales) * 0.98, label = Channel)) +
            # ggtitle(paste0('Exploring ', feature, ' and Sales')) +
            theme(
                plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
                legend.position = 'None'
            ) 
        
        ggplotly(viz) %>%
            layout(
                margin = 100,
                xaxis = list(fixedrange = TRUE),
                yaxis = list(fixedrange = TRUE)
            ) %>%
            hide_legend()
        
        # annotate(
        #     'text',
        #     # label = paste0('R = ', round(CorrTest$estimate, 2), ', ', Plabel),
        #     label = '', #paste0('R = ', data[['Channel']]),
        #     x = max(.data[['Channel']], na.rm = TRUE) * 0.1,
        #     y = max(.data[['Expenses']], na.rm = TRUE) * 0.98,
        #     color = 'steelblue',
        #     size = 5
        # )
    })
    
    explain_significance <- function(s) {
        if (is.na(s)) {
            return ('')
        }
        else if (s > 0.05) {
            return ('This estimate is NOT acceptable. The probability of getting this estimate by chance is more than 5%.')
        }
        else if (s > 0.01) {
            return ('This estimate is acceptable. The probability of getting this estimate by chance is less than 5%.')
        }
        else if (s > 0.001) {
            return ('This estimate is significant. The probability of getting this estimate by chance is less than 1%.')
        }
        else {
            return ('This estimate is highly significant. The probability of getting this estimate by chance is less than 0.1%.')
        }
    }
    
    output$model_table <- renderTable(
        {
            if (length(input$channels) == 0) {
                return (NULL)
            }

            if (is.null(values$model)) {
                return (NULL)
            }
            
            summary <- summary(values$model)
            
            data <- summary$coefficients %>%
                as.data.frame() %>%
                rownames_to_column() %>%
                select(c(1, 2, 5))
            
            intercept <- data[1, ]
            data <- data[-1, ]

            data <- rbind(data, intercept)
            data[nrow(data), 1] <- 'General'
            
            colnames(data) <- c('Channel', 'Estimate', 'Significance')
            
            data$Significance <- sapply(as.numeric(data$Significance), explain_significance)
            data <- data %>%
                mutate(
                    Meaning = sprintf('Spend one additional dollar spent on this channel, and sales is expected to increase by %.2f.', Estimate)
                )
            
            data[nrow(data), 'Meaning'] <- 'This is the expected baseline sales when no money is spent on any channel.'
            data[c('Channel', 'Estimate', 'Meaning', 'Significance')]
        },
        
        colnames = TRUE,
        digits = 2,
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$model_text <- renderText({
        if (length(input$channels) == 0) {
            return ('Start with selecting the advertising channels on the left, and the results will be shown here')
        }
        
        if (is.null(values$model)) {
            return ('Start with selecting the advertising channels on the left, and the results will be shown here')
        }
        
        summary <- summary(values$model)
        
        F <- summary$fstatistic
        pv <- pf(F[1], F[2], F[3], lower.tail = FALSE)
        
        sprintf('The adjusted R-square of the model is %.2f%%. This estimate represents how much of the variation of the sales that can be explained by the model. %s', summary$adj.r.squared * 100, explain_significance(pv))
    })
    
    output$model_verbatim <- renderPrint({
        if (length(input$channels) == 0) {
            return (NULL)
        }
        
        if (is.null(values$model)) {
            return ('No model trained yet.')
        }
        
        summary(values$model)
    })

    # output$test_table <- renderTable(
    #     {
    #         if (length(input$channels) == 0) {
    #             return (NULL)
    #         }
    #         
    #         if (is.null(values$model)) {
    #             return (NULL)
    #         }
    # 
    #         data <- load_data()[-values$samples, ]
    #         model <- values$model
    # 
    #         predictions <- predict(model, data)
    #         RMSE <- RMSE(predictions, data$Sales)
    #         Percent <- RMSE / mean(data$Sales)
    #         R2 <- R2(predictions, data$Sales)
    # 
    #         print(RMSE)
    #         print(R2)
    # 
    #         data.frame(
    #             'Result' = c('Test Set Size', 'Root Mean Square Error (RMSE)', 'R-Square (R2)'),
    #             'Value' = c(nrow(data), RMSE, R2),
    #             'Meaning' = c('', '', '')
    #         )                        
    #     },
    #     
    #     colnames = TRUE,
    #     digits = 2,
    #     bordered = TRUE,
    #     striped = TRUE,
    #     hover = TRUE
    # )
    # 

    output$test_text <- renderUI({
        if (length(input$channels) == 0) {
            return ('Start with selecting the advertising channels on the left, and the results will be shown here')
        }
        
        if (is.null(values$model)) {
            return ('Start with selecting the advertising channels on the left, and the results will be shown here')
        }

        data <- load_data()[-values$samples, ]
        model <- values$model

        predictions <- predict(model, data)
        RMSE <- RMSE(predictions, data$Sales)
        AverageSales <- mean(data$Sales)
        Percent <- RMSE / AverageSales * 100
        R2 <- R2(predictions, data$Sales) * 100

        text1 <- sprintf('<p><strong>Root Mean Squared Error (RMSE)</strong> - RMSE measures the average difference between the predicted sales and the actual sales in the test data. The lower it is, the more accurate the model is. The RMSE for this model is %.2f, which is %.2f%% of the average actual sales of %.2f. You can decide whether this variation is acceptable or not.</p>', RMSE, Percent, AverageSales)
        text2 <- sprintf('<p><strong>R-squared</strong> - This is the squared correlation between the predicted sales and the actual sales in the test data. The higher it is, the better the model is. The R-squared for this model is %.2f%%.</p>', R2)
        text3 <- sprintf('<p><strong>Residual plot</strong> - You can use the plot below to visually gauge whether the prediction is close enough to the data or not. The yellow dots are the predicted sales, and the other colored dots are the actual sales. The model is good if you see small gaps between them.</p>')
        
        HTML(paste(text1, text2, text3, sep = ''))
    })

    output$test_plot <- renderPlotly({
        if (length(input$channels) == 0) {
            return (NULL)
        }
        
        if (is.null(values$model)) {
            return (NULL)
        }
        
        data <- load_data()[-values$samples, ]
        model <- values$model
        
        data$Estimate <- predict(model, data)
        data$Residual <- data$Estimate - data$Sales
        
        data <- pivot_longer(data, all_of(Channels), names_to = 'Channel', values_to = 'Expenses')
        data$Channel <- factor(data$Channel, levels = Channels)
        
        viz <- ggplot(data, aes(x = Expenses, y = Sales)) +
            geom_point(aes(color = Channel)) +
            geom_point(aes(y = Estimate), color = "yellow", shape = 1) +
            geom_segment(aes(xend = Expenses, yend = Estimate), color = "gray", alpha = 0.7) +
            facet_grid(rows = vars(Channel), scales = 'free_x') +
            ggtitle('Residual Plot - Predicted Sales vs. Actual Sales') +
            theme(
                plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
                legend.position = 'None'
            )  
        
        ggplotly(viz) %>%
            layout(
                margin = 100,
                xaxis = list(fixedrange = TRUE),
                yaxis = list(fixedrange = TRUE)
            ) %>%
            hide_legend()
    })

    output$predict_table <- renderTable(
        {
            if (length(input$channels) == 0) {
                return (NULL)
            }
            
            if (is.null(values$model)) {
                return (NULL)
            }
                        
            summary <- summary(values$model)
            estimates <- summary(values$model)$coefficients
            
            # print(estimates)

            estimate_youtube <- ifelse('YouTube' %in% input$channels, estimates['YouTube', 'Estimate'], 0)
            estimate_facebook <- ifelse('Facebook' %in% input$channels, estimates['Facebook', 'Estimate'], 0)
            estimate_newspaper <- ifelse('Newspaper' %in% input$channels, estimates['Newspaper', 'Estimate'], 0)
            
            sales_youtube <- ifelse('YouTube' %in% input$channels, estimate_youtube * input$youtube, 0)
            sales_facebook <- ifelse('YouTube' %in% input$channels, estimate_facebook * input$facebook, 0)
            sales_newspaper <- ifelse('YouTube' %in% input$channels, estimate_newspaper * input$newspaper, 0)
            sales_general <- estimates[1, 'Estimate']
            sales_total <- sales_youtube + sales_facebook + sales_newspaper + sales_general
            
            data.frame(
                'Channel' = c(Channels, 'General', 'Total'),
                'Expenses' = c(input$youtube, input$facebook, input$newspaper, 0, NA),
                'Estimates' = c(estimate_youtube, estimate_facebook, estimate_newspaper, sales_general, NA),
                'Sales' = c(sales_youtube, sales_facebook, sales_newspaper, sales_general, sales_total)
            )
        },
        
        colnames = TRUE,
        digits = 2,
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    channel_estimate <- function(channel) {
        estimates <- summary(values$model)$coefficients
        channel <- as.character(channel)
        
        if (channel %in% rownames(estimates)) {
            return (estimates[channel, 'Estimate'])
        }
        else {
            return (0)
        }
    }
    
    output$predict_plot <- renderPlotly({
        if (length(input$channels) == 0) {
            return (NULL)
        }
        
        if (is.null(values$model)) {
            return (NULL)
        }
        
        data <- load_data_long()
        model <- values$model
        estimates <- summary(model)$coefficients
        
        newdata <- data.frame(YouTube = input$youtube, Facebook = input$facebook, Newspaper = input$newspaper)
        predicted <- predict(model, newdata)

        # print(estimates)        
        # print(predicted)
        
        data <- data %>%
            mutate(
                x = ifelse(Channel == 'YouTube', input$youtube, ifelse(Channel == 'Facebook', input$facebook, ifelse(Channel == 'Newspaper', input$newspaper, 0))),
                # y = ifelse(Channel == 'YouTube', x * estimates['youtube', 'Estimate'], ifelse(Channel == 'Facebook', x * estimates['facebook', 'Estimate'], ifelse(Channel == 'Newspaper', x * estimates['newspaper', 'Estimate'], 0)))
                y = x * sapply(Channel, channel_estimate)
            )
        
        viz <- ggplot(data, aes(x = Expenses, y = Sales)) +
            geom_point(aes(color = Channel)) +
            geom_smooth(formula = y ~ x, method = 'loess', se = FALSE, color = 'blue') +
            geom_vline(aes(xintercept = x), linetype = 'dashed') +
            geom_hline(aes(yintercept = y), linetype = 'dashed') +
            geom_point(aes(x = x, y = y), color = 'black', size = 2) +
            facet_grid(rows = vars(Channel), scales = 'free_x') +
            ggtitle(paste0('Prediction of Sales Based on Advertising Expenses')) +
            theme(
                plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
                legend.position = 'None'
            ) 
        
        ggplotly(viz) %>%
            layout(
                margin = 100,
                xaxis = list(fixedrange = TRUE),
                yaxis = list(fixedrange = TRUE)
            ) %>%
            hide_legend()
    })
    
    observeEvent(
        c(input$channels, input$split),
        
        {        
            data <- load_data()
            channels <- input$channels
            split <- input$split / 100
            
            if (length(channels) == 0) {
                values$model = NULL
                return (NULL)
            }
            
            set.seed(88)
            values$samples <- createDataPartition(data$Sales, p = split, list = FALSE)
            
            train <- data[values$samples, ]
            
            formula <- paste0('Sales ~ ', paste0(channels, collapse = ' + '))
            values$model <- lm(formula = formula, data = train)
        }
    )
}

shinyApp(ui, server)