source("helpers.R")

ui <- shiny::fluidPage(
    # waiter::use_waitress(),
    # shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),

    shiny::titlePanel(
        title = shiny::div(
            "Event study",
            shiny::img(src = "logo.png", height = 50, align = "right")
        )
    ),

    shiny::tags$hr(),

    theme = bslib::bs_theme(version = 4, bootswatch = "solar"),

    shiny::sidebarLayout(

        shiny::sidebarPanel(

            shiny::textInput(
                "tickers",
                "List tickers separated by comma:",
                placeholder = "AAPL, MSFT, AMZN, FB, GOOGL, NVDA"
            ),

            shiny::dateRangeInput(
                "date_range",
                "Select start and end date:",
                start = "2019-01-01",
                end = "2021-01-01"
            ),

            shinyWidgets::awesomeRadio(
                "price_type",
                "Select the price type",
                choices = c("Close", "Open")
            ),

            # shiny::column(
            #     width = 12,
            #     shiny::actionButton("download", "Download", width = "100%"),
            #     align = "center"
            # ),


            shiny::tags$hr(),

            # shinyjs::disabled(
                shinyWidgets::awesomeRadio(
                    "compounding",
                    "Select the compounding type",
                    choices = c(
                        "Discrete" = "discrete",
                        "Continuous" = "continuous"
                    )
                ),
            # ),

            # shinyjs::disabled(
                shinyWidgets::awesomeRadio(
                    "multi_day",
                    "Take into account rates between\n more than one day?",
                    choices = c(
                        "Yes" = TRUE,
                        "No" = FALSE
                    )
                ),
            # ),

            shiny::tags$hr(),

            # shinyjs::disabled(
                shinyWidgets::awesomeRadio(
                    "model",
                    "Select the market model",
                    choices = c(
                        "Mean-adjusted returns model" = "mean_adj",
                        "Market-adjusted returns model" = "mrkt_adj",
                        "Single index market\n model" = "sim"
                    )
                ),
            # ),


            shiny::conditionalPanel(
                condition =
                    "input.model == 'mrkt_adj' | input.model == 'sim'",
                shiny::tags$hr(),
                shiny::textInput(
                    "index",
                    "Type the ticker of the index",
                    placeholder = "^GSPC"
                ),
                # Add a button to allow downloading
            ),

            shiny::tags$hr(),

            # shinyjs::disabled(
                shiny::dateRangeInput(
                    "estmation_window",
                    "Select the estimation window:"
                ),
            # ),

            # shinyjs::disabled(
                shiny::dateRangeInput(
                    "event_window",
                    "Select the event window:"
                ),
            # )

            shiny::column(
                width = 12,
                shiny::actionButton("calculate", "Calculate!", width = "100%"),
                align = "center"
            )

        ),

        mainPanel(
            shiny::tabsetPanel(
                shiny::tabPanel(
                    title = "Parametric test",
                    DT::dataTableOutput("parametric_table")

                ),
                shiny::tabPanel(
                    title = "Nonparametric test",
                    DT::dataTableOutput("nonparametric_table")
                )
            )
        )
    )
)

server <- function(input, output, session) {

    prices <- shiny::eventReactive(input$calculate, {

        shinyFeedback::feedbackWarning(
            "date_range",
            input$date_range[1] >= input$date_range[2],
            "Please make sure the start date is lower than the end date."
        )

        shiny::req(input$date_range[1] < input$date_range[2])

        shinyFeedback::feedbackWarning(
            "tickers",
            input$tickers == "",
            "Please specify tickers separated by comma."
        )

        shiny::req(input$tickers)

        tickers <- input$tickers %>%
            stringr::str_split(",") %>%
            unlist() %>%
            stringr::str_trim(side = "both")

        prices_list <- list()

        withProgress(message = "Downloading prices...", {

            for (ticker in tickers) {

                prices_list[[ticker]] <- download_prices(
                    ticker,
                    start = input$date_range[1],
                    end = input$date_range[2],
                    quote = input$price_type,
                    retclass = "list"
                ) %>%
                    purrr::pluck(1)

                if (is.null(prices_list[[ticker]])) {
                    shiny::showNotification(
                        paste0(
                            "Could not retrieve prices for ",
                            ticker,
                            ". This security will be ignored."
                        ),
                        type = "warning"
                    )
                }

                incProgress(1 / length(tickers))

            }

        })

        shinyFeedback::feedbackWarning(
            "tickers",
            length(prices_list) < 2,
            "Please make sure that at least two valid tickers are specified."
        )

        shiny::req(length(prices_list) > 1)

        prices_list

    })

    rates <- shiny::eventReactive(input$calculate, {
        get_rates_from_prices(
            prices = prices(),
            quote = input$price_type,
            multi_day = input$multi_day,
            compounding = input$compounding
        )
    })

    rates_indx <- shiny::eventReactive(input$calculate, {

        if (input$model != "mean_adj") {

            shinyFeedback::feedbackWarning(
                "index",
                input$index == "",
                "Please specify a valid ticker for the market index."
            )

            shiny::req(input$index)

            prices_indx <- download_prices(
                input$index,
                start = input$date_range[1],
                end = input$date_range[2],
                quote = input$price_type,
                retclass = "zoo"
            )

            shinyFeedback::feedbackWarning(
                "index",
                is.null(prices_indx),
                "Please specify a valid ticker for the market index."
            )

            shiny::req(!is.null(prices_indx))

            get_rates_from_prices(
                prices_indx,
                quote = input$price_type,
                multi_day = input$multi_day,
                compounding = input$compounding
            )
        } else {
            NULL
        }
    })

    stock_returns <- shiny::eventReactive(input$calculate, {
        apply_market_model(
            rates = rates(),
            regressor = rates_indx(),
            same_regressor_for_all = TRUE,
            market_model = input$model,
            estimation_method = "ols",
            estimation_start = input$estmation_window[1],
            estimation_end = input$estmation_window[2]
        )
    })

    output$parametric_table <- DT::renderDataTable({

        # req(stock_returns())

        parametric_tests(
            list_of_returns = stock_returns(),
            event_start = isolate(input$event_window[1]),
            event_end = isolate(input$event_window[2])
        ) %>%
            beautify() %>%
            formattable::as.datatable(options = list(scrollX = TRUE))

    })

    output$nonparametric_table <- DT::renderDataTable({

        nonparametric_tests(
            list_of_returns = stock_returns(),
            event_start = isolate(input$event_window[1]),
            event_end = isolate(input$event_window[2])
        ) %>%
            beautify() %>%
            formattable::as.datatable(options = list(scrollX = TRUE))

    })


    # Interactions between UI elements
    #---------------------------------------------------------------------------

    # shiny::observeEvent(input$download, {
    #
    #     req(prices())
    #
    #     shinyjs::enable("compounding")
    #     shinyjs::enable("multi_day")
    #     shinyjs::enable("model")
    #     shinyjs::enable("estmation_window")
    #     shinyjs::enable("event_window")
    #
    # })

    shiny::observeEvent( # XXX: can we just use `observe()`?
        input$date_range, {

            estimation_window_length <- 2 / 3 *
                as.numeric(input$date_range[2] - input$date_range[1])

            shiny::updateDateRangeInput(
                session = session,
                inputId = "estmation_window",
                start = input$date_range[1],
                end = input$date_range[1] + estimation_window_length,
                min = input$date_range[1],
                max = input$date_range[2]
            )
        }

    )

    shiny::observeEvent(
        input$estmation_window,
        shiny::updateDateRangeInput(
            session = session,
            inputId = "event_window",
            start = input$estmation_window[2] + 1,
            end = input$date_range[2],
            min = input$estmation_window[2] + 1,
            max = input$date_range[2]
        )
    )

    #---------------------------------------------------------------------------
    # Bookmarking

    observe({
        reactiveValuesToList(input)
        session$doBookmark()
    })

    onBookmarked(updateQueryString)

}

shiny::shinyApp(ui = ui, server = server, enableBookmarking = "url")
