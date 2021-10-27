ui <- shiny::fluidPage(

    shinyjs::useShinyjs(),

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

            shiny::column(
                width = 12,
                shiny::actionButton("download", "Download", width = "100%"),
                align = "center"
            ),


            shiny::tags$hr(),

            shinyjs::disabled(
                shinyWidgets::awesomeRadio(
                    "compounding",
                    "Select the compounding type",
                    choices = c(
                        "Discrete" = "discrete",
                        "Continuous" = "continuous"
                    )
                )
            ),

            shinyjs::disabled(
                shinyWidgets::awesomeRadio(
                    "multi_day",
                    "Take into account rates between\n more than one day?",
                    choices = c(
                        "Yes" = TRUE,
                        "No" = FALSE
                    )
                )
            ),

            shiny::tags$hr(),

            shinyjs::disabled(
                shinyWidgets::awesomeRadio(
                    "model",
                    "Select the market model",
                    choices = c(
                        "Mean-adjusted returns model" = "mean_adj",
                        "Market-adjusted returns model" = "mrkt_adj",
                        "Single index market\n model" = "sim"
                    )
                )
            ),


            shiny::conditionalPanel(
                condition =
                    "input.model == 'mrkt_adj' | input.model == 'sim'",
                shiny::tags$hr(),
                shiny::textInput(
                    "index",
                    "Type the ticker of the index",
                    placeholder = "^GSPC"
                ),
            )
            ,

            shiny::tags$hr(),

            shinyjs::disabled(
                shiny::dateRangeInput(
                    "estmation_window",
                    "Select the estimation window:"
                )
            ),

            shinyjs::disabled(
                shiny::dateRangeInput(
                    "event_window",
                    "Select the event window:"
                )
            ),

            shiny::tags$hr(),

            shinyjs::disabled(
                shiny::column(
                    width = 12,
                    shiny::actionButton(
                        "compute",
                        "Calculate!",
                        width = "100%"
                    ),
                    align = "center"
                )
            )

        ),

        mainPanel(
            shiny::tabsetPanel(
                shiny::tabPanel(
                    title = "Parametric test"
                ),
                shiny::tabPanel(
                    title = "Nonparametric test"
                ),
                shiny::tabPanel(
                    title = "Plots"
                )
            )
        )
    )
)

server <- function(input, output, session) {

    prices <- shiny::eventReactive(input$download, {

        req(input$date_range[1] < input$date_range[2])
        # XXX: make a pop up window if it is not true

        req(input$tickers)
        # XXX: also validate that indeed can download these tickers

        tickers <- input$tickers %>%
            stringr::str_split(",") %>%
            unlist() %>%
            stringr::str_trim(side = "both")

        get_prices_from_tickers(
            tickers,
            start = input$date_range[1],
            end = input$date_range[2],
            quote = input$price_type,
            retclass = "zoo"
        )

    })

    rates <- shiny::reactive({
        # XXX: do we need req(prices())? Looks like no!
        get_rates_from_prices(
            prices = prices(),
            quote = input$price_type,
            multi_day = input$multi_day,
            compounding = input$compounding
        )
    })



    shiny::observeEvent(input$download, {

        req(prices())

        shinyjs::enable("compounding")
        shinyjs::enable("multi_day")
        shinyjs::enable("model")
        shinyjs::enable("estmation_window")
        shinyjs::enable("event_window")
        shinyjs::enable("compute")

    })

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

    shiny::observeEvent(input$compute, {

        tickers <- input$tickers %>%
            stringr::str_split(",") %>%
            unlist() %>%
            stringr::str_trim(side = "both")

        if (input$model != "mean_adj") {
            rates_indx <- get_prices_from_tickers(
                input$index,
                start = input$date_range[1],
                end = input$date_range[2],
                quote = input$price_type,
                retclass = "zoo"
            ) %>%
                get_rates_from_prices(
                    quote = input$price_type,
                    multi_day = input$multi_day,
                    compounding = input$compounding
                )
        }

        results <- get_prices_from_tickers(
            tickers,
            start = input$date_range[1],
            end = input$date_range[2],
            quote = input$price_type,
            retclass = "zoo"
        ) %>%
            get_rates_from_prices(
                quote = input$price_type,
                multi_day = input$multi_day,
                compounding = input$compounding
            ) %>%
            apply_market_model(
                regressor = rates_indx,
                same_regressor_for_all = TRUE,
                market_model = input$model,
                estimation_method = "ols",
                estimation_start = input$estmation_window[1],
                estimation_end = input$estmation_window[2]
            ) %>%
            parametric_tests(
                event_start = input$event_window[1],
                event_end = input$event_window[2]
            )

        print(results)


    })

}

shiny::shinyApp(ui = ui, server = server)
