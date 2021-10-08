ui <- shiny::fluidPage(

    shinyjs::useShinyjs(),

    shiny::titlePanel(
        title = shiny::div(
            "Event study", shiny::img(src = "logo.png", height = 50, align = "right")
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

            shiny::tags$hr(),

            shinyWidgets::awesomeRadio(
                "compounding",
                "Select the compounding type",
                choices = c(
                    "Discrete" = "discrete",
                    "Continuous" = "continuous"
                )
            ),

            shinyWidgets::awesomeRadio(
                "multi_day",
                "Take into account rates between\n more than one day?",
                choices = c(
                    "Yes" = TRUE,
                    "No" = FALSE
                )
            ),

            shiny::tags$hr(),

            shinyWidgets::awesomeRadio(
                "model",
                "Select the market model",
                choices = c(
                    "Mean-adjusted returns model" = "mean_adj",
                    "Market-adjusted returns model" = "mrkt_adj",
                    "Single index market\n model" = "sim"
                )
            ),

            shiny::conditionalPanel(
                condition = "input.model == 'mrkt_adj' | input.model == 'sim'",
                shiny::tags$hr(),
                shiny::textInput(
                    "index",
                    "Type the ticker of the index",
                    placeholder = "NDXT"
                ),
            ),

            shiny::tags$hr(),

            shiny::dateRangeInput(
                "estmation_window",
                "Select the estimation window:"
            ),

            shiny::dateRangeInput(
                "event_window",
                "Select the event window:"
            ),

            shiny::tags$hr(),

            shiny::column(
                width = 12,
                shiny::actionButton(inputId = "compute", label = "Calculate!"),
                align = "center"
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

    shiny::observeEvent(
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

}

shiny::shinyApp(ui = ui, server = server)
