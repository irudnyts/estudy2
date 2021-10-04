ui <- shiny::fluidPage(

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
                "Select start and end date:",
                start = "2019-01-01",
                end = "2021-01-01"
            ),

            shiny::dateRangeInput(
                "event_window",
                "Select the event window:",
                start = "2019-01-01",
                end = "2021-01-01"
            ),

            shiny::tags$hr(),

            shiny::column(
                width = 12,
                shiny::actionButton(inputId = "compute", label = "Calculate!"),
                align = "center"
            )



        ),

        mainPanel(

        )
    )
)

server <- function(input, output) {




}

shiny::shinyApp(ui = ui, server = server)
