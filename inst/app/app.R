ui <- shiny::fluidPage(

    theme = bslib::bs_theme(version = 4, bootswatch = "solar"),

    shiny::sidebarLayout(

        shiny::sidebarPanel(

            shiny::textInput(
                "tickers",
                "Please list tickers separated by comma:",
                placeholder = "AAPL, MSFT, AMZN, FB, GOOGL, NVDA"
            ),

            shiny::dateRangeInput(
                "date_range",
                "Please select start and end date:",
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
            )

        ),

        mainPanel(

        )
    )
)

server <- function(input, output) {



}

shiny::shinyApp(ui = ui, server = server)
