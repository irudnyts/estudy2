ui <- shiny::fluidPage(
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

            shinyWidgets::switchInput(
                "price_type",
                onLabel = "Open",
                offLabel = "Close"
            )

        ),
        mainPanel(

        )
    )
)

server <- function(input, output) {

}

shiny::shinyApp(ui = ui, server = server)
