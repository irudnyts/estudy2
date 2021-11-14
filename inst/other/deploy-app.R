library(rsconnect)

rsconnect::setAccountInfo(
    name = Sys.getenv("NAME"),
    token = Sys.getenv("TOKEN"),
    secret = Sys.getenv("SECRET")
)

deployApp(
    appDir = "inst/app/",
    appName = "estudy2",
    account = Sys.getenv("ACCOUNT")
)
