library(rsconnect)

rsconnect::setAccountInfo(
    name = Sys.getenv("NAME"),
    token = Sys.getenv("TOKEN"),
    secret = Sys.getenv("SECRET")
)
