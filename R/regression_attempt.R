# Attempt to implement GLS model from the package 'nlme'
library(nlme)
library(magrittr)
library(ggplot2)

data(cars)

ggplot(cars, mapping = aes())

View(mpg)

mpg %>%
    ggplot(aes(displ, cty))+
    geom_point(aes(colour = drv,
                   size = trans),
               alpha = 0.5)+
    geom_smooth(method = lm)+
    # facet_wrap(~year, nrow = 1)+
    labs(x = "Engine size",
         y = "MPG in the city",
         title = "Fuel efficiency")+
    theme_bw()

x <- mpg$displ
y <- as.double(mpg$cty)

gls_fit <- nlme::gls(model = y ~ x,
                     data = mpg,
                     correlation = NULL,
                     weights = NULL
)

lm_fit <- lm(y ~ x)
plot(mpg$displ ~ mpg$cty, data = mpg)
abline(lm_fit)
abline(gls_fit)


lm_resid <- lm_fit %>% resid
gls_resid <- gls_fit %>% resid

lm_fit %>% resid %>% density %>% plot
gls_fit %>% resid %>% density %>% plot

lm_resid %>% qqnorm %>% plot
gls_resid %>% qqnorm %>% plot

lm_resid %>% qqline %>% plot
gls_resid %>%  qqline %>% plot


