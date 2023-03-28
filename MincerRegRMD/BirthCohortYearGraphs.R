# Birth Cohort Wage Gap - Year Graphs

CPI <- read.csv("/Users/mac/Downloads/ZAFCPIALLMINMEI-2.csv") %>%
    rename(CPI = ZAFCPIALLMINMEI) %>%
    mutate(Year = year(DATE)) %>%
    mutate(CPI = CPI*100/131.70924) %>%
    select(Year, CPI)

#> Birth Cohort-Wage Gap-Year Regression - Females ====
library(tibble)
library(tidyverse)
wgy_fem_x <- by_postfem_real %>%
    select(Race, Year, AAPre2008, AAPre2014, AAPre2019, AAPost2019) %>%
    unique() %>%
    add_column(exp = 0) %>%
    add_column(expsq = 0) %>%
    add_column(prispline = 0) %>%
    add_column(secspline = 0) %>%
    add_column(terspline = 0) %>%
    add_column(GFC = 0) %>%
    add_column(by1940 = 0) %>%
    add_column(by1950 = 0) %>%
    add_column(by1960 = 0) %>%
    add_column(by1970 = 0) %>%
    add_column(by1980 = 0) %>%
    add_column(by1990 = 0)

postfemales_wgy <- postfemales %>%
    left_join(CPI, by = "Year") %>%
    mutate(logsal = log(exp(logsal)*100/CPI))

byfemreg <- lm(formula = logsal ~ Race + prispline + secspline + terspline + GFC + GFC*prispline + GFC*secspline + exp + expsq + AAPre2008 + AAPre2014 + AAPre2019 + AAPost2019 + AAPre2008*Race + AAPre2014*Race + AAPre2019*Race + AAPost2019*Race + by1940 + by1950 + by1960 + by1970 + by1980 + by1990 + by1940*Race + by1950*Race + by1960*Race + by1970*Race + by1980*Race + by1990*Race, data = by_postfem_real, weights = Weight)
byfemreg %>%
    summary()


wgy_fem_x$y <- predict.lm(object = byfemreg, newdata = wgy_fem_x)


wgy_fem_x %>%
    ggplot(aes(y = y, x= Year, group = Race, color = Race)) +
    geom_line()

library(tidyverse)
wgyfem1 <- wgy_fem_x %>%
    select(Race, Year, y) %>%
    mutate(
        Race = case_when(
            Race == "Indian/Asian" ~ "Indian",
            Race == "African/Black" ~ "African",
            TRUE ~ Race
        )
    ) %>%
    pivot_wider(names_from = Race, values_from = y) %>%
    mutate(
        Indian = Indian - African,
        Coloured = Coloured - African,
        White = White - African,
        African = African - African
    ) %>%
    pivot_longer(c(Indian, Coloured, White, African), names_to = "Race", values_to = "wage_gap")

wgyfem_noblack <- wgyfem1[wgyfem1$Race != "African",]


wgyfem_noblack %>%
    ggplot(aes(y = wage_gap, x= Year, group = Race, color = Race)) +
    geom_line() +
    labs(x='Year', y='Wage Gap')


#> Birth Cohort-Wage Gap-Year - Males ====
wgy_male_x <- by_postmales_real%>%
    select(Race, Year, AAPre2008, AAPre2014, AAPre2019, AAPost2019) %>%
    unique() %>%
    add_column(exp = 0) %>%
    add_column(expsq = 0) %>%
    add_column(prispline = 0) %>%
    add_column(secspline = 0) %>%
    add_column(terspline = 0) %>%
    add_column(GFC = 0) %>%
    add_column(by1940 = 0) %>%
    add_column(by1950 = 0) %>%
    add_column(by1960 = 0) %>%
    add_column(by1970 = 0) %>%
    add_column(by1980 = 0) %>%
    add_column(by1990 = 0)

postmales_wgy <- postmales %>%
    left_join(CPI, by = "Year") %>%
    mutate(logsal = log(exp(logsal)*100/CPI))

bymalereg <- lm(formula = logsal ~ Race + prispline + secspline + terspline + GFC + GFC*prispline + GFC*secspline + exp + expsq + AAPre2008 + AAPre2014 + AAPre2019 + AAPost2019 + AAPre2008*Race + AAPre2014*Race + AAPre2019*Race + AAPost2019*Race + by1940 + by1950 + by1960 + by1970 + by1980 + by1990 + by1940*Race + by1950*Race + by1960*Race + by1970*Race + by1980*Race + by1990*Race, data = by_postmales_real, weights = Weight)
bymalereg %>%
    summary()


wgy_male_x$y <- predict.lm(object = bymalereg, newdata = wgy_male_x)


wgy_male_x %>%
    ggplot(aes(y = y, x= Year, group = Race, color = Race)) +
    geom_line()

library(tidyverse)
wgymale1 <- wgy_male_x %>%
    select(Race, Year, y) %>%
    mutate(
        Race = case_when(
            Race == "Indian/Asian" ~ "Indian",
            Race == "African/Black" ~ "African",
            TRUE ~ Race
        )
    ) %>%
    pivot_wider(names_from = Race, values_from = y) %>%
    mutate(
        Indian = Indian - African,
        Coloured = Coloured - African,
        White = White - African,
        African = African - African
    ) %>%
    pivot_longer(c(Indian, Coloured, White, African), names_to = "Race", values_to = "wage_gap")

wgymale_noblack <- wgymale1[wgymale1$Race != "African",]


wgymale_noblack %>%
    ggplot(aes(y = wage_gap, x= Year, group = Race, color = Race)) +
    geom_line() +
    labs(x='Year', y='Wage Gap')

#> Output ====
rw_reg<- huxreg("(1) Male Log of Real Wages" = male_rw_reg, "(2) Male Log of Real Wages" = bymalereg, "(3) Female Log of Real Wages" = fem_rw_reg, "(4) Female Log of Real Wages" = byfemreg)

real_reg <- huxtable::as_flextable(rw_reg)

real_reg %>%
    set_caption("Linear Regression of South African Wages")

install.packages("webshot")
library(webshot)


save_as_image(real_reg, path = "/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/RealWageRegs.png", zoom = 3, expand = 10, webshot = "webshot")



#> Name Changes ====
install.packages("stargazer")
library(stargazer)
stargazer(postmales_real, postfemales_real, type="html",
          dep.var.labels=c("Male Log of Real Wages","Female Log of Real Wages"), covariate.labels=c("Coloured", "Indian/Asian", "White", "Primary education", "Secondary education", "Tertiary education", "GFC", "Experience", "Experience-squared", "AA: 2003-2007", "AA: 2008-2013", "AA: 2014-2018", "AA: 2019-2021", "Primary * GFC", "Secondary * GFC", "Coloured * AA: 2003-2007", "Indian/Asian * AA: 2003-2007", "White * AA: 2003-2007", "Coloured * AA: 2008-2013", "Indian/Asian * AA: 2008-2013", "White * AA: 2008-2013", "Coloured * AA: 2014-2018", "Indian/Asian * AA: 2014-2018", "White * AA: 2014-2018", "Coloured * AA: 2019-2021", "Indian/Asian * AA: 2019-2021", "White * AA: 2019-2021"), out="RealWageModels.htm")


realwageregs <- huxreg("Male Log of Real Wages" = postmales_real, "Female Log of Real Wages" = postfemales_real, custom.coef.names = c("Intercept","Coloured", "Indian/Asian", "White", "Primary education", "Secondary education", "Tertiary education", "GFC", "Experience", "Experience-squared", "AA: 2003-2007", "AA: 2008-2013", "AA: 2014-2018", "AA: 2019-2021", "Primary * GFC", "Secondary * GFC", "Coloured * AA: 2003-2007", "Indian/Asian * AA: 2003-2007", "White * AA: 2003-2007", "Coloured * AA: 2008-2013", "Indian/Asian * AA: 2008-2013", "White * AA: 2008-2013", "Coloured * AA: 2014-2018", "Indian/Asian * AA: 2014-2018", "White * AA: 2014-2018", "Coloured * AA: 2019-2021", "Indian/Asian * AA: 2019-2021", "White * AA: 2019-2021"))


c("Intercept" = "(Intercept)","Coloured" = "RaceColoured ", "Indian/Asian" = "RaceIndian/Asian ", "White" = "RaceWhite", "Primary education" = "prispline", "Secondary education", "Tertiary education", "GFC", "Experience", "Experience-squared", "AA: 2003-2007", "AA: 2008-2013", "AA: 2014-2018", "AA: 2019-2021", "Primary * GFC", "Secondary * GFC", "Coloured * AA: 2003-2007", "Indian/Asian * AA: 2003-2007", "White * AA: 2003-2007", "Coloured * AA: 2008-2013", "Indian/Asian * AA: 2008-2013", "White * AA: 2008-2013", "Coloured * AA: 2014-2018", "Indian/Asian * AA: 2014-2018", "White * AA: 2014-2018", "Coloured * AA: 2019-2021", "Indian/Asian * AA: 2019-2021", "White * AA: 2019-2021"))


realwageregs <- huxreg("Male Log of Real Wages" = postmales_real, "Female Log of Real Wages" = postfemales_real, coefs = c("Intercept" = "(Intercept)","Coloured" = "RaceColoured ", "Indian/Asian" = "RaceIndian/Asian ", "White" = "RaceWhite", "Primary education" = "prispline"))

wordregsec <- huxtable::as_flextable(realwageregs)

realwageregs %>%
    set_caption("Linear Regression of South African Wages")

install.packages("webshot")
library(webshot)


save_as_image(realwageregs, path = "/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/PostGraphRegressionWithSecSpline.png", zoom = 3, expand = 10, webshot = "webshot")
