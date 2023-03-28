x <- appendedGHS %>%
    select(Race, Year) %>%
    unique() %>%
    add_column(exp = 0) %>%
    add_column(expsq = 0) %>%
    add_column(Gender = 0) %>%
    add_column(prispline = 0) %>%
    add_column(secspline = 0) %>%
    add_column(terspline = 0)

new_x$y <- predict.lm(object = mincer4, newdata = new_x)



#> Time Cohort Regression: Males ====
by_postmales_real <- postmales_real %>%
    mutate(birthyear = Year - Age) %>%
    filter(birthyear >= 1940 & birthyear <= 2000) %>%
    mutate(
        by1940 = if_else(birthyear < 1950, birthyear - 1940, 10),
        by1950 = if_else(birthyear < 1950, 0, if_else(birthyear < 1960, birthyear - 1950, 10)),
        by1960 = if_else(birthyear < 1960, 0, if_else(birthyear < 1970, birthyear - 1960, 10)),
        by1970 = if_else(birthyear < 1970, 0, if_else(birthyear < 1980, birthyear - 1970, 10)),
        by1980 = if_else(birthyear < 1980, 0, if_else(birthyear < 1990, birthyear - 1980, 10)),
        by1990 = if_else(birthyear < 1990, 0, if_else(birthyear < 2000, birthyear - 1990, 10))
    )

bymalereg <- lm(formula = logsal ~ Race + prispline + secspline + terspline + GFC + GFC*prispline + GFC*secspline + exp + expsq + AAPre2008 + AAPre2014 + AAPre2019 + AAPost2019 + AAPre2008*Race + AAPre2014*Race + AAPre2019*Race + AAPost2019*Race + by1940 + by1950 + by1960 + by1970 + by1980 + by1990 + by1940*Race + by1950*Race + by1960*Race + by1970*Race + by1980*Race + by1990*Race, data = by_postmales_real, weights = Weight)
bymalereg %>%
    summary()


bymale_x <- by_postmales_real %>%
    select(Race, birthyear, by1940, by1950, by1960, by1970, by1980, by1990) %>%
    unique() %>%
    add_column(exp = 0) %>%
    add_column(expsq = 0) %>%
    add_column(prispline = 0) %>%
    add_column(secspline = 0) %>%
    add_column(terspline = 0) %>%
    add_column(GFC = 0) %>%
    add_column(AAPre2008 = 0) %>%
    add_column(AAPre2014 = 0) %>%
    add_column(AAPre2019 = 0) %>%
    add_column(AAPost2019 = 0)

bymale_x <- CJ(Race = c("African/Black", "Coloured", "Indian/Asian", "White"), birthyear = seq(1940,2000)) %>%
    mutate(
        by1940 = if_else(birthyear < 1950, birthyear - 1940, 10),
        by1950 = if_else(birthyear < 1950, 0, if_else(birthyear < 1960, birthyear - 1950, 10)),
        by1960 = if_else(birthyear < 1960, 0, if_else(birthyear < 1970, birthyear - 1960, 10)),
        by1970 = if_else(birthyear < 1970, 0, if_else(birthyear < 1980, birthyear - 1970, 10)),
        by1980 = if_else(birthyear < 1980, 0, if_else(birthyear < 1990, birthyear - 1980, 10)),
        by1990 = if_else(birthyear < 1990, 0, if_else(birthyear < 2000, birthyear - 1990, 10)) ) %>%
    add_column(exp = 0) %>%
    add_column(expsq = 0) %>%
    add_column(prispline = 0) %>%
    add_column(secspline = 0) %>%
    add_column(terspline = 0) %>%
    add_column(GFC = 0) %>%
    add_column(AAPre2008 = 0) %>%
    add_column(AAPre2014 = 0) %>%
    add_column(AAPre2019 = 0) %>%
    add_column(AAPost2019 = 0)


bymale_x$y <- predict.lm(object = bymalereg, newdata = bymale_x)


bymale_x %>%
    ggplot(aes(y = y, x= birthyear, group = Race, color = Race)) +
    geom_line()

library(tidyverse)
bymale1 <- bymale_x %>%
    select(Race, birthyear, y) %>%
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

bymale_noblack <- bymale1[bymale1$Race != "African",]

bymale_noblack %>%
    ggplot(aes(y = wage_gap, x= birthyear, group = Race, color = Race)) +
    geom_line() +
    labs(x='Birth Year', y='Wage Gap')

#> Wage Gap-Year Graph ====





bymale_x$y <- predict.lm(object = bymalereg, newdata = bymale_x)


bymale_x %>%
    ggplot(aes(y = y, x= Year, group = Race, color = Race)) +
    geom_line()

library(tidyverse)
bymale1 <- bymale_x %>%
    select(Race, birthyear, y) %>%
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

bymale_noblack <- bymale1[bymale1$Race != "African",]

bymale_noblack %>%
    ggplot(aes(y = wage_gap, x= birthyear, group = Race, color = Race)) +
    geom_line() +
    labs(x='Birth Year', y='Wage Gap')


new_x$y <- predict.lm(object = mincer4, newdata = new_x)