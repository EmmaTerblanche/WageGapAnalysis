

#> Time Cohort Regression: Females ====
by_postfem_real <- postfemales_real %>%
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

byfemreg <- lm(formula = logsal ~ Race + prispline + secspline + terspline + GFC + GFC*prispline + GFC*secspline + exp + expsq + AAPre2008 + AAPre2014 + AAPre2019 + AAPost2019 + AAPre2008*Race + AAPre2014*Race + AAPre2019*Race + AAPost2019*Race + by1940 + by1950 + by1960 + by1970 + by1980 + by1990 + by1940*Race + by1950*Race + by1960*Race + by1970*Race + by1980*Race + by1990*Race, data = by_postfem_real, weights = Weight)
byfemreg %>%
    summary()


byfem_x <- by_postfem_real %>%
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

byfem_x <- CJ(Race = c("African/Black", "Coloured", "Indian/Asian", "White"), birthyear = seq(1940,2000)) %>%
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


byfem_x$y <- predict.lm(object = byfemreg, newdata = byfem_x)


byfem_x %>%
    ggplot(aes(y = y, x= birthyear, group = Race, color = Race)) +
    geom_line()

library(tidyverse)
byfem1 <- byfem_x %>%
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

byfem_noblack <- byfem1[byfem1$Race != "African",]


byfem_noblack %>%
    ggplot(aes(y = wage_gap, x= birthyear, group = Race, color = Race)) +
    geom_line() +
    labs(x='Birth Year', y='Wage Gap')



