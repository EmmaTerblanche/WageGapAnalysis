# Post-graph Regressions

library(dplyr)

#> Dummy for years of GFC effect ====
## 2008-2013 (Esser, 2017)
appendedGHSPost <- appendedGHS

appendedGHSPost$Year <- as.numeric(paste(appendedGHSPost$Year))

class(appendedGHSPost$Year)

appendedGHSPost <- appendedGHSPost %>%
    mutate(GFC = case_when(
        appendedGHSPost$Year > 2007 & appendedGHSPost$Year < 2014 ~ 1,
        TRUE ~ 0
    ) )

#> Updated Mincer4 ====
updmincer4 <- lm(logsal ~ Race + prispline + secspline + terspline + GFC + GFC*prispline + exp + expsq + Gender + Year*Race, data = appendedGHSPost)
updmincer4 %>%
    summary()

#> Year splines ====
library(dplyr)
## Pre-2003:
appendedGHSPost <- appendedGHSPost %>%
    mutate(AAPre2003 = case_when(
        Year == 2002 ~ "0",
        Year == 2003 ~ "1",
        Year > 2003 ~ "1"
    ))
# uit los

appendedGHSPost$AAPre2003 <- as.numeric(appendedGHSPost$AAPre2003)

## 2003: Employment Equity Act + Skills Development Act
appendedGHSPost <- appendedGHSPost %>%
    mutate(AAPre2008 = case_when(
        Year < 2003 ~ "0",
        Year == 2003 ~ "1",
        Year == 2004 ~ "2",
        Year == 2005 ~ "3",
        Year == 2006 ~ "4",
        Year == 2007 ~ "5",
        Year > 2007 ~ "5"
    ))

appendedGHSPost$AAPre2008 <- as.numeric(appendedGHSPost$AAPre2008)

## 2008: Codes of Good Practice
appendedGHSPost <- appendedGHSPost %>%
    mutate(AAPre2014 = case_when(
        Year < 2008 ~ "0",
        Year == 2008 ~ "1",
        Year == 2009 ~ "2",
        Year == 2010 ~ "3",
        Year == 2011 ~ "4",
        Year == 2012 ~ "5",
        Year == 2013 ~ "6",
        Year > 2013 ~ "6"
    ))

appendedGHSPost$AAPre2014 <- as.numeric(appendedGHSPost$AAPre2014)

## 2014: Simplification of BEE scorecard
appendedGHSPost <- appendedGHSPost %>%
    mutate(AAPre2019 = case_when(
        Year < 2014 ~ "0",
        Year == 2014 ~ "1",
        Year == 2015 ~ "2",
        Year == 2016 ~ "3",
        Year == 2017 ~ "4",
        Year == 2018 ~ "5",
        Year > 2018 ~ "5"
    ))

appendedGHSPost$AAPre2019 <- as.numeric(appendedGHSPost$AAPre2019)

## 2019: Significant changes
appendedGHSPost <- appendedGHSPost %>%
    mutate(AAPost2019 = case_when(
        Year < 2019 ~ "0",
        Year == 2019 ~ "1",
        Year == 2020 ~ "2",
        Year == 2021 ~ "3",
        Year > 2021 ~ "3"
    ))

appendedGHSPost$AAPost2019 <- as.numeric(appendedGHSPost$AAPost2019)

#> Updated Mincer4 with Year Splines ====
updmincer4 <- lm(logsal ~ Race + prispline + secspline + terspline + GFC + GFC*prispline + exp + expsq + Gender + AAPre2008 + AAPre2014 + AAPre2019 + AAPost2019 + AAPre2008*Race + AAPre2014*Race + AAPre2019*Race + AAPost2019*Race, data = appendedGHSPost)
updmincer4 %>%
    summary()



#> Just Males ====
postmales <- appendedGHSPost[appendedGHSPost$Gender == 0, ]

min4males <- lm(logsal ~ Race + prispline + secspline + terspline + GFC + GFC*prispline + exp + expsq + AAPre2008 + AAPre2014 + AAPre2019 + AAPost2019 + AAPre2008*Race + AAPre2014*Race + AAPre2019*Race + AAPost2019*Race, data = postmales)
min4males %>%
    summary()


#> Just Females ====
postfemales <- appendedGHSPost[appendedGHSPost$Gender == 1, ]

min4fem <- lm(logsal ~ Race + prispline + secspline + terspline + GFC + GFC*prispline + exp + expsq + AAPre2008 + AAPre2014 + AAPre2019 + AAPost2019 + AAPre2008*Race + AAPre2014*Race + AAPre2019*Race + AAPost2019*Race, data = postfemales)
min4fem %>%
    summary()

install.packages("huxtable")
library(huxtable)
postgraphregs <- huxreg("Male Log Wages" = min4males, "Female Log Wages" = min4fem)

postgraphregs %>%
    set_caption("Linear Regression of South African Wages")

install.packages("webshot")
library(webshot)
wordreg <- huxtable::as_flextable(postgraphregs)

save_as_image(wordreg, path = "/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/PostGraphRegression.png", zoom = 3, expand = 10, webshot = "webshot")


#> Just Males With Secspline ====
min4malessec <- lm(logsal ~ Race + prispline + secspline + terspline + GFC + GFC*prispline + GFC*secspline + exp + expsq + AAPre2008 + AAPre2014 + AAPre2019 + AAPost2019 + AAPre2008*Race + AAPre2014*Race + AAPre2019*Race + AAPost2019*Race, data = postmales)
min4malessec %>%
    summary()


#> Just Females With Secspline ====
min4femsec <- lm(logsal ~ Race + prispline + secspline + terspline + GFC + GFC*prispline + GFC*secspline + exp + expsq + AAPre2008 + AAPre2014 + AAPre2019 + AAPost2019 + AAPre2008*Race + AAPre2014*Race + AAPre2019*Race + AAPost2019*Race, data = postfemales)
min4femsec %>%
    summary()

postgraphregssec <- huxreg("(1) Male Log Wages" = min4males, "(2) Male Log Wages" = min4malessec, "(3) Female Log Wages" = min4fem, "(4) Female Log Wages" = min4femsec)

wordregsec <- huxtable::as_flextable(postgraphregssec)

wordregsec %>%
    set_caption("Linear Regression of South African Wages")

install.packages("webshot")
library(webshot)


save_as_image(wordregsec, path = "/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/PostGraphRegressionWithSecSpline.png", zoom = 3, expand = 10, webshot = "webshot")


