# Real Wages

CPI <- read.csv("/Users/mac/Downloads/ZAFCPIALLMINMEI-2.csv") %>%
    rename(CPI = ZAFCPIALLMINMEI) %>%
    mutate(Year = year(DATE)) %>%
    mutate(CPI = CPI*100/131.70924) %>%
    select(Year, CPI)

#> Post-graph Regression - Females ====
postfemales_real <- postfemales %>%
    left_join(CPI, by = "Year") %>%
    mutate(logsal = log(exp(logsal)*100/CPI))

fem_rw_reg <- lm(logsal ~ Race + prispline + secspline + terspline + GFC + GFC*prispline + GFC*secspline + exp + expsq + AAPre2008 + AAPre2014 + AAPre2019 + AAPost2019 + AAPre2008*Race + AAPre2014*Race + AAPre2019*Race + AAPost2019*Race, data = postfemales_real, weights = Weight)
fem_rw_reg %>%
    summary()

yearfem_x <- postfemales_real %>%
    select(Race, Year) %>%
    unique() %>%
    mutate(AAPre2008 = case_when(
        Year < 2003 ~ "0",
        Year == 2003 ~ "1",
        Year == 2004 ~ "2",
        Year == 2005 ~ "3",
        Year == 2006 ~ "4",
        Year == 2007 ~ "5",
        Year > 2007 ~ "5"
    )) %>%
    mutate(AAPre2014 = case_when(
        Year < 2008 ~ "0",
        Year == 2008 ~ "1",
        Year == 2009 ~ "2",
        Year == 2010 ~ "3",
        Year == 2011 ~ "4",
        Year == 2012 ~ "5",
        Year == 2013 ~ "6",
        Year > 2013 ~ "6"
    )) %>%
    mutate(AAPre2019 = case_when(
        Year < 2014 ~ "0",
        Year == 2014 ~ "1",
        Year == 2015 ~ "2",
        Year == 2016 ~ "3",
        Year == 2017 ~ "4",
        Year == 2018 ~ "5",
        Year > 2018 ~ "5"
    )) %>%
    mutate(AAPost2019 = case_when(
        Year < 2019 ~ "0",
        Year == 2019 ~ "1",
        Year == 2020 ~ "2",
        Year == 2021 ~ "3",
        Year > 2021 ~ "3"
    )) %>%
    mutate(
        AAPre2008 = as.numeric(AAPre2008),
        AAPre2014 = as.numeric(AAPre2014),
        AAPre2019 = as.numeric(AAPre2019),
        AAPost2019 = as.numeric(AAPost2019),
    ) %>%
    add_column(exp = 0) %>%
    add_column(expsq = 0) %>%
    add_column(prispline = 0) %>%
    add_column(secspline = 0) %>%
    add_column(terspline = 0) %>%
    add_column(GFC = 0)

yearfem_x <- CJ(Race = c("African/Black", "Coloured", "Indian/Asian", "White"), Year = seq(2002,2021)) %>%
    mutate(AAPre2008 = case_when(
        Year < 2003 ~ "0",
        Year == 2003 ~ "1",
        Year == 2004 ~ "2",
        Year == 2005 ~ "3",
        Year == 2006 ~ "4",
        Year == 2007 ~ "5",
        Year > 2007 ~ "5"
    )) %>%
    mutate(AAPre2014 = case_when(
        Year < 2008 ~ "0",
        Year == 2008 ~ "1",
        Year == 2009 ~ "2",
        Year == 2010 ~ "3",
        Year == 2011 ~ "4",
        Year == 2012 ~ "5",
        Year == 2013 ~ "6",
        Year > 2013 ~ "6"
    )) %>%
    mutate(AAPre2019 = case_when(
        Year < 2014 ~ "0",
        Year == 2014 ~ "1",
        Year == 2015 ~ "2",
        Year == 2016 ~ "3",
        Year == 2017 ~ "4",
        Year == 2018 ~ "5",
        Year > 2018 ~ "5"
    )) %>%
    mutate(AAPost2019 = case_when(
        Year < 2019 ~ "0",
        Year == 2019 ~ "1",
        Year == 2020 ~ "2",
        Year == 2021 ~ "3",
        Year > 2021 ~ "3"
    )) %>%
    mutate(
        AAPre2008 = as.numeric(AAPre2008),
        AAPre2014 = as.numeric(AAPre2014),
        AAPre2019 = as.numeric(AAPre2019),
        AAPost2019 = as.numeric(AAPost2019),
    ) %>%
    add_column(exp = 0) %>%
    add_column(expsq = 0) %>%
    add_column(prispline = 0) %>%
    add_column(secspline = 0) %>%
    add_column(terspline = 0) %>%
    add_column(GFC = 0)


yearfem_x$y <- predict.lm(object = fem_rw_reg, newdata = yearfem_x)


yearfem_x %>%
    ggplot(aes(y = y, x= Year, group = Race, color = Race)) +
    geom_line()

library(tidyverse)
yearfem1 <- yearfem_x %>%
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

yearfem_noblack <- yearfem1[yearfem1$Race != "African",]


yearfem_noblack %>%
    ggplot(aes(y = wage_gap, x= Year, group = Race, color = Race)) +
    geom_line() +
    labs(x='Year', y='Wage Gap')


#> Post-Graph Regression - Males ====
postmales_real <- postmales %>%
    left_join(CPI, by = "Year") %>%
    mutate(logsal = log(exp(logsal)*100/CPI))

male_rw_reg <- lm(logsal ~ Race + prispline + secspline + terspline + GFC + GFC*prispline + GFC*secspline + exp + expsq + AAPre2008 + AAPre2014 + AAPre2019 + AAPost2019 + AAPre2008*Race + AAPre2014*Race + AAPre2019*Race + AAPost2019*Race, data = postmales_real, weights = Weight)
male_rw_reg %>%
    summary()

yearmale_x <- postmales_real %>%
    select(Race, Year, AAPre2008, AAPre2014, AAPre2019, AAPost2019) %>%
    unique() %>%
    mutate(AAPre2008 = case_when(
        Year < 2003 ~ "0",
        Year == 2003 ~ "1",
        Year == 2004 ~ "2",
        Year == 2005 ~ "3",
        Year == 2006 ~ "4",
        Year == 2007 ~ "5",
        Year > 2007 ~ "5"
    )) %>%
    mutate(AAPre2014 = case_when(
        Year < 2008 ~ "0",
        Year == 2008 ~ "1",
        Year == 2009 ~ "2",
        Year == 2010 ~ "3",
        Year == 2011 ~ "4",
        Year == 2012 ~ "5",
        Year == 2013 ~ "6",
        Year > 2013 ~ "6"
    )) %>%
    mutate(AAPre2019 = case_when(
        Year < 2014 ~ "0",
        Year == 2014 ~ "1",
        Year == 2015 ~ "2",
        Year == 2016 ~ "3",
        Year == 2017 ~ "4",
        Year == 2018 ~ "5",
        Year > 2018 ~ "5"
    )) %>%
    mutate(AAPost2019 = case_when(
        Year < 2019 ~ "0",
        Year == 2019 ~ "1",
        Year == 2020 ~ "2",
        Year == 2021 ~ "3",
        Year > 2021 ~ "3"
    )) %>%
    mutate(
        AAPre2008 = as.numeric(AAPre2008),
        AAPre2014 = as.numeric(AAPre2014),
        AAPre2019 = as.numeric(AAPre2019),
        AAPost2019 = as.numeric(AAPost2019),
    ) %>%
    add_column(exp = 0) %>%
    add_column(expsq = 0) %>%
    add_column(prispline = 0) %>%
    add_column(secspline = 0) %>%
    add_column(terspline = 0) %>%
    add_column(GFC = 0)




yearmale_x <- CJ(Race = c("African/Black", "Coloured", "Indian/Asian", "White"), Year = seq(2002,2021)) %>%
    mutate(AAPre2008 = case_when(
        Year < 2003 ~ "0",
        Year == 2003 ~ "1",
        Year == 2004 ~ "2",
        Year == 2005 ~ "3",
        Year == 2006 ~ "4",
        Year == 2007 ~ "5",
        Year > 2007 ~ "5"
    )) %>%
    mutate(AAPre2014 = case_when(
        Year < 2008 ~ "0",
        Year == 2008 ~ "1",
        Year == 2009 ~ "2",
        Year == 2010 ~ "3",
        Year == 2011 ~ "4",
        Year == 2012 ~ "5",
        Year == 2013 ~ "6",
        Year > 2013 ~ "6"
    )) %>%
    mutate(AAPre2019 = case_when(
        Year < 2014 ~ "0",
        Year == 2014 ~ "1",
        Year == 2015 ~ "2",
        Year == 2016 ~ "3",
        Year == 2017 ~ "4",
        Year == 2018 ~ "5",
        Year > 2018 ~ "5"
    )) %>%
    mutate(AAPost2019 = case_when(
        Year < 2019 ~ "0",
        Year == 2019 ~ "1",
        Year == 2020 ~ "2",
        Year == 2021 ~ "3",
        Year > 2021 ~ "3"
    )) %>%
    mutate(
        AAPre2008 = as.numeric(AAPre2008),
        AAPre2014 = as.numeric(AAPre2014),
        AAPre2019 = as.numeric(AAPre2019),
        AAPost2019 = as.numeric(AAPost2019),
    ) %>%
    add_column(exp = 0) %>%
    add_column(expsq = 0) %>%
    add_column(prispline = 0) %>%
    add_column(secspline = 0) %>%
    add_column(terspline = 0) %>%
    add_column(GFC = 0)


yearmale_x$y <- predict.lm(object = male_rw_reg, newdata = yearmale_x)


yearmale_x %>%
    ggplot(aes(y = y, x= Year, group = Race, color = Race)) +
    geom_line()

library(tidyverse)
yearmale1 <- yearmale_x %>%
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

yearmale_noblack <- yearmale1[yearmale1$Race != "African",]


yearmale_noblack %>%
    ggplot(aes(y = wage_gap, x= Year, group = Race, color = Race)) +
    geom_line() +
    labs(x='Year', y='Wage Gap')



#> Output ====
library(dplyr)
by_postmales_real1 <- by_postmales_real %>%
    rename(
        "Experience" = "exp",
        "ExperienceSq" = "expsq",
        "Primary" = "prispline",
        "Secondary" = "secspline",
        "Tertiary" = "terspline",
        "Primary" = "prispline",
        "Year2003_2007" = "AAPre2008",
        "Year2008_2013" = "AAPre2014",
        "Year2014_2018" = "AAPre2019",
        "Year2019_2021"= "AAPost2019" ,
        "Birthyear1940_1949" = "by1940",
        "Birthyear1950_1959" = "by1950",
        "Birthyear1960_1969" = "by1960",
        "Birthyear1970_1979" = "by1970",
        "Birthyear1980_1989" = "by1980",
        "Birthyear1990_1999" = "by1990"
    )

by_postfem_real1 <- by_postfem_real %>%
    rename(
        "Experience" = "exp",
        "ExperienceSq" = "expsq",
        "Primary" = "prispline",
        "Secondary" = "secspline",
        "Tertiary" = "terspline",
        "Primary" = "prispline",
        "Year2003_2007" = "AAPre2008",
        "Year2008_2013" = "AAPre2014",
        "Year2014_2018" = "AAPre2019",
        "Year2019_2021"= "AAPost2019" ,
        "Birthyear1940_1949" = "by1940",
        "Birthyear1950_1959" = "by1950",
        "Birthyear1960_1969" = "by1960",
        "Birthyear1970_1979" = "by1970",
        "Birthyear1980_1989" = "by1980",
        "Birthyear1990_1999" = "by1990"
    )

postmales_real1 <- postmales_real %>%
    rename(
        "Experience" = "exp",
        "ExperienceSq" = "expsq",
        "Primary" = "prispline",
        "Secondary" = "secspline",
        "Tertiary" = "terspline",
        "Primary" = "prispline",
        "Year2003_2007" = "AAPre2008",
        "Year2008_2013" = "AAPre2014",
        "Year2014_2018" = "AAPre2019",
        "Year2019_2021"= "AAPost2019"
    )

postfemales_real1 <- postfemales_real %>%
    rename(
        "Experience" = "exp",
        "ExperienceSq" = "expsq",
        "Primary" = "prispline",
        "Secondary" = "secspline",
        "Tertiary" = "terspline",
        "Primary" = "prispline",
        "Year2003_2007" = "AAPre2008",
        "Year2008_2013" = "AAPre2014",
        "Year2014_2018" = "AAPre2019",
        "Year2019_2021"= "AAPost2019"
    )

fem_rw_reg1 <- lm(logsal ~ Race + Primary + Secondary + Tertiary + GFC + GFC*Primary + GFC*Secondary + Experience + ExperienceSq + Year2003_2007 + Year2008_2013 + Year2014_2018 + Year2019_2021 + Year2003_2007*Race + Year2008_2013*Race + Year2014_2018*Race + Year2019_2021*Race, data = postfemales_real1, weights = Weight)
fem_rw_reg1 %>%
    summary()

male_rw_reg1 <- lm(logsal ~ Race + Primary + Secondary + Tertiary + GFC + GFC*Primary + GFC*Secondary + Experience + ExperienceSq + Year2003_2007 + Year2008_2013 + Year2014_2018 + Year2019_2021 + Year2003_2007*Race + Year2008_2013*Race + Year2014_2018*Race + Year2019_2021*Race, data = postmales_real1, weights = Weight)
male_rw_reg1 %>%
    summary()

bymalereg1 <- lm(formula = logsal ~ Race + Primary + Secondary + Tertiary + GFC + GFC*Primary + GFC*Secondary + Experience + ExperienceSq + Year2003_2007 + Year2008_2013 + Year2014_2018 + Year2019_2021 + Year2003_2007*Race + Year2008_2013*Race + Year2014_2018*Race + Year2019_2021*Race + Birthyear1940_1949 + Birthyear1950_1959 + Birthyear1960_1969 + Birthyear1970_1979 +Birthyear1980_1989 + Birthyear1990_1999 + Birthyear1940_1949*Race + Birthyear1950_1959*Race + Birthyear1960_1969*Race + Birthyear1970_1979*Race + Birthyear1980_1989*Race + Birthyear1990_1999*Race, data = by_postmales_real1, weights = Weight)
bymalereg1 %>%
    summary()

byfemreg1 <- lm(formula = logsal ~ Race + Primary + Secondary + Tertiary + GFC + GFC*Primary + GFC*Secondary + Experience + ExperienceSq + Year2003_2007 + Year2008_2013 + Year2014_2018 + Year2019_2021 + Year2003_2007*Race + Year2008_2013*Race + Year2014_2018*Race + Year2019_2021*Race + Birthyear1940_1949 + Birthyear1950_1959 + Birthyear1960_1969 + Birthyear1970_1979 +Birthyear1980_1989 + Birthyear1990_1999 + Birthyear1940_1949*Race + Birthyear1950_1959*Race + Birthyear1960_1969*Race + Birthyear1970_1979*Race + Birthyear1980_1989*Race + Birthyear1990_1999*Race, data = by_postfem_real1, weights = Weight)
byfemreg1 %>%
    summary()

library(flextable)

rw_reg<- huxreg("(1) Male Log of Real Wages" = male_rw_reg1, "(2) Male Log of Real Wages" = bymalereg1, "(3) Female Log of Real Wages" = fem_rw_reg1, "(4) Female Log of Real Wages" = byfemreg1, omit_coefs = "(Birthyear1940_1949) & (Birthyear1950_1959) & (Birthyear1960_1969) & (Birthyear1970_1979) & (Birthyear1980_1989) & (Birthyear1990_1999)")

real_reg <- huxtable::as_flextable(rw_reg)
real_reg

real_reg %>%
    set_caption("Linear Regression of South African Wages")

install.packages("webshot")
library(webshot)

save_as_image(real_reg, path = "/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/RealWageRegsRenamed.png", zoom = 3, expand = 10, webshot = "webshot")


## Stargazer
library(stargazer)
stargazer(male_rw_reg1, bymalereg1, fem_rw_reg1, byfemreg1, type="html",
          dep.var.labels=c("Male Log of Real Wage", "Male Log of Real Wage", "Female Log of Real Wages", "Female Log of Real Wages"),
          out="models2.htm")




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


#> Descriptive Statistics ====

Appended_real <- appendedGHS %>%
    mutate(Year = as.numeric(paste(Year)))%>%
    left_join(CPI, by = "Year") %>%
    mutate(logsal = log(exp(logsal)*100/CPI))

# Average Wages Over Entire Period
sapply(Appended_real, mean, na.rm=TRUE)

## Average overall wage
7.617347e+00
### men
sapply(Appended_real[Appended_real$Gender == 0,], mean, na.rm=TRUE)
7.748943e+00
### women
7.458495e+00

## Average overall education
9.365177e+00
### men
9.182207e+00
### women
9.585108e+00

## Average overall age
3.824006e+01
### men
3.776124e+01
### women
3.881756e+01




## Average White Wage



sapply(Appended_real[Appended_real$Race == "White",], mean, na.rm=TRUE)
9.049471e+00
### men
sapply(appendedGHS[appendedGHS$Race == "White" & appendedGHS$Gender == 0,], mean, na.rm=TRUE)
9.186836e+00
### women
8.880682e+00

## Average white education
1.240233e+01
### men
1.226785e+01
### women
1.256623e+01

## Average white age
4.024313e+01
### men
4.034967e+01
### women
4.011250e+01



## Average Black Wage
sapply(appendedGHS[appendedGHS$Race == "African/Black",], mean, na.rm=TRUE)
7.490084e+00
### men
sapply(appendedGHS[appendedGHS$Race == "African/Black" & appendedGHS$Gender == 0,], mean, na.rm=TRUE)
7.625220e+00
### women
7.327719e+00

## Average black education
9.142690e+00
### men
8.954255e+00
### women
9.368198e+00

## Average black age
3.824314e+01
### men
3.759325e+01
### women
3.902343e+01


## Average Coloured Wage
sapply(appendedGHS[appendedGHS$Race == "Coloured",], mean, na.rm=TRUE)
7.547848e+00
### men
sapply(appendedGHS[appendedGHS$Race == "Coloured" & appendedGHS$Gender == 0,], mean, na.rm=TRUE)
7.641954e+00
### women
7.437849e+00

## Average coloured education
8.882290e+00
### men
8.621457e+00
### women
9.185969e+00

## Average coloured age
3.741697e+01
### men
3.748817e+01
### women
3.733379e+01



## Average Indian/Asian Wage
sapply(appendedGHS[appendedGHS$Race == "Indian/Asian",], mean, na.rm=TRUE)
8.520964e+00
### men
sapply(appendedGHS[appendedGHS$Race == "Indian/Asian" & appendedGHS$Gender == 0,], mean, na.rm=TRUE)
8.569845e+00
### women
8.437641e+00

## Average Indian/Asian education
1.158239e+01
### men
1.149011e+01
### women
1.173814e+01

## Average Indian/Asian age
3.743468e+01
### men
3.785246e+01
### women
3.672845e+01






