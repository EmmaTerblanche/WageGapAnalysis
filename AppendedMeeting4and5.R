# Appending and Regressing

# Append ----
library(dplyr)
appendedGHS <- bind_rows(GHS2002, GHS2003, GHS2004, GHS2005, GHS2006, GHS2007, GHS2008, GHS2009, GHS2010, GHS2011, GHS2012, GHS2013, GHS2014, GHS2015, GHS2016, GHS2017, GHS2018, GHS2019, GHS2020, GHS2021)

# Regressions ----
mincer1 <- lm(logsal ~ Race + educnum + AgeSq + Age + Gender + Race*Y2009 + Race*Y2016, data = appendedGHS)
mincer1 %>%
  summary()


appendedGHS %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()

# Splines ----
appendedGHS <- appendedGHS %>%
  mutate(prispline = ifelse(
    educnum < 8, educnum, 7))

appendedGHS <- appendedGHS %>%
  mutate(secspline = ifelse(
    educnum < 8, 0, ifelse(educnum == 8, 1, ifelse(educnum == 9, 2, ifelse(educnum == 10, 3, ifelse(educnum == 11, 4, ifelse(educnum == 12, 5, 5)))))))

appendedGHS <- appendedGHS %>%
  mutate(terspline = ifelse(
    educnum < 13, 0, ifelse(educnum == 13, 1, ifelse(educnum == 14, 2, ifelse(educnum == 15, 3, ifelse(educnum == 16, 4, ifelse(educnum == 17, 5, 5)))))))

# Regressions with splines ----
mincer2 <- lm(logsal ~ Race + prispline + secspline + terspline + AgeSq + Age + Gender + Race*Y2005 + Race*Y2009 + Race*Y2013 + Race*Y2016 + Race*Y2020, data = appendedGHS)
mincer2 %>%
  summary()

# Regressions with Experience
mincer3 <- lm(logsal ~ Race + exp + expsq + Gender + Race*Y2005 + Race*Y2009 + Race*Y2013 + Race*Y2016 + Race*Y2020, data = appendedGHS)
mincer3 %>%
    summary()

# Experience Variable
appendedGHS$exp <- appendedGHS$Age - appendedGHS$educnum - 6

# Experience Squared
appendedGHS$expsq <- appendedGHS$exp^2

# Export data
write.csv(appendedGHS, "/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/data/appendedGHS.csv")

# Coefficient Graph
install.packages("coefplot")
library(coefplot)

coefplot(mincer3)

# Adding coeff's to new dataframe
install.packages("tidyr")
install.packages("sjPlot")
library(tidyr)
library(sjPlot)

coeffs <- summary(mincer3)$coefficients

plot(coeffs)

plot_model(mincer3, type = "int")
#Race x axis, grouped by Y2020

plot_model(mincer3, type = "pred", terms = c("Y2020", "Race"))
##Y2020 x axis, grouped by race

# Regression with Factor Year Variable
library(dplyr)

appendedGHS <- appendedGHS %>%
    mutate(
        Year = case_when(
            Y2002 == 1 ~ "2002",
            Y2003 == 1 ~ "2003",
            Y2004 == 1 ~ "2004",
            Y2005 == 1 ~ "2005",
            Y2006 == 1 ~ "2006",
            Y2007 == 1 ~ "2007",
            Y2008 == 1 ~ "2008",
            Y2009 == 1 ~ "2009",
            Y2010 == 1 ~ "2010",
            Y2011 == 1 ~ "2011",
            Y2012 == 1 ~ "2012",
            Y2013 == 1 ~ "2013",
            Y2014 == 1 ~ "2014",
            Y2015 == 1 ~ "2015",
            Y2016 == 1 ~ "2016",
            Y2017 == 1 ~ "2017",
            Y2018 == 1 ~ "2018",
            Y2019 == 1 ~ "2019",
            Y2020 == 1 ~ "2020",
            Y2021 == 1 ~ "2021",
            TRUE ~ "other"
        )
    )


appendedGHS$Year <- factor(appendedGHS$Year)

mincer4 <- lm(logsal ~ Race + prispline + secspline + terspline + exp + expsq + Gender + Year*Race, data = appendedGHS, weights = Weight)
mincer4 %>%
    summary()

# OUTREG HERE 

library(sjPlot)
plot_model(mincer4, type = "pred", terms = c("Year", "Race"))
# Miskien nuttig vir gesprek oor almal wat op gegaan het en oor standard errors? 

# Regression with Factor Year Variable and Splines
library(splines)
mincer5 <- lm(logsal ~ Race + exp + expsq + Gender + bs(Year,knots = c(2005,2009,2013,2016))*Race, data = appendedGHS)
mincer5 %>%
    summary()
##Iets is verkeerd.

plot_model(mincer5, type = "pred", terms = c("Year", "Race"))

install.packages("data.table")
library(data.table)
library(tibble)

new_x <- appendedGHS %>%
    select(Race, Year) %>%
    unique() %>%
    add_column(exp = 0) %>%
    add_column(expsq = 0) %>%
    add_column(Gender = 0) %>%
    add_column(prispline = 0) %>%
    add_column(secspline = 0) %>%
    add_column(terspline = 0)

new_x$y <- predict.lm(object = mincer4, newdata = new_x)

blacksal <- new_x[ which(new_x$Race=='African/Black'), ]


new_x <- new_x %>%
    mutate(wagegap = case_when(Year == "2002" ~ y - blacksal$y[1],
                               Year == "2003" ~ y - blacksal$y[2],
                               Year == "2004" ~ y - blacksal$y[3],
                                Year == "2005" ~ y - blacksal$y[4],
                               Year == "2006" ~ y - blacksal$y[5],
                               Year == "2007" ~ y - blacksal$y[6],
                               Year == "2008" ~ y - blacksal$y[7],
                                Year == "2009" ~ y - blacksal$y[8],
                               Year == "2010" ~ y - blacksal$y[9],
                               Year == "2011" ~ y - blacksal$y[10],
                               Year == "2012" ~ y - blacksal$y[11],
                                Year == "2013" ~ y - blacksal$y[12],
                               Year == "2014" ~ y - blacksal$y[13],
                               Year == "2015" ~ y - blacksal$y[14],
                                Year == "2016" ~ y - blacksal$y[15],
                               Year == "2017" ~ y - blacksal$y[16],
                               Year == "2018" ~ y - blacksal$y[17],
                               Year == "2019" ~ y - blacksal$y[18],
                                Year == "2020" ~ y - blacksal$y[19],
                               Year == "2021" ~ y - blacksal$y[20]))


# Sort by race then maybe for loop that uses every 4th observation.
# OR: new datasets with only black in one and the rest of the races in other.
# Then use leftjoin. You will have one black obs for 2002 for every three (white, col, indian) of the same year in the other => minus.

ggplot(data=new_x, aes(x = Year, y = wagegap, group = Race)) +
    geom_line(aes(color = Race)) +
    geom_point(aes(color = Race))

gapgraphdf <- new_x[new_x$Race != "African/Black",]

gapgraph <- ggplot(data=gapgraphdf, aes(x = Year, y = wagegap, group = Race)) +
    geom_line(aes(color = Race)) +
    geom_point(aes(color = Race))+
  labs(x='Year', y='Wage Gap') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
gapgraph


# Something weird happened in 2020 w Indian salaries => check with just comparing averages in years
# Look at sample methods in 2020


# Notas
##predict command
##Categorical year variable with splines - knots for everything but first and last year.
##Then run new regression, interacting with race. Then plot that.

# 2002-2008 Spline 
appendedGHS$Year <- as.numeric(paste(appendedGHS$Year))

appendedGHS <- appendedGHS %>% 
  mutate(Spline20022008 = case_when(
    Year < 2002 ~ "0",
    Year == 2002 ~ "1",
    Year == 2003 ~ "2",
    Year == 2004 ~ "3",
    Year == 2005 ~ "4",
    Year == 2006 ~ "5",
    Year == 2007 ~ "6",
    Year == 2008 ~ "7",
    Year > 2008 ~ "7"
  ))

appendedGHS$Spline20022008 <- as.numeric(appendedGHS$Spline20022008)

# New Gap Graph with 2002-2008 Spline 
appendedGHS$Year <- factor(appendedGHS$Year)

mincer20022008 <- lm(logsal ~ Race + prispline + secspline + terspline + exp + expsq + Gender + Year*Race + Spline20022008, data = appendedGHS)
mincer20022008 %>%
  summary()
# NA's? 

spline20022008_x <- appendedGHS %>%
  select(Race, Year) %>%
  unique() %>%
  add_column(exp = 0) %>%
  add_column(expsq = 0) %>%
  add_column(Gender = 0) %>% 
  add_column(prispline = 0) %>% 
  add_column(secspline = 0) %>% 
  add_column(terspline = 0) %>% 
  add_column(Spline20022008 = 0)


spline20022008_x$y <- predict.lm(object = mincer20022008, newdata = spline20022008_x)

spline20022008_blacksal <- spline20022008_x[ which(spline20022008_x$Race=='African/Black'), ]

spline20022008_x <- spline20022008_x %>%
  mutate(wagegap = case_when(Year == "2002" ~ y - spline20022008_blacksal$y[1],
                             Year == "2003" ~ y - spline20022008_blacksal$y[2],
                             Year == "2004" ~ y - spline20022008_blacksal$y[3],
                             Year == "2005" ~ y - spline20022008_blacksal$y[4],
                             Year == "2006" ~ y - spline20022008_blacksal$y[5],
                             Year == "2007" ~ y - spline20022008_blacksal$y[6],
                             Year == "2008" ~ y - spline20022008_blacksal$y[7],
                             Year == "2009" ~ y - spline20022008_blacksal$y[8],
                             Year == "2010" ~ y - spline20022008_blacksal$y[9],
                             Year == "2011" ~ y - spline20022008_blacksal$y[10],
                             Year == "2012" ~ y - spline20022008_blacksal$y[11],
                             Year == "2013" ~ y - spline20022008_blacksal$y[12],
                             Year == "2014" ~ y - spline20022008_blacksal$y[13],
                             Year == "2015" ~ y - spline20022008_blacksal$y[14],
                             Year == "2016" ~ y - spline20022008_blacksal$y[15],
                             Year == "2017" ~ y - spline20022008_blacksal$y[16],
                             Year == "2018" ~ y - spline20022008_blacksal$y[17],
                             Year == "2019" ~ y - spline20022008_blacksal$y[18],
                             Year == "2020" ~ y - spline20022008_blacksal$y[19],
                             Year == "2021" ~ y - spline20022008_blacksal$y[20]))

ggplot(data=spline20022008_x, aes(x = Year, y = wagegap, group = Race)) +
  geom_line(aes(color = Race)) +
  geom_point(aes(color = Race))

spline_gapgraphdf <- spline20022008_x[spline20022008_x$Race != "African/Black",]

spline_gapgraph <- ggplot(data=spline_gapgraphdf, aes(x = Year, y = wagegap, group = Race)) +
  geom_line(aes(color = Race)) +
  geom_point(aes(color = Race))
spline_gapgraph



#> Descriptive Statistics ====

# Average Wages Over Entire Period 
sapply(appendedGHS, mean, na.rm=TRUE)

## Average overall wage 
7.617347e+00 
### men 
sapply(appendedGHS[appendedGHS$Gender == 0,], mean, na.rm=TRUE)
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
sapply(appendedGHS[appendedGHS$Race == "White",], mean, na.rm=TRUE)
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





