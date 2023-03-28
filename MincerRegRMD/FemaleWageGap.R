# Female Wage Gap graph

female_GHS <- appendedGHS[appendedGHS$Gender == 1,]

femalemincer <- lm(logsal ~ Race + prispline + secspline + terspline + exp + expsq + Gender + Year*Race, data = female_GHS, weights = Weight)
femalemincer %>%
    summary()

female_x <- female_GHS %>%
    select(Race, Year) %>%
    unique() %>%
    add_column(exp = 0) %>%
    add_column(expsq = 0) %>%
    add_column(Gender = 0) %>%
    add_column(prispline = 0) %>%
    add_column(secspline = 0) %>%
    add_column(terspline = 0)


female_x$y <- predict.lm(object = femalemincer, newdata = female_x)


fem_blacksal <- female_x[ which(female_x$Race=='African/Black'), ]

female_x <- female_x %>%
    mutate(wagegap = case_when(Year == "2002" ~ y - fem_blacksal$y[1],
                               Year == "2003" ~ y - fem_blacksal$y[2],
                               Year == "2004" ~ y - fem_blacksal$y[3],
                               Year == "2005" ~ y - fem_blacksal$y[4],
                               Year == "2006" ~ y - fem_blacksal$y[5],
                               Year == "2007" ~ y - fem_blacksal$y[6],
                               Year == "2008" ~ y - fem_blacksal$y[7],
                               Year == "2009" ~ y - fem_blacksal$y[8],
                               Year == "2010" ~ y - fem_blacksal$y[9],
                               Year == "2011" ~ y - fem_blacksal$y[10],
                               Year == "2012" ~ y - fem_blacksal$y[11],
                               Year == "2013" ~ y - fem_blacksal$y[12],
                               Year == "2014" ~ y - fem_blacksal$y[13],
                               Year == "2015" ~ y - fem_blacksal$y[14],
                               Year == "2016" ~ y - fem_blacksal$y[15],
                               Year == "2017" ~ y - fem_blacksal$y[16],
                               Year == "2018" ~ y - fem_blacksal$y[17],
                               Year == "2019" ~ y - fem_blacksal$y[18],
                               Year == "2020" ~ y - fem_blacksal$y[19],
                               Year == "2021" ~ y - fem_blacksal$y[20]))

ggplot(data=female_x, aes(x = Year, y = wagegap, group = Race)) +
    geom_line(aes(color = Race)) +
    geom_point(aes(color = Race))

fem_gapgraphdf <- female_x[female_x$Race != "African/Black",]

fem_gapgraph <- ggplot(data=fem_gapgraphdf, aes(x = Year, y = wagegap, group = Race)) +
    geom_line(aes(color = Race)) +
    geom_point(aes(color = Race)) +
    labs(x='Year', y='Wage Gap') +
    theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
fem_gapgraph
