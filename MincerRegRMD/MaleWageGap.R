# Male Wage Gap Graph

male_GHS <- appendedGHS[appendedGHS$Gender == 0,]

malemincer <- lm(logsal ~ Race + prispline + secspline + terspline + exp + expsq + Gender + Year*Race, data = male_GHS, weights = Weight)
malemincer %>%
    summary()

male_x <- male_GHS %>%
    select(Race, Year) %>%
    unique() %>%
    add_column(exp = 0) %>%
    add_column(expsq = 0) %>%
    add_column(Gender = 0) %>%
    add_column(prispline = 0) %>%
    add_column(secspline = 0) %>%
    add_column(terspline = 0)


male_x$y <- predict.lm(object = malemincer, newdata = male_x)


male_blacksal <- male_x[ which(male_x$Race=='African/Black'), ]

male_x <- male_x %>%
    mutate(wagegap = case_when(Year == "2002" ~ y - male_blacksal$y[1],
                               Year == "2003" ~ y - male_blacksal$y[2],
                               Year == "2004" ~ y - male_blacksal$y[3],
                               Year == "2005" ~ y - male_blacksal$y[4],
                               Year == "2006" ~ y - male_blacksal$y[5],
                               Year == "2007" ~ y - male_blacksal$y[6],
                               Year == "2008" ~ y - male_blacksal$y[7],
                               Year == "2009" ~ y - male_blacksal$y[8],
                               Year == "2010" ~ y - male_blacksal$y[9],
                               Year == "2011" ~ y - male_blacksal$y[10],
                               Year == "2012" ~ y - male_blacksal$y[11],
                               Year == "2013" ~ y - male_blacksal$y[12],
                               Year == "2014" ~ y - male_blacksal$y[13],
                               Year == "2015" ~ y - male_blacksal$y[14],
                               Year == "2016" ~ y - male_blacksal$y[15],
                               Year == "2017" ~ y - male_blacksal$y[16],
                               Year == "2018" ~ y - male_blacksal$y[17],
                               Year == "2019" ~ y - male_blacksal$y[18],
                               Year == "2020" ~ y - male_blacksal$y[19],
                               Year == "2021" ~ y - male_blacksal$y[20]))

ggplot(data=male_x, aes(x = Year, y = wagegap, group = Race)) +
    geom_line(aes(color = Race)) +
    geom_point(aes(color = Race))

male_gapgraphdf <- male_x[male_x$Race != "African/Black",]

male_gapgraph <- ggplot(data=male_gapgraphdf, aes(x = Year, y = wagegap, group = Race)) +
    geom_line(aes(color = Race)) +
    geom_point(aes(color = Race))+
    labs(x='Year', y='Wage Gap') +
    theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
male_gapgraph
