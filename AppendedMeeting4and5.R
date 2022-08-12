# Appending and Regressing 

# Append ----
append <- bind_rows(GHS2002, GHS2009)

appendedGHS <- bind_rows(append, GHS2016)

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
mincer2 <- lm(logsal ~ Race + prispline + secspline + terspline + AgeSq + Age + Gender + lastyear + Race*Y2009 + Race*Y2016, data = appendedGHS)
mincer2 %>% 
  summary()

# Experience Variable 
appendedGHS$exp <- appendedGHS$Age - appendedGHS$educnum - 6

# Experience Squared 
appendedGHS$expsq <- appendedGHS$exp^2

# Export data 
write.csv(appendedGHS, "/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/data/appendedGHS.csv")

