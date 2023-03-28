# Checking weights

weightsmincer2002 <- lm(logsal ~ Race +  Age + AgeSq + Gender, data = GHS2002, weights = Weight)
weightsmincer2002 %>%
    summary()

noweightsmincer2002 <- lm(logsal ~ Race +  Age + AgeSq + Gender, data = GHS2002)
noweightsmincer2002 %>%
    summary()


weightsmincer2019 <- lm(logsal ~ Race +  Age + AgeSq + Gender, data = GHS2019, weights = Weight)
weightsmincer2019 %>%
    summary()

noweightsmincer2019 <- lm(logsal ~ Race +  Age + AgeSq + Gender, data = GHS2019)
noweightsmincer2019 %>%
    summary()

library(huxtable)
weightingregs <- huxreg("2002 Log Wages (Weighted)" = weightsmincer2002, "2002 Log Wages (Unweighted)" = noweightsmincer2002, "2019 Log Wages (Weighted)" = weightsmincer2019, "2019 Log Wages (Unweighted)" = noweightsmincer2019)


install.packages("webshot")
library(webshot)
weightreg <- huxtable::as_flextable(weightingregs)

weightreg %>%
    set_caption("Comparing the Use of Sample Weights via Regressions")

save_as_image(weightreg, path = "/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/WeightingRegressions.png", zoom = 3, expand = 10, webshot = "webshot")

