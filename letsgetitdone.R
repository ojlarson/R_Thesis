library(readxl)
library(GGally)
library(tidyverse)
library(multcomp)
library(emmeans)

read_stats <- function(number) {
  read_excel("TomStats.xlsx", sheet = paste0("Sheet", number), 
             skip = 1)
}
raw_conc <- read_stats(1)
us_ds <- read_stats(2)
emc <- read_stats(3) %>%
  rename_with(~ gsub(" .*", "", .x)) %>%
  mutate(Site=as.factor(Site))
us_ds_emc <- read_stats(4)

library(tidyverse)

# Pivot your data to wide format
emc_wide <- emc %>%
  pivot_wider(names_from = Site, values_from = c(TN, TP, Nitrate, SRP, TSS, DOC, pH, Conductivity))

# View the reshaped data
head(emc_wide)
# Perform MANOVA

# View the results
summary(fit, test = "Wilks")


emc_tn_long <- emc[, c(1,2,3)]
emc_tn_wide <- emc_tn_long %>%
  pivot_wider(names_from = 'Site', values_from = 'TN')
ggpairs(emc_tn_wide_clean[, 2:4])

emc_tn_wide_clean <- na.omit(emc_tn_wide)
emc_tn_long_clean <- emc_tn_wide_clean %>%
  pivot_longer(!DateCollected, names_to = "Site", values_to = "TN") %>%
  mutate(Site = as.factor(Site))

library(car)

# Assuming emc_tn_long is your data frame
fit <- aov(TN ~ Site, data = emc_tn_long_clean)
summary(fit)

library(car)

# Assuming emc_tn_wide is your data frame
fit <- manova(cbind(TN, TP, Nitrate, SRP, TSS, DOC, pH, Conductivity) ~ Site, data = emc)
summary(Anova(fit, type="II"))


# 2. ANOVA

anova_model <- aov(TN ~ Site, data = emc_tn_long_clean)
summary(anova_model)
tukey <- glht(anova_model, linfct = mcp(Site = "Tukey"))
ci_output <- confint(tukey, level = 0.95)
ci_output
posthoc <- TukeyHSD(fit)
print(posthoc)


# Pairwise comparisons with Bonferroni correction using emmeans
pairwise <- emmeans(anova_model, pairwise ~ Site, adjust = "bonferroni")
summary(pairwise)

# 3. Test the difference of differences (H0: μP−μL=μL−μE)
# car package does not have a direct equivalent of glht, 
# we will use linearHypothesis from car package instead
hyp <- linearHypothesis(anova_model, "Boundary - Confluence = Confluence - Outflow")
print(hyp)

# 4. Calculate R-squared values
model1 <- lm((Boundary - Confluence) ~ (Boundary - Outflow), data = emc_tn_wide, na.action = na.exclude)
model2 <- lm((Confluence - Outflow) ~ (Boundary - Outflow), data = emc_tn_wide, na.action = na.exclude)
print(c(summary(model1)$r.squared, summary(model2)$r.squared))
