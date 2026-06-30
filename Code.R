
##=============================================================================
##
##  The tyranny of the p: when significance misleads 
##
##  Code for OLS regression, residual checks, and plots.
##  Written in "Mariposa Orchid" Release (f0b76cc0, 2025-05-04) for Windows
##
##=============================================================================

pkgs <- c(
  "readxl", "ggplot2", "lmtest", "patchwork", "sandwich", "moments"
  )
for (p in pkgs) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
    }
  }

df1 <- read_excel("Nessie.xlsx")
model_1 = lm(df1$Y~df1$X)
summary(model_1)
## Beta = .3582,  p < 2.2e-16***
ggplot(df1, aes(x = df1$X, y = df1$Y)) + 
  geom_point(size = 0.1) +
  xlab("") +
  ylab("") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

## Assumptions
plot(model_1, which = 2)  ## QQ-plot
coeftest(model_1, vcov = vcovHC(model_1, type = "HC1")) ## Robust check
## Check residuals
res <- residuals(model_1)
skewness(res) ## measure of asymmetry
kurtosis(res) ## measure of tail-heaviness
## Residuals are approximately normal

##=============================================================================
##
## End
##
##=============================================================================