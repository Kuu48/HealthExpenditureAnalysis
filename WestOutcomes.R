library(readxl)
West_Africa <- read_excel("C:/Users/hp/Desktop/ARTICLES/paper 7,8,9,10,11, and 12/West Africa.xlsx")
West_Africa
nrow(West_Africa)
ncol(West_Africa)
names(West_Africa)
summary(West_Africa)


library(lavaan)

WestD<-na.omit(West_Africa)



# Check rows after omitting missing data
nrow(WestD)
summary(WestD)

# Create a new dataframe with the relevant variables
columnss <- c("BR", "LE", "DR", "FR", "LR", "NM", "SM", "PG", 
             "DGGHE", "CHE", "DPHE", "OPE", "EHE")
WestD_subset <- WestD[, columnss]

# Select only numeric columns
numeric_cols <- sapply(WestD_subset, is.numeric)

# Standardize the numeric columns (mean = 0, variance = 1)
WestD_subset[numeric_cols] <- lapply(WestD_subset[numeric_cols], scale)

# Check the standardized data (means should be 0 and standard deviations should be 1)
check_standardization1 <- sapply(WestD_subset[numeric_cols], function(x) c(mean = mean(x), sd = sd(x)))
print(check_standardization1)

# Define the SEM model
modelw <- '
  # Measurement model for dependent variables
  BR ~ DGGHE + CHE + DPHE + OPE + EHE
  LE ~ DGGHE + CHE + DPHE + OPE + EHE
  DR ~ DGGHE + CHE + DPHE + OPE + EHE
  FR ~ DGGHE + CHE + DPHE + OPE + EHE
  LR ~ DGGHE + CHE + DPHE + OPE + EHE
  NM ~ DGGHE + CHE + DPHE + OPE + EHE
  SM ~ DGGHE + CHE + DPHE + OPE + EHE
  PG ~ DGGHE + CHE + DPHE + OPE + EHE
'

# Fit the SEM model using the standardized data
fit_West <- sem(modelw, data = WestD_subset)

# Check the fit of the model
summary(fit_West, fit.measures = TRUE, standardized = TRUE)
varTable(fit_West)

# Visualize the SEM path diagram
library(semPlot)  # For semPaths



# Visualize the SEM path diagram with your specified preferences
semPaths(fit_West, 
         what = "std",            # Standardized estimates
         layout = "tree2",        # Tree layout with better spacing
         edge.label.cex = 0.8,    # Adjusts the size of the labels on the paths
         sizeMan = 10,            # Adjusts the size of the nodes
         curvePivot = TRUE,       # Curved arrows for clearer paths
         edge.color ="purple",     # Set edge color to blue
         residuals = FALSE,       # Hide residual arrows for clarity
         nCharNodes = 0)        





