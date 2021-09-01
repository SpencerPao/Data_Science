rm(list=ls())
library(caret) # Machine Learning Library
library(xgboost) # XGBoost library

set.seed(1)
# Getting data from UCI.
col_names <- c("state",
               "county",
               "community",
               "communityname",
               "fold",
               "population",
               "householdsize",
               "racepctblack",
               "racePctWhite",
               "racePctAsian",
               "racePctHisp",
               "agePct12t21",
               "agePct12t29",
               "agePct16t24",
               "agePct65up",
               "numbUrban",
               "pctUrban",
               "medIncome",
               "pctWWage",
               "pctWFarmSelf",
               "pctWInvInc",
               "pctWSocSec",
               "pctWPubAsst",
               "pctWRetire",
               "medFamInc",
               "perCapInc",
               "whitePerCap",
               "blackPerCap",
               "indianPerCap",
               "AsianPerCap",
               "OtherPerCap",
               "HispPerCap",
               "NumUnderPov",
               "PctPopUnderPov",
               "PctLess9thGrade",
               "PctNotHSGrad",
               "PctBSorMore",
               "PctUnemployed",
               "PctEmploy",
               "PctEmplManu",
               "PctEmplProfServ",
               "PctOccupManu",
               "PctOccupMgmtProf",
               "MalePctDivorce",
               "MalePctNevMarr",
               "FemalePctDiv",
               "TotalPctDiv",
               "PersPerFam",
               "PctFam2Par",
               "PctKids2Par",
               "PctYoungKids2Par",
               "PctTeen2Par",
               "PctWorkMomYoungKids",
               "PctWorkMom",
               "NumIlleg",
               "PctIlleg",
               "NumImmig",
               "PctImmigRecent",
               "PctImmigRec5",
               "PctImmigRec8",
               "PctImmigRec10",
               "PctRecentImmig",
               "PctRecImmig5",
               "PctRecImmig8",
               "PctRecImmig10",
               "PctSpeakEnglOnly",
               "PctNotSpeakEnglWell",
               "PctLargHouseFam",
               "PctLargHouseOccup",
               "PersPerOccupHous",
               "PersPerOwnOccHous",
               "PersPerRentOccHous",
               "PctPersOwnOccup",
               "PctPersDenseHous",
               "PctHousLess3BR",
               "MedNumBR",
               "HousVacant",
               "PctHousOccup",
               "PctHousOwnOcc",
               "PctVacantBoarded",
               "PctVacMore6Mos",
               "MedYrHousBuilt",
               "PctHousNoPhone",
               "PctWOFullPlumb",
               "OwnOccLowQuart",
               "OwnOccMedVal",
               "OwnOccHiQuart",
               "RentLowQ",
               "RentMedian",
               "RentHighQ",
               "MedRent",
               "MedRentPctHousInc",
               "MedOwnCostPctInc",
               "MedOwnCostPctIncNoMtg",
               "NumInShelters",
               "NumStreet",
               "PctForeignBorn",
               "PctBornSameState",
               "PctSameHouse85",
               "PctSameCity85",
               "PctSameState85",
               "LemasSwornFT",
               "LemasSwFTPerPop",
               "LemasSwFTFieldOps",
               "LemasSwFTFieldPerPop",
               "LemasTotalReq",
               "LemasTotReqPerPop",
               "PolicReqPerOffic",
               "PolicPerPop",
               "RacialMatchCommPol",
               "PctPolicWhite",
               "PctPolicBlack",
               "PctPolicHisp",
               "PctPolicAsian",
               "PctPolicMinor",
               "OfficAssgnDrugUnits",
               "NumKindsDrugsSeiz",
               "PolicAveOTWorked",
               "LandArea",
               "PopDens",
               "PctUsePubTrans",
               "PolicCars",
               "PolicOperBudg",
               "LemasPctPolicOnPatr",
               "LemasGangUnitDeploy",
               "LemasPctOfficDrugUn",
               "PolicBudgPerPop",
               "ViolentCrimesPerPop")

# Not needing to locally store the dataset, let's scrape the data.
data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/communities/communities.data"))
colnames(data) <- col_names # dependent variable : Violent Crimes per Pop

# Summary of data (checking if any NA)
summary(data)
sapply(data, class) # checking out data types

# taking out community name.
data <- subset(data, select = -c(communityname))

# Converting characters to numerical data.
data[] <- sapply(data, as.numeric) # keeps in the format of dataframe.

indices <- createDataPartition(data$ViolentCrimesPerPop, p = 0.8, list=FALSE)
train_data <- data[indices,]
test_data <- data[-indices,]


# Doing XGBoost for Regression purposes.
grid_tune <- expand.grid(
  nrounds = c(500,1000,1500), #number of trees
  max_depth = c(2,4,6),
  eta = 0.3, #c(0.025,0.05,0.1,0.3), #Learning rate
  gamma = 0, # pruning --> Should be tuned. i.e c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0)
  colsample_bytree = 1, # c(0.4, 0.6, 0.8, 1.0) subsample ratio of columns for tree
  min_child_weight = 1, # c(1,2,3) # the larger, the more conservative the model
  #is; can be used as a stop
  subsample = 1 # c(0.5, 0.75, 1.0) # used to prevent overfitting by sampling X% training
)

train_control <- trainControl(method = "cv",
                              number=3,
                              verboseIter = TRUE,
                              allowParallel = TRUE)
xgb_tune <- train(x = train_data[,-127],
                  y = train_data[,127],
                  trControl = train_control,
                  tuneGrid = grid_tune,
                  method= "xgbTree",
                  verbose = TRUE)
xgb_tune

predict(xgb_tune, test_data)

# Prediction:
xgb.pred <- predict(xgb_tune, test_data)

mse = mean((test_data$ViolentCrimesPerPop - xgb.pred)^2)
mae = caret::MAE(test_data$ViolentCrimesPerPop, xgb.pred)
rmse = caret::RMSE(test_data$ViolentCrimesPerPop, xgb.pred)
