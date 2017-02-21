# These functions are made by Johanna Piipponen
#### 

# Data handling and splitting  ------------------------------------------------


FetchData <- function(station, utc, lead.time, season = 0, response = 1) {
  # Loads the data from the csv files.
  # 
  # Args:
  #   station: (5, 640, 2861)
  #   utc: (1-2)
  #   lead.time: (1-65)
  #   season: the whole year (0) or season (winter-autumn 1-4)
  #   response: T2 (1)
  # 
  # Returns:
  #   data: a data frame in which the first column is the response
  #         variable and the rest are independent variables
  #   timevector: a POSIXct vector with UTC-based datetimes of the observations
  
  
  # creating the file path which relies on the constant variables in
  # importantvariables.rData
  filepath <- paste("data/station_", STATIONIDS[station],
                    "_", ANALYSISTIME[utc],
                    "_", FORECASTS[lead.time],
                    "_season", season,
                    "_", RESPONSES[response],
                    "_level", RESPONSELEVELS[response],
                    ".csv", sep="")
  
  # loads data from the file and removes the first useless row
  alldata <- read.csv(file = filepath)
  alldata <- alldata[,-1]
  
  # extracts the first column and defines it as a datetime vector
  timevector <- as.POSIXct(alldata[,1], tz = "UTC")
  # the first column is then erased
  data <- alldata[,-1]
  
  # renames the first column
  # just putting the response variable in line with the naming convention
  colnames(data)[1] <- "HAVAINTO"
  
  # gathers the returned list
  result <- list("data" = data, "timevector" = timevector)
  
  return(result)
}



CleanData <- function(data.object, what.data = "all") {
  # Cleans a data object from bad observations and variables.
  #
  # Args:
  #   data.object: a data object from FetchData function
  #   what.data: all/oldandnew/new
  #
  # Returns:
  #   A cleaned data object of the same form that the input argument.
  
  # separate the data object into a data frame and a vector
  data <- data.object$data
  timevector <- data.object$timevector
  no.of.points <- length(timevector)
  
  # first we will delete faulty variables which are NA most of the time
  
  # calculating the number of NAs is variables (not calculating the response
  # variable because we can not remove it)
  no.of.na <- apply(X = data[,-1], MARGIN = 2, FUN = function(x) sum(is.na(x)))
  
  # "normal" amount of NAs is one fourth of all points
  na.tolerance <- no.of.points/4
  
  # searching the variables which fulfill the conditions (number of NAs is lower
  # than the tolerance of NAs)
  nice.variables <- which(no.of.na < na.tolerance)
  
  # a small tweak because the repsonse variable CAN NOT be removed
  nice.variables <- c(1, nice.variables+1)
  
  # finally removing the variables which did not fulfill the earlier condition
  data <- data[,nice.variables]
  
  # secondly, we will delete all incomplete observations
  
  # searching the complete cases and keeping only them
  complete_obs <- complete.cases(data)
  data <- data[complete_obs,]
  timevector <- timevector[complete_obs]
  
  # third, we will remove snow variables because they are not important
  important.vars <- !(colnames(data) %in% c("SD", "RSN"))
  data <- data[,important.vars]
  
  # removing faulty U_100M and V_100M observations
  # both variables can vary between -40 and 40
  good.u <- (data$U_100M > -40) & (data$U_100M < 40)
  good.v <- (data$V_100M > -40) & (data$V_100M < 40)
  good.indices <- good.u & good.v
  data <- data[good.indices,]
  timevector <- timevector[good.indices]
  
  # thirdly, we will remove constant variables
  data <- RemoveConstantVariables(data)
  
  # NOTE: other things to consider are
  #   * checking if standard deviation is "too low"
  #   * removing SD and RSN
  #   * removing highly correlated variables
  
  if (what.data != "all") {
    if (what.data == "oldandnew") {
      limit.for.truncated.data <- as.POSIXct("2015-11-06 23:59:00", tz = "UTC")
    } else if (what.data == "new") {
      limit.for.truncated.data <- as.POSIXct("2016-03-20 23:59:00", tz = "UTC")
    }
    index <- which.max(timevector >= limit.for.truncated.data)
    last.index <- length(timevector)
    timevector <- timevector[index:last.index]
    data <- data[index:last.index,]
  } else {
    # carry on as usual
  }
  
  # returning the data object in the same form but somewhat smaller size
  results <- list("data" = data, "timevector" = timevector)
  
  return(results)
}


RemoveConstantVariables <- function(data) {
  # Removes variables that are constant everywhere.
  #
  # Args:
  #   data: a data matrix
  #
  # Returns:
  #   A data frame with the constant variables removed.
  
  constants <- apply(X = data, MARGIN = 2, FUN = IsVariableConstant)
  data <- data[,!constants]
  
  return(data)
}


IsVariableConstant <- function(x) {
  # Tests whether a vector is constant.
  #
  # Args:
  #   x: a vector
  #
  # Returns:
  #   TRUE if a vector is constant, FALSE otherwise.
  
  # tests if the difference between the smallest and the largest value is
  # smaller than the precision
  result <- abs(max(x) - min(x)) < .Machine$double.eps^0.5
  return(result)
}


LoadCleanedData <- function(..., what.data = "all") {
  # A wrapper function for FetchData and CleanData: performs both.
  #
  # Args:
  #   The same as FetchData has because everything is passed to it.
  #
  # Returns:
  #   A fetched and cleaned data object.
  
  data.object <- FetchData(...)
  cleaned.data.object <- CleanData(data.object = data.object,
                                   what.data = what.data)
  
  return(cleaned.data.object)
}


SplitData <- function(timevector) {
  # Splits data into year-long groups. THIS HAS BEEN UPGRADED TO FUNCTION
  # `SplitDataEvenly`!!!
  #
  # Args:
  #   timevector: a vector of POSIXct elements
  # 
  # Returns:
  #   Indices that define the first element in a new group. The last index is
  #   non-existent which is to be noted when using indices in a loop.
  
  # the data starts at 2011-12-01 which leads to choosing these cut-off dates
  constants <- as.POSIXct(c("2011-11-30 23:59:00",
                            "2012-11-30 23:59:00",
                            "2013-11-30 23:59:00",
                            "2014-11-30 23:59:00",
                            "2015-11-30 23:59:00"), tz = "UTC")
  
  # find out the indices of datetimes that happen RIGHT after the above times
  indices <- sapply(X = constants, FUN = function(x) which.max(timevector >= x))
  
  # we will erase the duplicated values
  # the duplicated values might happen if there isn't ANY data on some interval
  # (I'm looking at you, Cairo Airport)
  indices <- indices[!duplicated(indices)]
  
  # forming the indices
  indices <- c(indices, length(timevector)+1)
  
  return(indices)
}


SplitDataEvenly <- function(timevector, folds = 4) {
  # Splits data evenly into `folds` groups, default is four groups.
  #
  # Args:
  #   timevector: a vector
  #   folds: the number of groups
  #
  # Returns:
  #   Indices that define the first element in a new group. The last index is
  #   non-existent which is to be noted when using indices in a loop.
  
  no.of.points <- length(timevector)
  min.points <- no.of.points %/% folds
  add.points <- no.of.points %% folds
  point.vector <- rep(min.points, times = folds)
  if (add.points > 0) {
    point.vector[1:add.points] <- point.vector[1:add.points] + 1
  }
  indices <- rep(NA, times = (folds+1))
  indices[1] <- 1
  for (ii in 2:(folds+1)) {
    indices[ii] <- indices[ii-1] + point.vector[ii-1]
  }
  return(indices)
}


IndexVectorToFilter <- function(indices) {
  # Created a vector based on indices that can be used as a filter in plotting.
  # 
  # Args:
  #   indices: an indice vector from functions SplitData or SplitDataEvenly
  # 
  # Returns:
  #   A vector of the same length as the last element in indices minus one. For
  #   example, if the input vector is c(1,5,8,12), the output is 
  #   c(1,1,1,1,2,2,2,3,3,3,3).
  
  no.of.groups <- (length(indices) - 1)
  sizes.of.groups <- diff(indices)
  filter <- rep(1:no.of.groups, times = sizes.of.groups)
  return(filter)
}


FetchAndCombineSeasons <- function(...) {
  # Reads and combines all four seasons of the same data set.
  #
  # Args:
  #   ...: passed on to FetchData but does not need to specify `season`
  #
  # Return:
  #   Returns a data object with timevector and data.
  
  winter <- FetchData(season = 1, ...)
  spring <- FetchData(season = 2, ...)
  summer <- FetchData(season = 3, ...)
  autumn <- FetchData(season = 4, ...)
  
  time.winter <- winter$timevector
  time.spring <- spring$timevector
  time.summer <- summer$timevector
  time.autumn <- autumn$timevector
  
  data.winter <- winter$data
  data.spring <- spring$data
  data.summer <- summer$data
  data.autumn <- autumn$data
  
  all.times <- .POSIXct(c(time.winter, time.spring, time.summer, time.autumn),
                        tz = "UTC")
  all.datas <- rbind(data.winter, data.spring, data.summer, data.autumn)
  
  duplicated.indices <- duplicated(all.times)
  
  times.trunc <- all.times[!duplicated.indices]
  datas.trunc <- all.datas[!duplicated.indices,]
  
  correct.indices <- sort.list(times.trunc)
  
  timevector <- times.trunc[correct.indices]
  data <- datas.trunc[correct.indices,]
  
  results <- list("data" = data, "timevector" = timevector)
  return(results)
}


# Model fitting ---------------------------------------------------------------


FitWithStep <- function(training.set, test.set = NULL,
                        object.formula, upper.formula, lower.formula,
                        direction, steps = 1000) {
  # Chooses a linear model by a stepwise algorithm and predicts a fit in an
  # independent test set.
  #
  # Args:
  #   training.set: a data matrix
  #   test.set: a data matrix
  #   object.formula: a formula viable for a linear model
  #   upper.formula: a formula viable for a linear model
  #   lower.formula:a formula viable for a linear model
  #   direction: both/forward/backward
  #   steps: the number of steps the algorithm is allowed to take
  #
  # Returns:
  #   A list of coefficients, residuals and fitted values.
  
  # saving the training and test set to global environment
  # NOTE: some algorithms do not work without this!
  # NOTE: step function does not work without this workaround, no idea why
  training.set <<- training.set
  test.set <<- test.set
  
  # generating the linear models
  # note that they are dependent on the data used
  object.lm <- lm(object.formula, data = training.set)
  upper.lm <- lm(upper.formula, data = training.set)
  lower.lm <- lm(lower.formula, data = training.set)
  
  # choosing the perfect model
  step.model <- step(object = object.lm,
                    scope = list(upper = upper.lm, lower = lower.lm),
                    direction = direction, trace = 0, steps = steps)
  
  # if there is no test data, the fit is calculated from the training data
  if (is.null(test.set)) {
    test.set <- training.set
  }
  
  # calculating the fit
  fitted.values <- predict(object = step.model, newdata = test.set)
  
  fitted.values <- as.numeric(fitted.values)
  residuals <- as.numeric(test.set[,1] - fitted.values)
  coefficients <- step.model$coefficients
  
  results <- list("coefficients" = coefficients,
                  "residuals" = residuals,
                  "fitted.values" = fitted.values)
  
  class(results) <- "simplelm"
  
  return(results)
}


FitWithGlmnet <- function(training.set, test.set = NULL,
                          alpha, standardize = TRUE,
                          pmax = NULL,
                          choosing.lambda = "1se") {
  # Chooses a best GLMNET model and predicts a fit.
  #
  # Args:
  #   training.set:
  #   test.set:
  #   alpha: from 0 to 1
  #   standardize: TRUE/FALSE
  #
  # Returns:
  #   A list of coefficients, residuals and fitted values.
  
  if (is.null(pmax)) {
    pmax = (ncol(training.set)-1)
  }
  
  # transforming the data to matrices
  training.set.x <- as.matrix(training.set[,-1])
  training.set.y <- training.set[,1]
  
  # if there is no test data, the fit is calculated from the training data
  if (is.null(test.set)) {
    test.set.x <- training.set.x
    test.set.y <- training.set.y
  } else {
    test.set.x <- as.matrix(test.set[,-1])
    test.set.y <- test.set[,1]
  }
  
  #grid <- 10^seq(10, -4, length = 200)
  
  # estimating the glmnet model
  # warnings are suppressed because the size limit (pmax) of the model results
  # into many unimportant warnings
  glmnet.model <- suppressWarnings(
    glmnet(training.set.x, training.set.y, family = "gaussian",
           alpha = alpha, 
           standardize = standardize, pmax = pmax)
  )
  
  # crossvalidating the glmnet model to find out the best value for lambda/s
  # NOTE: some sources say that the hyperparameter estimation should be done in
  # a separate data set and not in the same that was used when estimating the
  # coefficients. this is not implemented.
  filter.for.folds <- IndexVectorToFilter(SplitDataEvenly(training.set.y))
  
  cv.glmnet.model <- suppressWarnings(
    cv.glmnet(training.set.x, training.set.y, alpha = alpha,
              foldid = filter.for.folds, pmax = pmax)
  )
  
  if (choosing.lambda == "1se") {
    best.lambda <- cv.glmnet.model$lambda.1se
  } else {
    best.lambda <- cv.glmnet.model$lambda.min
  }
  
  # choosing the best coefficients
  all.coefficients <- coef(cv.glmnet.model, s = best.lambda)
  all.coef.names <- rownames(all.coefficients)
  nonzero.indices <- which(all.coefficients != 0)
  coefficients <- all.coefficients[nonzero.indices]
  names(coefficients) <- all.coef.names[nonzero.indices]
  
  # predicting the fit and residuals
  fitted.values <- predict.glmnet(object = glmnet.model, newx = test.set.x,
                                  s = best.lambda,
                                  type = "response")
  residuals <- test.set.y - fitted.values
  
  # combining results
  fitted.values <- as.numeric(fitted.values)
  residuals <- as.numeric(residuals)
  
  results <- list("coefficients" = coefficients,
                  "residuals" = residuals,
                  "fitted.values" = fitted.values)
  class(results) <- "simplelm"
  
  return(results)
}


FitWithRegsubsets <- function(training.set, test.set = NULL,
                              x, force.in, method) {
  # Chooses a best subsets model of 10 variables and predicts a fit.
  #
  # Args:
  #   training.set:
  #   test.set:
  #   x: a formula
  #   force.in: the index of the variable that is forced in the model
  #   method: forward/backward/seqrep
  #
  # Returns:
  #   A list of coefficients, residuals and fitted values.
  
  # estimating the best models
  regsubsets.model <- regsubsets(x, data = training.set, nbest = 1, nvmax = 10,
                                 force.in = force.in, method = method)
  
  # if there is no test data, the fit is calculated from the training data
  if (is.null(test.set)) {
    test.set <- training.set
  }
  
  # calculating the fitted values
  fitted.values <- PredictWithRegsubsets(object = regsubsets.model,
                                         newdata = test.set)
  residuals <- test.set[,1] - fitted.values
  
  # the estimated coefficients of a model
  # a coefficient is zero if the variable is not in the model
  id <- dim(summary(regsubsets.model)$which)[1]
  coefficients <- coef(regsubsets.model, id)
  
  fitted.values <- as.numeric(fitted.values)
  residuals <- as.numeric(residuals)
  
  results <- list("coefficients" = coefficients,
                  "residuals" = residuals,
                  "fitted.values" = fitted.values)
  
  class(results) <- "simplelm"
  return(results)
}


FitWithPCR <- function(training.set, test.set = NULL,
                       formula, ncomp = (length(training.set)-1), scale = TRUE,
                       regulation = "none") {
  # Performs a principal component regression and predicts with the model.
  #
  # Args:
  #   training.set:
  #   test.set:
  #   formula:
  #   ncomp: the number of principal components
  #   scale: TRUE/FALSE
  #   regulation: which regulation is chosen (none/eigenvalues/variance)
  #
  # Returns:
  #   A list of coefficients, residuals and fitted values.
  
  pca <- prcomp(~., data = training.set[,-1],
                center = TRUE, scale. = TRUE, tol = NULL)
  
  variables <- switch(regulation,
                      eigenvalues = RegulatePCRUsingEigenvalues(pca),
                      variance = RegulatePCRUsingVariance(pca),
                      none = c("."))
  no.of.variables <- length(variables)
  if (variables[1] == ".") {
    no.of.variables = ncomp
  }
  
  pcr.formula <- as.formula(paste(colnames(training.set)[1], " ~ ",
                                  paste(variables, collapse = " + "),
                                  sep = ""))
  pcr.model <- pcr(pcr.formula, ncomp = no.of.variables, data = training.set,
                   scale = scale, validation = "none")
  coefficients.array <- coef(pcr.model, ncomp = no.of.variables,
                             intercept = TRUE)
  coefficients <- as.numeric(coefficients.array)
  names(coefficients) <- rownames(coefficients.array)
  
  # if there is no test data, the fit is calculated from the training data
  if (is.null(test.set)) {
    test.set <- training.set
  }
  
  # predicts with the model
  fitted.values <- predict(pcr.model, ncomp = no.of.variables,
                           newdata = test.set)
  residuals <- test.set[,1] - fitted.values
  
  fitted.values <- as.numeric(fitted.values)
  residuals <- as.numeric(residuals)
  
  results <- list("coefficients" = coefficients,
                  "residuals" = residuals,
                  "fitted.values" = fitted.values)
  
  class(results) <- "simplelm"
  
  return(results)
}


RegulatePCRUsingEigenvalues <- function(prcomp.object) {
  # Removes the least important variables based on the eigenvalues in principal
  # component analysis.
  #
  # Args:
  #   prcomp.object: an object created by the function prcomp()
  #
  # Returns:
  #   A vector of the names of the variables that are left in the model.
  
  eigenvalues <- prcomp.object$sdev^2
  eigenvectors.abs <- abs(prcomp.object$rotation)
  
  # finding the eigenvalues that are smaller than 0.70 and saving their indices
  # in a decreasing order (from the least important to more important)
  low.eigenvalues <- rev(which(eigenvalues < 0.70))
  
  # saving the variables which we are going to reduce
  variables <- rownames(eigenvectors.abs)
  
  for (ll in low.eigenvalues) {
    # finding the largest coefficient
    deleted.variable <- which.max(eigenvectors.abs[,ll])
    # deleting the variable corresponding the largest coefficient from the
    # eigenvector matrix and the variables
    eigenvectors.abs <- eigenvectors.abs[-deleted.variable,]
    variables <- variables[-deleted.variable]
  }
  
  return(variables)
}


RegulatePCRUsingVariance <- function(prcomp.object) {
  # Removes the least important variables based on the explained variation in
  # principal component analysis.
  #
  # Args:
  #   prcomp.object: an object created by the function prcomp()
  #
  # Returns:
  #   A vector of the names of the variables that are left in the model.
  
  cumulative.prop.of.var <- cumsum(prcomp.object$sdev^2 / sum(prcomp.object$sdev^2))
  eigenvectors.abs <- abs(prcomp.object$rotation)
  
  # finding how many PCs are needed to explain at least 80 % of the variance
  excess.components <- which(cumulative.prop.of.var > 0.80)
  excess.components <- excess.components[-1]
  excess.components <- rev(excess.components)
  
  # saving the initila variables
  variables <- rownames(eigenvectors.abs)
  
  for (ee in excess.components) {
    deleted.variable <- which.max(eigenvectors.abs[,ee])
    eigenvectors.abs <- eigenvectors.abs[-deleted.variable,]
    variables <- variables[-deleted.variable]
  }
  
  return(variables)
}


FitWithStepPCA <- function(training.set, test.set = NULL,
                           direction, steps = 1000) {
  # Selects a principal component regression model via the stepwise algorithm.
  #
  # Args:
  #   training.set:
  #   test.set:
  #   direction: forward/backward/both
  #   steps:
  #
  # Returns:
  #   A list of coefficients, residuals and fitted values.
  
  response.variable <- colnames(training.set)[1]
  
  pca <- prcomp(~., data = training.set[,-1],
                center = TRUE, scale. = TRUE, tol = NULL)
  
  transformed.training.set <- data.frame(training.set[,1], pca$x)
  colnames(transformed.training.set)[1] <- colnames(training.set)[1]
  
  formula.lm.full <- as.formula(paste(response.variable, " ~ .", sep = ""))
  formula.lm.constant <- as.formula(paste(response.variable, " ~ 1", sep = ""))
  
  lm.full <- lm(formula = formula.lm.full,
                data = transformed.training.set)
  lm.constant <- lm(formula = formula.lm.constant,
                    data = transformed.training.set)
  
  step.pca.model <- step(lm.constant,
                         scope = list(upper = lm.full, lower = lm.constant),
                         direction = direction, trace = 0, steps = steps)
  
  if (is.null(test.set)) {
    test.set <- training.set
  }
  
  transformed.test.set <- data.frame(test.set[,1],
                                     TransformToPCACoordinates(test.set[,-1], pca))
  colnames(transformed.test.set)[1] <- colnames(training.set)[1]
  
  fitted.values <- predict(object = step.pca.model,
                           newdata = transformed.test.set)
  residuals <- test.set[,1] - fitted.values
  
  fitted.values <- as.numeric(fitted.values)
  residuals <- as.numeric(residuals)
  
  results <- list("coefficients" = NULL,
                  "residuals" = residuals,
                  "fitted.values" = fitted.values)
  
  class(results) <- "simplelm"
  
  return(results)
}


TransformToPCACoordinates <- function(x, prcomp.object) {
  # Transforms a data matrix to principal coordinates.
  #
  # Args:
  #   x:
  #   prcomp.object:
  #
  # Returns:
  #   A data frame in principal coordinate system.
  
  obs <- dim(x)[1]
  vars <- dim(x)[2]
  
  y <- x - matrix(rep(prcomp.object$center, each = obs), ncol = vars, nrow = obs)
  y <- y / matrix(rep(prcomp.object$scale, each = obs), ncol = vars, nrow = obs)
  y <- as.matrix(y) %*% prcomp.object$rotation
  y <- as.data.frame(y)
  
  colnames(y) <- paste("PC", 1:vars, sep="")
  
  return(y)
}


FitWithPLSR <- function(training.set, test.set = NULL,
                        formula, ncomp, scale, method = "kernelpls") {
  # Performs a partial least squares regression and predicts with the model.
  #
  # Args:
  #   training.set:
  #   test.set:
  #   formula:
  #   ncomp:
  #   scale:
  #
  # Returns:
  #   A vector of fitted values.
  
  # estimates the PC regression model
  plsrmodel <- plsr(formula = formula, data = training.set, scale = scale,
                    method = method, validation = "none")
  
  # if there is no test data, the fit is calculated from the training data
  if (is.null(test.set)) {
    test.set <- training.set
  }
  
  coefficients.array <- coef(plsrmodel, ncomp = ncomp, intercept = TRUE)
  coefficients <- as.numeric(coefficients.array)
  names(coefficients) <- rownames(coefficients.array)
  
  # predicts with the model
  fitted.values <- predict(plsrmodel, ncomp = ncomp, newdata = test.set)
  residuals <- test.set[,1] - fitted.values
  
  fitted.values <- as.numeric(fitted.values)
  residuals <- as.numeric(residuals)
  
  results <- list("coefficients" = coefficients,
                  "residuals" = residuals,
                  "fitted.values" = fitted.values)
  
  class(results) <- "simplelm"
  
  return(results)
}


FitWithBtSVD <- function(training.set, test.set = NULL, P) {
  # Performs a partial least squares regression and predicts with the model.
  # THIS FUNCTION MAY BE BROKEN!!!
  # 
  # Args:
  #   training.set: test.set: P: minimum explanation rate
  # 
  # Returns:
  #   A list of coefficients, residuals and fitted values.
  
  training.set.y <- as.matrix(training.set[,1], ncol = 1)
  training.set.x <- as.matrix(training.set[,-1])
  training.set.x <- scale(training.set.x, center = TRUE, scale = TRUE)
  
  N <- nrow(training.set.x)
  M <- ncol(training.set.x)
  
  decomposition <- svd(training.set.x, nu = M, nv = M)
  training.U <- decomposition$u
  singular.values <- decomposition$d
  V <- decomposition$v
  
  # estimates the PC regression model
  btsvd.model <- BayesianTruncatedSVD(U = training.U, y = training.set.y,
                                  singular.values = singular.values, P = P)
  
  # if there is no test data, the fit is calculated from the training data
  if (is.null(test.set)) {
    test.set <- training.set
  }
  
  test.set.y <- as.matrix(test.set[,1], ncol = 1)
  test.set.x <- as.matrix(test.set[,-1])
  test.set.x <- scale(test.set.x, center = TRUE, scale = TRUE)
  
  test.U <- test.set.x %*% V %*% diag(1/singular.values)
  
  test.U.intercept <- cbind(rep(1, times = nrow(test.U)), test.U)
  test.Uq <- test.U.intercept[,1:ncol(btsvd.model$Uq)]
  
  # predicts with the model
  fitted.values <- test.Uq %*% btsvd.model$mu.U
  residuals <- test.set[,1] - fitted.values
  
  fitted.values <- as.numeric(fitted.values)
  residuals <- as.numeric(residuals)
  
  results <- list("coefficients" = NULL,
                  "residuals" = residuals,
                  "fitted.values" = fitted.values)
  
  class(results) <- "simplelm"
  
  return(results)
}


FitWithCorrPCA <- function(training.set, test.set = NULL, min.correlation) {
  # Performs a partial least squares regression and predicts with the model.
  #
  # Args:
  #   training.set:
  #   test.set:
  #   formula:
  #   ncomp:
  #   scale:
  #
  # Returns:
  #   A list of coefficients, residuals and fitted values.
  
  response.variable <- colnames(training.set)[1]
  
  # doing PCA
  pca <- prcomp(~., data = training.set[,-1],
                center = TRUE, scale. = TRUE, tol = NULL)
  
  # calculating the correlation between the response and principal components
  correlations <- cor(training.set[,1], pca$x)
  
  # choosing the components which correlate the most with the repsonse variable
  chosen.components <- which(abs(correlations) >= min.correlation)
  
  names.of.chosen.components <- ""
  if (length(chosen.components) > 0) {
    names.of.chosen.components <- paste("PC", chosen.components, sep = "")
  }
  
  # gathering all model data
  transformed.training.set <- data.frame(training.set[,1], pca$x)
  colnames(transformed.training.set)[1] <- response.variable
  
  # writing the formula
  lm.formula <- as.formula(paste(response.variable, " ~ ",
                                 paste(names.of.chosen.components,
                                       collapse = " + "), " + 1", sep = ""))
  
  # estimating the linear model with the most correlated PCs
  lm.model <- lm(formula = lm.formula, data = transformed.training.set)
  
  if (is.null(test.set)) {
    test.set <- training.set
  }
  
  # transforming the test data
  transformed.test.set <- data.frame(test.set[,1],
                                    TransformToPCACoordinates(test.set[,-1], pca))
  colnames(transformed.test.set)[1] <- response.variable
  
  # predicting
  fitted.values <- predict(lm.model, transformed.test.set)
  residuals <- test.set[,1] - fitted.values
  
  fitted.values <- as.numeric(fitted.values)
  residuals <- as.numeric(residuals)
  
  results <- list("coefficients" = NULL,
                  "residuals" = residuals,
                  "fitted.values" = fitted.values)
  
  class(results) <- "simplelm"
  
  return(results)
}


FitWithLM <- function(training.set, test.set = NULL) {
  # Estimates a full linear model and predicts with the model.
  #
  # Args:
  #   training.set:
  #   test.set:
  #
  # Returns:
  #   A list of coefficients, residuals and fitted values.
  
  response <- colnames(training.set)[1]
  formula <- paste(response, " ~ .", sep = "")
  model <- lm(formula = formula, data = training.set)
  
  if (is.null(test.set)) {
    test.set <- training.set
  }
  
  fitted.values <- as.vector(predict(object = model, newdata = test.set))
  residuals <- test.set[,1] - fitted.values
  
  fitted.values <- as.numeric(fitted.values)
  residuals <- as.numeric(residuals)
  
  results <- list("coefficients" = model$coefficients,
                  "residuals" = residuals,
                  "fitted.values" = fitted.values)
  
  class(results) <- "simplelm"
  
  return(results)
}


PredictWithRegsubsets = function(object, newdata) {
  # Predicts with a model from a regsubsets object.
  #
  # Args:
  #   object: a regsubsets object created via the regsubsets() function
  #   newdata: to which (independent) data are the predictions based on
  #
  # Returns:
  #   A vector of fitted values.
  
  # https://lagunita.stanford.edu/c4x/HumanitiesScience/StatLearning/asset/ch6.html
  
  # which id corresponds to the last model (having 10 variables)
  id <- dim(summary(object)$which)[1]
  
  # the estimated coefficients of a model
  # a coefficient is zero if the variable is not in the model
  coefficients <- coef(object, id)
  
  # creating a model matrix in which the first column is ones and the rest are
  # all the possible variances
  variables <- names(newdata)
  model.formula <- as.formula(paste(variables[1], " ~ ",
                                   paste(variables[-1], collapse = " + "),
                                   sep = ""))
  model.matrix <- model.matrix(model.formula, data = newdata)
  
  # predicting the fitted values
  fitted.values <- model.matrix[, names(coefficients)] %*% coefficients
  
  return(fitted.values)
}


BayesianTruncatedSVD <- function(U, y, singular.values, P) {
  # THIS FUNCTION MAY BE BROKEN!!!
  #
  # Estimates parameters in a Bayesian linear model via the truncated SVD
  # method. The function is based on the equations presented in the paper
  # "Linear Models for Airborne-Laser-Scanning-Based Operational Forest
  # Inventory With Small Field Sample Size and Highly Correlated LiDAR Data" by
  # Junttila et al. (2015). Equations are referenced by their numbers where
  # applicable. The learning algorithm steps are based on the paper "Bayesian
  # Inference: An Introduction to Principles and Practice in Machine Learning"
  # by Tipping (2006), and the steps are referenced where applicable.
  # 
  # Args:
  #   U: left singular vector of the singular value decomposition of the
  #     predictors (matrix, NxM)
  #   y: predictand (column vector, Nx1) 
  #   singular.values: singular values of the SVD of the predictors (vector,
  #     length N)
  #   P: minimum explanation rate (scalar in interval (0,1))
  # 
  # Returns:
  #   A list with the following arguments:
  #   mu.U: mean vector
  #   sigma.lower.sq: error variance (in the code referenced as tau^(-1))
  #   alpha.vector: alpha vector with the first element being alpha0 and the
  #     rest alpha
  #   Uq: reduced left singular vector with an intercept
  
  # calculating the number of predictors needed (Eq. 2)
  explanation.rate <- cumsum(singular.values)/sum(singular.values)
  q <- which.max(explanation.rate > P)
  
  # adding an intercept to the predictor matrix (Eq. 3)
  U.with.intercept <- cbind(rep(1, times = nrow(U)), U)
  
  # saving only needed predictors
  Uq <- U.with.intercept[,1:(q+1)]
  singular.values.q <- singular.values[1:q]
  
  # calculating sizes
  N <- nrow(Uq)  # n
  M <- ncol(Uq)  # q+1
  K <- ncol(y)   # 1
  
  # defining bounds
  max.no.of.iterations <- 10000
  tolerance.of.alphas <- 100000000
  
  # vector of singular values including the intercept
  # (diag(Sq) in the paper in Eq. 3)
  lambda <- c(1, singular.values.q)
  lambda.minus.sq <- lambda^(-2)
  
  # Beginning the learning algorithm!
  # Step 1: Initialize all alphas and tau
  
  # initializing the inverse variances of the regression model parameters
  alpha.previous <- 0.1
  alpha.new <- alpha.previous
  alpha0.previous <- 0.1
  alpha0.new <- alpha0.previous
  alpha.vector <- c(alpha0.new, rep(alpha.new, times = M-1))
  
  # initializing the inverse error variance (tau is actually sigma.lower^(-2))
  tau <- 1
  
  # iteration loop (steps 2-4) begins
  for (ii in 1:max.no.of.iterations) {
    
    # Step 2: Compute weight posterior sufficient statistics mu.U and
    #         sigma.upper.U
    
    # covariance (Eq. 8 but only the "U part")
    sigma.upper.U <- ginv(tau * t(Uq) %*% Uq +
                            diag(lambda.minus.sq * alpha.vector))
    # the last term is simplified by results from multiplying diagonal matrices
    
    # mean (Eq. 9 but only the "U part")
    mu.U <- tau * sigma.upper.U %*% t(Uq) %*% y
    
    # Step 3: Compute all gammas, then re-estimate alphas and tau
    
    # under Eq. 13
    gamma <- 1 - alpha.vector * lambda.minus.sq * diag(sigma.upper.U)
    
    alpha0.new <- gamma[1] / mu.U[1]  # Eq. 11
    alpha.new <- sum(gamma[-1]) / sum(mu.U^2 * lambda.minus.sq)  # Eq. 12
    alpha.vector <- c(alpha0.new, rep(alpha.new, times = M-1))
    
    tau <- (N - sum(gamma)) / crossprod(y - Uq %*% mu.U)  # Eq. 13
    tau <- as.numeric(tau)
    
    # Step 4: Repeat until convergence (so we are checking the convergenve
    #         conditions)
    
    # checking stopping conditions
    enough.iterations <- (ii > 50)
    alpha.too.large <- (alpha.new > tolerance.of.alphas)
    convergence.reached <- ((abs(alpha0.previous-alpha0.new) +
                               abs(alpha.previous-alpha.new)) < (0.0001/N))
    
    # possible scenarios which are (almost) mutually exclusive
    if (ii == max.no.of.iterations) {
      warning("Maximum number of iterations reached!")
    } else if (enough.iterations & alpha.too.large) {
      warning("Alphas tend towards infinity!")
      break
    } else if (enough.iterations & convergence.reached) {
      # cat("Convergence reached!\n")
      break
    } else {
      # Iterations continue...
      alpha.previous <- alpha.new
      alpha0.previous <- alpha0.new
      # alpha.vector already contains the new values
    }
    
  }
  
  # returning results
  results <- list(mu.U = mu.U, sigma.lower.sq = tau^(-1),
                  alpha.vector = alpha.vector, Uq = Uq)
  return(results)
  
}


# Wrappers --------------------------------------------------------------------


Crossvalidate <- function(type, data, timevector, ...) {
  # Crossvalidates a selected algorithm with selected parameters.
  #
  # Args:
  #   type: which model selection algorithm will be used
  #   data:
  #   timevector:
  #   ...: passed to the algorithm
  #
  # Returns:
  #   A list of residuals and fitted values.
  
  # check whether the type argument is correct
  types <- c("step", "regsubsets", "glmnet", "pcr", "plsr", "steppca", "btsvd",
             "corrpca", "lm")
  if (!(type %in% types)) {
    print("Write a correct type!")
    return()
  }
  
  # the number of observations in the 'data' data frame
  no.of.points <- nrow(data)
  
  # calculating the indices where the data is splitted
  # NOTE: works only on yearly data! support on season data dropped!
  fold.indices <- SplitDataEvenly(timevector = timevector)
  
  # how many groups we get from splitting
  no.of.seasons <- length(fold.indices) - 1
  
  # saving the results
  fitted.values <- rep(NA, times = no.of.points)
  residuals <- rep(NA, times = no.of.points)
  
  for (ii in 1:no.of.seasons) {
    
    # defining the current test set indices
    test.indices <- fold.indices[ii]:(fold.indices[ii+1]-1)
    
    # the data outside current validation region is assigned as training data
    #  set and the data between current validation region is validation data
    training.set <- data[-test.indices,]
    test.set <- data[test.indices,]
    
    # removing constants from the data
    training.set <- RemoveConstantVariables(training.set)
    test.set <- test.set[,names(test.set) %in% names(training.set)]
    
    # this variable will serve as a saved state for fitted values
    # the switch function chooses the correct fitting algorithm based on type
    cv.results <- switch(type,
                     step = FitWithStep(training.set = training.set,
                                        test.set = test.set, ...),
                     regsubsets = FitWithRegsubsets(training.set = training.set,
                                                    test.set = test.set, ...),
                     glmnet = FitWithGlmnet(training.set = training.set,
                                            test.set = test.set, ...),
                     pcr = FitWithPCR(training.set = training.set,
                                      test.set = test.set, ...),
                     plsr = FitWithPLSR(training.set = training.set,
                                        test.set = test.set, ...),
                     steppca = FitWithStepPCA(training.set = training.set,
                                              test.set = test.set, ...),
                     btsvd = FitWithBtSVD(training.set = training.set,
                                          test.set = test.set, ...),
                     corrpca = FitWithCorrPCA(training.set = training.set,
                                              test.set = test.set, ...),
                     lm = FitWithLM(training.set = training.set,
                                    test.set = test.set))
    
    fitted.values[test.indices] <- cv.results$fitted.values
    residuals[test.indices] <- cv.results$residuals
  }
  results <- list("residuals" = residuals, "fitted.values" = fitted.values)
  return(results)
}


TestAlgorithms <- function(data, timevector) {
  # Tests all interesting algorithms with varying arguments.
  # 
  # Args: 
  #   data:
  #   timevector:
  # 
  # Returns:
  #   A list with elements `residuals`, `fitted.values`, `response` which only
  #   lists the values in the response variable, and `elapsed.time` which keeps
  #   track of the fitted values by different algorithms and the elapsed time in
  #   running the algorithm.
  
  # calculating the index for regsubsets (column id IN PREDICTOR SET)
  index.of.T2 <- (which(colnames(data) == "T2") - 1)
  
  # The first column of `data` defines the dependent response variable
  # `HAVAINTO` or `BIAS`. `BIAS` is defined as `HAVAINTO-T2`. The remaining 51
  # columns define the independent predictors where the most relevant predictors
  # is `T2`, the temperature at two meters. The predictors are forecasts and 
  # `HAVAINTO` is the realised temperature.
  
  response <- colnames(data)[1]
  
  fInitial <- as.formula(paste(response, " ~ 1", sep = ""))
  fFull <- as.formula(paste(response, " ~ .", sep = ""))
  fT2 <- as.formula(paste(response, " ~ T2", sep = ""))
  
  # First, let us create a list to store our crossvalidation results:
  fitted.values.by.algorithm <- as.data.frame(matrix(NA, ncol = 0,
                                                     nrow = nrow(data)))
  residuals.by.algorithm <- as.data.frame(matrix(NA, ncol = 0,
                                                     nrow = nrow(data)))
  elapsed.time.by.algorithm <- NULL
  
  
  # Basic models --------------------------------------------------------------
  
  # for comparison purposes, we are going to input ECMWF's forecast as one model
  # and a linear model with ALL predictors as another model
  if (response == "HAVAINTO") {
    fitted.values.by.algorithm$T2 <- data$T2
    residuals.by.algorithm$T2 <- data[,1] - data$T2
  } else {
    fitted.values.by.algorithm$T2 <- rep(0, times = nrow(data))
    residuals.by.algorithm$T2 <- data[,1]
  }
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm, T2 = 0)
  
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "lm",
                                   data = data, timevector = timevector)
  )
  fitted.values.by.algorithm$FullLm <- tmp.cv.result$fitted.values
  residuals.by.algorithm$FullLm <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 FullLm = tmp.time.result[[3]])
  
  
  # Default model -------------------------------------------------------------
  
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "step",
                                   data = data, timevector = timevector,
                                   object.formula = fInitial,
                                   upper.formula = fFull,
                                   lower.formula = fInitial,
                                   direction = "forward",
                                   steps = 10)
  )
  fitted.values.by.algorithm$Defaul <- tmp.cv.result$fitted.values
  residuals.by.algorithm$Defaul <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 Defaul = tmp.time.result[[3]])
  
  
  # Stepwise algorithm --------------------------------------------------------
  
  # discarded
  
  
  # Best subsets -------------------------------------------------------------
  
  # Regs01
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "regsubsets",
                                   data = data, timevector = timevector,
                                   x = fFull, force.in = NULL,
                                   method = "forward")
  )
  fitted.values.by.algorithm$Regs01 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$Regs01 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 Regs01 = tmp.time.result[[3]])
  
  # Regs02
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "regsubsets",
                                   data = data, timevector = timevector,
                                   x = fFull, force.in = index.of.T2,
                                   method = "forward")
  )
  fitted.values.by.algorithm$Regs02 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$Regs02 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 Regs02 = tmp.time.result[[3]])
  
  # Regs03
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "regsubsets",
                                   data = data, timevector = timevector,
                                   x = fFull, force.in = NULL,
                                   method = "backward")
  )
  fitted.values.by.algorithm$Regs03 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$Regs03 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 Regs03 = tmp.time.result[[3]])
  
  # Regs04
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "regsubsets",
                                   data = data, timevector = timevector,
                                   x = fFull, force.in = index.of.T2,
                                   method = "backward")
  )
  fitted.values.by.algorithm$Regs04 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$Regs04 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 Regs04 = tmp.time.result[[3]])
  
  # Regs05
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "regsubsets",
                                   data = data, timevector = timevector,
                                   x = fFull, force.in = NULL,
                                   method = "seq")
  )
  fitted.values.by.algorithm$Regs05 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$Regs05 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 Regs05 = tmp.time.result[[3]])
  
  # Regs06
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "regsubsets",
                                   data = data, timevector = timevector,
                                   x = fFull, force.in = index.of.T2,
                                   method = "seq")
  )
  fitted.values.by.algorithm$Regs06 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$Regs06 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 Regs06 = tmp.time.result[[3]])
  
  
  # Glmnet -------------------------------------------------------------------
  
  # Glmn01
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "glmnet",
                                   data = data, timevector = timevector,
                                   alpha = 1, standardize = TRUE)
  )
  fitted.values.by.algorithm$Glmn01 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$Glmn01 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 Glmn01 = tmp.time.result[[3]])
  
  # Glmn02
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "glmnet",
                                   data = data, timevector = timevector,
                                   alpha = 0.80, standardize = TRUE)
  )
  fitted.values.by.algorithm$Glmn02 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$Glmn02 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 Glmn02 = tmp.time.result[[3]])
  
  # Glmn03
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "glmnet",
                                   data = data, timevector = timevector,
                                   alpha = 0.60, standardize = TRUE)
  )
  fitted.values.by.algorithm$Glmn03 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$Glmn03 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 Glmn03 = tmp.time.result[[3]])
  
  # GlmnR1
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "glmnet",
                                   data = data, timevector = timevector,
                                   alpha = 1, pmax = 11,
                                   choosing.lambda = "min")
  )
  fitted.values.by.algorithm$GlmnR1 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$GlmnR1 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 GlmnR1 = tmp.time.result[[3]])
  
  # GlmnR2
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "glmnet",
                                   data = data, timevector = timevector,
                                   alpha = 0.80, pmax = 11,
                                   choosing.lambda = "min")
  )
  fitted.values.by.algorithm$GlmnR2 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$GlmnR2 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 GlmnR2 = tmp.time.result[[3]])
  
  # GlmnR3
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "glmnet",
                                   data = data, timevector = timevector,
                                   alpha = 0.60, pmax = 11,
                                   choosing.lambda = "min")
  )
  fitted.values.by.algorithm$GlmnR3 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$GlmnR3 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 GlmnR3 = tmp.time.result[[3]])
  
  # GlmnM1
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "glmnet",
                                   data = data, timevector = timevector,
                                   alpha = 1, pmax = 11,
                                   choosing.lambda = "1se")
  )
  fitted.values.by.algorithm$GlmnM1 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$GlmnM1 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 GlmnM1 = tmp.time.result[[3]])
  
  # GlmnM2
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "glmnet",
                                   data = data, timevector = timevector,
                                   alpha = 0.80, pmax = 11,
                                   choosing.lambda = "1se")
  )
  fitted.values.by.algorithm$GlmnM2 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$GlmnM2 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 GlmnM2 = tmp.time.result[[3]])
  
  # GlmnM3
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "glmnet",
                                   data = data, timevector = timevector,
                                   alpha = 0.60, pmax = 11,
                                   choosing.lambda = "1se")
  )
  fitted.values.by.algorithm$GlmnM3 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$GlmnM3 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 GlmnM3 = tmp.time.result[[3]])
  
  
  
  ## Principal component regression -------------------------------------------
  
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "pcr",
                                   data = data, timevector = timevector,
                                   formula = fFull, ncomp = 5, scale = FALSE)
  )
  fitted.values.by.algorithm$PCR001 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$PCR001 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 PCR001 = tmp.time.result[[3]])

  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "pcr",
                                   data = data, timevector = timevector,
                                   formula = fFull, ncomp = 5, scale = TRUE)
  )
  fitted.values.by.algorithm$PCR002 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$PCR002 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 PCR002 = tmp.time.result[[3]])

  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "pcr",
                                   data = data, timevector = timevector,
                                   formula = fFull, ncomp = 10, scale = FALSE)
  )
  fitted.values.by.algorithm$PCR003 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$PCR003 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 PCR003 = tmp.time.result[[3]])
  
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "pcr",
                                   data = data, timevector = timevector,
                                   formula = fFull, ncomp = 10, scale = TRUE)
  )
  fitted.values.by.algorithm$PCR004 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$PCR004 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 PCR004 = tmp.time.result[[3]])
  
  
  # Regulated PCR -------------------------------------------------------------
  
  # discarded

  
  # Partial least squares regression -----------------------------------------
  
  # PLSR01
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "plsr",
                                   data = data, timevector = timevector,
                                   formula = fFull, ncomp = 5, scale = FALSE,
                                   method = "kernelpls")
  )
  fitted.values.by.algorithm$PLSR01 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$PLSR01 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 PLSR01 = tmp.time.result[[3]])
  # PLSR02
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "plsr",
                                   data = data, timevector = timevector,
                                   formula = fFull, ncomp = 5, scale = TRUE,
                                   method = "kernelpls")
  )
  fitted.values.by.algorithm$PLSR02 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$PLSR02 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 PLSR02 = tmp.time.result[[3]])
  
  # PLSR03
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "plsr",
                                   data = data, timevector = timevector,
                                   formula = fFull, ncomp = 10, scale = FALSE,
                                   method = "kernelpls")
  )
  fitted.values.by.algorithm$PLSR03 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$PLSR03 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 PLSR03 = tmp.time.result[[3]])
  
  # PLSR04
  tmp.time.result <- system.time(
    tmp.cv.result <- Crossvalidate(type = "plsr",
                                   data = data, timevector = timevector,
                                   formula = fFull, ncomp = 10, scale = TRUE,
                                   method = "kernelpls")
  )
  fitted.values.by.algorithm$PLSR04 <- tmp.cv.result$fitted.values
  residuals.by.algorithm$PLSR04 <- tmp.cv.result$residuals
  elapsed.time.by.algorithm <- c(elapsed.time.by.algorithm,
                                 PLSR04 = tmp.time.result[[3]])
  
  
  # Bayesian model via truncated SVD ------------------------------------------
  
  # discarded
  
  results <- list(fitted.values = fitted.values.by.algorithm,
                  residuals = residuals.by.algorithm,
                  response = data[, 1, drop = FALSE],
                  elapsed.time = elapsed.time.by.algorithm)
  
  return(results)
  
}


LoopAlgorithmsThroughDatas <- function(stations, utcs, lead.times,
                                       seasons = c(0), responses = c(1),
                                       response.variable = "HAVAINTO",
                                       what.data = "all") {
  # Automatizes the looping through various data sets in order to decide whether
  # an algorithm performs well or not.
  #
  # Args:
  #   stations:
  #   utcs:
  #   lead.times:
  #   seasons:
  #   responses:
  #
  # Returns:
  #   A list with elements fitted.values, residuals, response, elapsed.time, 
  #   no.of.points, and lead.times similarly to TestAlgorithms. The results from
  #   multiple data sets follow each other in the matrices. The list elements 
  #   no.of.points tells to how many data points there were in a dataset (length
  #   is the number of datasets examined) and lead.times just returns the
  #   argument lead.times.
  
  fitted.values <- NULL
  residuals <- NULL
  response <- NULL
  elapsed.time <- NULL
  no.of.points <- NULL
  
  for (ss in stations) {
    for (uu in utcs) {
      for (ll in lead.times) {
        for (ee in seasons) {
          for (rr in responses) {
            print(paste("Lead time index", ll, "begins!", sep = " "))
            dataobject <- LoadCleanedData(station = ss, utc = uu,
                                          lead.time = ll, season = ee,
                                          response = rr,
                                          what.data = what.data)
            data <- dataobject$data
            timevector <- dataobject$timevector
            
            if (response.variable == "BIAS") {
              data[,1] <- data$HAVAINTO - data$T2
              colnames(data)[1] <- "BIAS"
            }
            
            results <- TestAlgorithms(data = data, timevector = timevector)
            
            fitted.values <- rbind(fitted.values, results$fitted.values)
            residuals <- rbind(residuals, results$residuals)
            response <- rbind(response, results$response)
            elapsed.time <- rbind(elapsed.time, results$elapsed.time)
            no.of.points <- c(no.of.points, length(timevector))
          }
        }
      }
    }
  }
  
  results <- list(fitted.values = fitted.values,
                  residuals = residuals,
                  response = response,
                  elapsed.time = elapsed.time,
                  no.of.points = no.of.points,
                  lead.times = lead.times)
  return(results)
  
}


# Analyzing and plotting ------------------------------------------------------


MAE <- function(x) {
  # Calculates MAE.
  result <- mean(abs(x))
  return(result)
}


MSE <- function(x) {
  # Calculates MSE.
  result <- mean(x^2)
  return(result)
}


RMSE <- function(x) {
  # Calculates RMSE.
  result <- sqrt(MSE(x))
  return(result)
}


CalculateMeasure <- function(residuals, type,
                             no.of.points = nrow(residuals)) {
  # Calculates either MAE or RMSE of the results.
  #
  # Args:
  #   residuals: a residual matrix
  #   type: the error measure (mae/rmse/mse)
  #   no.of.points: a scalar or a vector (a scalar calculates a single measure
  #   of all residuals, a vector divides the residuals into groups)
  #
  # Returns:
  #   A named vector with measures.
  
  # if no.of.points is a vector, measure is calculated separately for all
  factors <- as.factor(rep(1:length(no.of.points), times = no.of.points))
  split.list <- split(x = residuals, f = factors)
  
  measures <- matrix(NA, ncol = ncol(residuals), nrow = length(no.of.points))
  colnames(measures) <- colnames(residuals)
  
  for (ii in 1:length(no.of.points)) {
    measures[ii,] <- switch(type,
                            rmse = apply(X = split.list[[ii]], MARGIN = 2,
                                         FUN = RMSE),
                            mse = apply(X = split.list[[ii]], MARGIN = 2,
                                        FUN = MSE),
                            mae = apply(X = split.list[[ii]], MARGIN = 2,
                                        FUN = MAE))
  }
  
  measures <- as.data.frame(measures)
  rownames(measures) <- NULL
  
  return(measures)
}


CalculateSkill <- function(...) {
  # Calculates a skill score based on MAE or MSE.
  #
  # Args:
  #   ...: passed to function CalculateMeasure
  #
  # Returns:
  #   A named vector with skill score
  
  measure <- CalculateMeasure(...)
  skill <- 1 - measure / measure[,match("T2", colnames(measure))]
  
  return(skill)
}


PlotResidualDistribution <- function(residuals) {
  # Plots a box-and-whiskers plot of the results in TestAlgorithms and
  # LoopAlgorithmsThroughDatas data frames.
  #
  # Args:
  #   residuals: a residual matrix
  #
  # Results:
  #   A lattice plot.
  
  residuals.long <- melt(residuals)
  
  tplot <- bwplot(value ~ variable, data = residuals.long, pch = "|",
                  scales = list(x = list(rot = 45)),
                  ylab = "Residual")
  return(tplot)
  # IT ALSO WORKS ON ELAPSED.TIME WHEN IT IS TRANSFORMED TO A DATA FRAME
}


PlotMeasure <- function(object, type, skill = FALSE) {
  # Plots an error measure (MAE/RMSE) in size order.
  #
  # Args:
  #   object: an object from TestAlgorithms
  #   type: mae/rmse/mse
  #   skill: TRUE/FALSE if we are interested in skill score instead of measure
  #
  # Returns:
  #   A plot.
  
  residuals <- object$residuals
  
  if (skill == FALSE) {
    measure <- CalculateMeasure(residuals = residuals, type = type)
    ylab <- toupper(type)
  } else if (skill == TRUE) {
    measure <- CalculateSkill(residuals = residuals, type = type)
    ylab <- paste(toupper(type), " Skill", sep = "")
  }
  
  statistics <- data.frame(model = colnames(measure),
                           measure = as.numeric(measure),
                           row.names = NULL)
  
  statistics$model <- factor(statistics$model,
                             levels = statistics[order(statistics$measure),
                                                 "model"])
  
  # making a vector all values FALSE except TRUE for Defaul, T2, and FullLm
  pos <- rep(FALSE, times = length(statistics$model))
  pos[1:3] <- TRUE
  
  col <- c("bisque", "aquamarine")
  default.index <- match("T2", colnames(measure))
  
  tplot <- barchart(measure ~ model, data = statistics,
                    scales = list(x = list(rot = 45)),
                    col = col[pos+1],
                    ylab = ylab,
                    origin = 0,
                    panel = function(x,y,...) {
                      panel.barchart(x,y,...)
                      panel.abline(h = measure[,default.index],
                                   col.line = "red", lty = 3)
                    })
  if (skill == FALSE) {
    tplot <- update(tplot, ylim = c(0, NA))
  }
  return(tplot)
}


PlotMAEandRMSE <- function(object) {
  # Plots MAE and RMSE to the same plot.
  #
  # Args:
  #   object: from the function TestAlgorithms
  #
  # Returns:
  #   A plot.
  
  residuals <- object$residuals
  
  no.of.models <- ncol(residuals)
  names.of.models <- colnames(residuals)
  
  mae <- CalculateMeasure(residuals, type = "mae")
  rmse <- CalculateMeasure(residuals, type = "rmse")
  
  tmp <- data.frame(model = rep(names.of.models, times = 2),
                    value = c(t(mae), t(rmse)),
                    type = rep(c("MAE", "RMSE"), each = no.of.models))
  print(tmp)
  default.index <- match("T2", colnames(mae))
  
  tplot <- barchart(value ~ model, data = tmp, groups = factor(type),
                    auto.key = list(points = FALSE, rectangles = TRUE),
                    scales = list(x = list(rot = 45)),
                    ylab = "MAE/RMSE",
                    ylim = c(0, NA),
                    panel = function(x,y,...) {
                      panel.barchart(x,y,...)
                      panel.abline(h = rmse[1,default.index], col.line = "red",
                                   lty = 3)
                      panel.abline(h = mae[1,default.index], col.line = "red",
                                   lty = 3)
                    })
  return(tplot)
}


PlotMeasureAgainstLeadTime <- function(object, type, skill = FALSE,
                                       lead.time.indices = object$lead.times,
                                       model.names = c("T2", "FullLm",
                                                       "Defaul")) {
  
  # let us first calculate ALL measures with ALL lead times
  if (skill == FALSE) {
    measure <- CalculateMeasure(residuals = object$residuals, type = type,
                                no.of.points = object$no.of.points)
    ylab <- toupper(type)
  } else if (skill == TRUE) {
    measure <- CalculateSkill(residuals = object$residuals, type = type,
                              no.of.points = object$no.of.points)
    ylab <- paste(toupper(type), " Skill", sep = "")
  }
  
  # then let us separate the lead times which we truly want to plot
  measure <- measure[lead.time.indices,]
  
  lead.times <- as.numeric(FORECASTS[lead.time.indices])
  
  left.hand.side <- paste("measure$", model.names, sep = "", collapse = " + ")
  formula <- as.formula(paste(left.hand.side, " ~ lead.times", sep = ""))
  
  tplot <- xyplot(formula,
                  type = "o", auto.key = list(points = TRUE),
                  ylab = ylab, xlab = "Lead time",
                  scales = list(x = list(at = seq(from = 0, to = 240, by = 6))))
  if (skill == FALSE) {
    tplot <- update(tplot, ylim = c(0, NA))
  }
  
  return(tplot)
}


PlotMeasureWithLimitations <- function(object, type, skill = FALSE,
                                       lead.time.indices = object$lead.times,
                                       limitation) {
  # let us first calculate ALL measures with ALL lead times
  if (skill == FALSE) {
    measure <- CalculateMeasure(residuals = object$residuals, type = type,
                                no.of.points = object$no.of.points)
    ylab <- toupper(type)
  } else if (skill == TRUE) {
    measure <- CalculateSkill(residuals = object$residuals, type = type,
                              no.of.points = object$no.of.points)
    ylab <- paste(toupper(type), " Skill", sep = "")
  }
  
  # then let us separate the lead times which we truly want to plot
  measure <- measure[lead.time.indices,]
  if (skill == FALSE) {
    good.models <- apply(X = measure, MARGIN = 2,
                         FUN = function(x) all(x < measure$T2))
  } else if (skill == TRUE) {
    good.models <- apply(X = measure, MARGIN = 2, FUN = function(x) all(x < 0))
  }
  
  model.names <- colnames(object$residuals)[!good.models]
  tplot <- PlotMeasureAgainstLeadTime(object = object, type = type,
                                      skill = skill,
                                      lead.time.indices = lead.time.indices,
                                      model.names = model.names)
  return(tplot)
}


PlotResidualsAgainstLeadTime <- function(object, model.name) {
  
  lead.times <- as.numeric(FORECASTS[object$lead.times])
  lead.time.factor <- as.factor(rep(lead.times, times = object$no.of.points))
  index <- match(model.name, colnames(object$residuals))
  residuals <- object$residuals[,index]
  
  tplot <- bwplot(residuals ~ lead.time.factor, pch = "|",
                  scales = list(x = list(rot = 90)),
                  ylab = "Residual in Kelvins",
                  xlab = "Lead time in hours")
  return(tplot)
}


PlotObservationAgainstFits <- function(object, model.name) {
  fitted.values <- object$fitted.values
  index <- match(model.name, colnames(object$fitted.values))
  
  tplot <- xyplot(object$response[,1] ~ object$fitted.values[,index],
                  ylab = "Observation",
                  xlab = paste("Fitted value from ", model.name, sep = ""),
                  panel = function(x,y,...) {
                    panel.xyplot(x,y,...)
                    panel.abline(a = 0, b = 1, col.line = "red", lty = 3)
                  })
  return(tplot)
}
