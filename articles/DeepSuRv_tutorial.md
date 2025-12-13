# Survival Analysis with Neural Networks, a DeepSuRv Tutorial

## Introduction

`DeepSuRv` is an R6 class that reimplements the Python
[DeepSurv](https://github.com/jaredleekatzman/DeepSurv) neural network
for survival analysis using the `torch` package. It allows users to
train a neural network on censored survival data, predict patient risk
scores, and evaluate model performance with bootstrap confidence
intervals for the concordance index (C-index).

In this tutorial, we’ll demonstrate how to build, train, and evaluate a
DeepSuRv model, while explaining the purpose and impact of each argument
users can provide.

## Dataset Preparation

In this example, we will be working with the WHAS500 dataset, which has
500 patients and 14 features. We begin by identifying our training and
test datasets.

``` r
whas_train <- readr::read_csv(
  system.file("extdata/train_test_data/whas_train.csv", package = "DeepSuRv"),
  show_col_types = FALSE
)

whas_test <- readr::read_csv(
  system.file("extdata/train_test_data/whas_test.csv", package = "DeepSuRv"),
  show_col_types = FALSE
)
```

DeepSuRv expects numeric matrices for features (`X`) and numeric vectors
for survival times (`t`) and event indicators (`e`). It is important to
standardize the features so that each has mean 0 and standard
deviation 1. This is the default setting in both this implementation of
DeepSuRv and the original Python version. Standardization ensures that
all features contribute equally to training and prevents features with
larger scales from dominating the model’s learning.

``` r
x_train <- as.matrix(select(whas_train, starts_with("x")))
t_train <- whas_train$time
e_train <- whas_train$event

x_test <- as.matrix(select(whas_test, starts_with("x")))
t_test <- whas_test$time
e_test <- whas_test$event
```

## Building a DeepSuRv Model

When users create a DeepSuRv object, they need to specify the number of
input features,the structure of hidden layers, the dropout rate,
learning rate, and the number of epochs for training. The `n_in`
argument is where users define the number of input features, which
should be the number of columns in their input dataset.`hidden_layers`
is a numeric vector that defines the size of each hidden layer. More
layers increase the model’s capacity to learn complex patterns but also
increase the risk of overfitting. `dropout` is the fraction of neurons
that are randomly turned off during each forward pass in model training
to prevent overfitting. `learning_rate` is the step size of the
optimizer function. It controls the speed of parameter updates during
gradient descent. If it is set too high, training will become faster but
may overshoot minima. `n_epochs` are the number of passes through the
full training dataset. More epochs allow the model to converge to an
optima but too many epochs may result in overfitting.

Below we build a model using the WHAS500 dataset:

``` r
model_whas <- DeepSuRv$new(
n_in = ncol(x_train),
hidden_layers = c(48, 48), # Two hidden layers with 48 neurons each
dropout = 0.147,
learning_rate = 0.067,
n_epochs = 50
)
```

## Training the Model

Training involves passing the dataset’s features and survival outcomes
to the network. The model uses the negative log partial likelihood of
the Cox proportional hazards model as the loss function. The three
required inputs, in order are, the standardized matrix of training
features, survival (or follow-up) times, event indicators (1 if the
event occurred, 0 if censored). There is a fourth optional logical
argument, `verbose`, which prints the training loss values if desired.

``` r
model_whas$set_standardization(x_train)
model_whas$train(x_train, t_train, e_train)
#> Epoch: 10 Loss: 3345.604 
#> Epoch: 20 Loss: 3342.146 
#> Epoch: 30 Loss: 3321.036 
#> Epoch: 40 Loss: 3306.22 
#> Epoch: 50 Loss: 3308.43
```

During training, the network iteratively updates its weights to minimize
the Cox loss. Each epoch processes all training samples once, and the
optimizer adjusts the weights based on the gradient of the loss.

## Making Predictions

After training, users can predict risk scores for new patients. The risk
scores are relative, meaning that higher scores indicate higher
predicted risk. This method automatically standardizes the new data
using the mean and standard deviation computed from the training set.

``` r
scores <- model_whas$predict_risk(x_test)
head(scores)
#> [1] 2.940258 3.224050 2.915402 3.274704 2.960247 4.239460
```

## Evaluation the Model Performance

DeepSuRv provides a method to estimate the C-index with bootstrap
confidence intervals, which quantifies how well the predicted risk
scores align with the observed survival times. Users must provide the
test-equivalent inputs used to train the dataset. That is, a
standardized matrix of test features, survival (or follow-up) times,
event indicators (1 if the event occurred, 0 if censored). Users also
need to specify the number of bootstrap samples to compute confidence
intervals with. The default number of bootstraps is set to 500. A
built-in seed is used in the function for reproducibility. Users can
change the seed with the `seed` argument. The more samples provided will
yield a more stable estimate. The output gives the point estimate a 95%
confidence interval of the C-index of the model. The C-index is a
goodness of fit measure ranging from 0 (completely inaccurate) to 1
(perfectly accurate) that evaluates how often a model’s predictions are
correctly ordered in comparison to the observations true order.

``` r
whas_res <- model_whas$bootstrap_cindex(x_test, t_test, e_test)
whas_res
#> $cindex
#> [1] 0.1988945
#> 
#> $lower
#>      2.5% 
#> 0.1625551 
#> 
#> $upper
#>    97.5% 
#> 0.237385
```
