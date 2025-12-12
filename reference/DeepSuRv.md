# DeepSuRv Neural Network Model (R6 Class)

DeepSuRv Neural Network Model (R6 Class)

DeepSuRv Neural Network Model (R6 Class)

## Value

A fully constructed DeepSuRv R6 object

## Details

An R6-based DeepSurv neural network for survival analysis using `torch`.
Supports feature standardization, training, risk prediction, and
bootstrap C-index evaluation.

## Public fields

- `model`:

  The underlying `nn_module` representing the neural network.

- `n_in`:

  Number of input features.

- `hidden_layers`:

  Integer vector specifying hidden layer sizes.

- `dropout`:

  Dropout probability applied to hidden layers.

- `learning_rate`:

  Learning rate for Adam optimizer.

- `n_epochs`:

  Number of epochs used during training.

- `mu`:

  Column means used for feature standardization.

- `sigma`:

  Column standard deviations used for feature standardization.

## Methods

### Public methods

- [`DeepSuRv$new()`](#method-DeepSuRv-new)

- [`DeepSuRv$set_standardization()`](#method-DeepSuRv-set_standardization)

- [`DeepSuRv$standardize()`](#method-DeepSuRv-standardize)

- [`DeepSuRv$nll_loss()`](#method-DeepSuRv-nll_loss)

- [`DeepSuRv$train()`](#method-DeepSuRv-train)

- [`DeepSuRv$predict_risk()`](#method-DeepSuRv-predict_risk)

- [`DeepSuRv$bootstrap_cindex()`](#method-DeepSuRv-bootstrap_cindex)

- [`DeepSuRv$clone()`](#method-DeepSuRv-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a DeepSuRv object

#### Usage

    DeepSuRv$new(
      n_in,
      hidden_layers = c(),
      dropout = 0,
      learning_rate = 0.01,
      n_epochs = 50
    )

#### Arguments

- `n_in`:

  Number of input features.

- `hidden_layers`:

  Integer vector specifying hidden layer sizes.

- `dropout`:

  Dropout probability.

- `learning_rate`:

  Learning rate for Adam optimizer.

- `n_epochs`:

  Number of epochs.

#### Returns

A DeepSuRv object

------------------------------------------------------------------------

### Method `set_standardization()`

Compute and store column means and standard deviations for features.

#### Usage

    DeepSuRv$set_standardization(X)

#### Arguments

- `X`:

  Feature matrix (numeric matrix or data.frame).

------------------------------------------------------------------------

### Method `standardize()`

Standardize a feature matrix using stored mean and SD.

#### Usage

    DeepSuRv$standardize(X)

#### Arguments

- `X`:

  Feature matrix to standardize.

#### Returns

Standardized feature matrix.

------------------------------------------------------------------------

### Method `nll_loss()`

Compute negative log partial likelihood (Cox model loss).

#### Usage

    DeepSuRv$nll_loss(pred, t, e)

#### Arguments

- `pred`:

  Tensor of predicted risk scores.

- `t`:

  Tensor of event/censoring times.

- `e`:

  Tensor of event indicators (1 = event, 0 = censored).

#### Returns

Negative log partial likelihood (torch tensor).

------------------------------------------------------------------------

### Method `train()`

Train the DeepSuRv model.

#### Usage

    DeepSuRv$train(X_train, t_train, e_train, verbose = TRUE)

#### Arguments

- `X_train`:

  Training feature matrix.

- `t_train`:

  Event/censoring times.

- `e_train`:

  Event indicator vector.

- `verbose`:

  Logical; print loss updates every 10 epochs.

------------------------------------------------------------------------

### Method `predict_risk()`

Predict risk scores for new data.

#### Usage

    DeepSuRv$predict_risk(X)

#### Arguments

- `X`:

  Feature matrix.

#### Returns

Numeric vector of predicted risk scores.

------------------------------------------------------------------------

### Method `bootstrap_cindex()`

Compute bootstrap C-index confidence intervals.

#### Usage

    DeepSuRv$bootstrap_cindex(X, t, e, n_boot = 500, seed = 42)

#### Arguments

- `X`:

  Feature matrix.

- `t`:

  Event/censoring times.

- `e`:

  Event indicator vector.

- `n_boot`:

  Number of bootstrap iterations (default 500).

- `seed`:

  Random seed (default 42).

#### Returns

List with `cindex`, `lower`, `upper`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DeepSuRv$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
