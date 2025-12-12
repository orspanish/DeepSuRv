
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DeepSuRv

<!-- badges: start -->

<!-- badges: end -->

DeepSuRv is an R implementation of the DeepSurv feed-forward neural
network-based Cox proportional hazards model originally developed in
Python using the Theano and Lasagne libraries as the main NN
architecture: <https://github.com/jaredleekatzman/DeepSurv.git>

DeepSuRv offers:

- DeepSurv’s neural network survival model in a full native-R
  implementation backed by the `torch` R package. `torch` is R’s open
  source framework equivalent to Python’s `PyTorch`.

- Support for flexible network architectures that the user can define
  (e.g. number of layers, activations, dropout rates, etc.).

- Functions for training, predicting, and evaluating deep survival
  models.

- Integration with the `survex` R package to generate counterfactual
  explanations for individual survival predictions.

- A user-friendly workflow that promotes reproducibility and
  interpretability.

DeepSuRv enables researchers to train and interpret neural network
survival models through counterfactual reasoning in R.

## Key Features

### DeepSurv Model in R

- Feed-forward neural network that produces log-hazard outputs

- Trained using the partial log-likelihood loss of the Cox model

- GPU support via `torch` when applicable

- Compatibility with tidy data workflows

- Returns concordance index (C-index) and loss during training

### Counterfactual Explanations with `survex`

DeepSuRv adds new functionality to the original DeepSurv workflow by
enabling counterfactual explanations for the neural network model it
creates using. `survex`: <https://github.com/ModelOriented/survex.git>.

`survex` provides infrastructure for:

- Counterfactual survival curves

- Perturbation-based variable importance

- Model stability checks

- Generation of actionable and human-interpretable explanations

DeepSuRv’s prediction interface is fully compatible with `survex`, so
users can pass DeepSuRv models directly into the counterfactual
functions found in the `survex` package (more to follow below).

## Installation

You can install the development version of DeepSuRv from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("orspanish/DeepSuRv")
```

- **German Breast Cancer Study Group (GBSG2):** 686 patients and 8
  features (Age, Menopausal status, tumor localization, tumor size,
  estrogen and progesterone receptor status, tumor grading, histologic
  tumor type, and number of involved lymph nodes); The endpoint is
  recurrence free survival, which occurred for 299 patients (43.6%).

- **WHAS500 (Worcester Heart Attack Study):** 500 patients and 14
  features (AFIB indicator, Age in years at hospital admission, Complete
  heart block indicator, BMI, Congestive heart complications indicator,
  History of cardiovascular disease indicator, Initial diastolic blood
  pressure, Initial systolic blood pressure, gender, Initial heart rate,
  Length of hospital stay, Acute myocardial infarction (MI) order, MI
  type, Cardiogenic shock indicator); The endpoint is death, which
  occurred for 215 patients (43.0%).

## How to Train a DeepSurv Model

## How to Produce a Counterfactual Explanation with `survex`

## References

Katzman, J. L., Shaham, U., Cloninger, A., Bates, J., Jiang, T., &
Kluger, Y. (2018).  
*DeepSurv: Personalized Treatment Recommender System Using a Cox
Proportional Hazards Deep Neural Network*.  
Proceedings of the Machine Learning for Healthcare Conference,
312–325.  
<https://github.com/jaredleekatzman/DeepSurv> 

## Descriptions of Datasets Available in the Package

- **German Breast Cancer Study Group (GBSG2):** 686 patients and 8
  features (Age, Menopausal status, tumor localization, tumor size,
  estrogen and progesterone receptor status, tumor grading, histologic
  tumor type, and number of involved lymph nodes); The endpoint is
  recurrence free survival, which occurred for 299 patients (43.6%).

- **WHAS500 (Worcester Heart Attack Study):** 500 patients and 14
  features (AFIB indicator, Age in years at hospital admission, Complete
  heart block indicator, BMI, Congestive heart complications indicator,
  History of cardiovascular disease indicator, Initial diastolic blood
  pressure, Initial systolic blood pressure, gender, Initial heart rate,
  Length of hospital stay, Acute myocardial infarction (MI) order, MI
  type, Cardiogenic shock indicator); The endpoint is death, which
  occurred for 215 patients (43.0%).

## How to Train a DeepSurv Model

## How to Produce a Counterfactual Explanation with `survex`

## References

Katzman, J. L., Shaham, U., Cloninger, A., Bates, J., Jiang, T., &
Kluger, Y. (2018).  
*DeepSurv: Personalized Treatment Recommender System Using a Cox
Proportional Hazards Deep Neural Network*.  
Proceedings of the Machine Learning for Healthcare Conference,
312–325.  
<https://github.com/jaredleekatzman/DeepSurv>

Spytek M, Krzyziński M, Langbein SH, Baniecki H, Wright MN, Biecek P.
survex: an R package for explaining machine learning survival models.
Bioinformatics. 2023 Dec 1;39(12):btad723. doi:
10.1093/bioinformatics/btad723. PMID: 38039146; PMCID: PMC11025379.
<https://github.com/ModelOriented/survex/>
