Model and forecast financial intraday signals, including trading volume and volatility, using state-space models. Currently, the univaraite state-space model for intraday trading volume is available.

## Installation
The package can be installed from [GitHub](https://github.com/convexfi/intradayModeling):
```{r, eval=FALSE}
# install development version from GitHub
devtools::install_github("convexfi/intradayModeling")
```

To get help:
```{r, eval=FALSE}
library(intradayModel)
?fit_mvt
```

To cite `intradayModel` in publications:
```{r, eval=FALSE}
citation("intradayModeling")
```


## Quick Start
To get started, we load the package and some sample data: the 15-minute intraday trading volume of AAPL from 2019-01-02 to 2019-06-28, covering 124 trading days.

```{r, message = FALSE}
devtools::load_all()
data(AAPL_volume)
AAPL_volume[1:5, 1:5]
```

Next, we define a univariate state-space model using the `uniModelSpec` function.

```{r}
model <- uniModelSpec(fit = TRUE)
```

Then, we use the first 104 trading days to fit the model and the last 20 days to evaluate its forecasting performance. Fitting can be achieved by the `uniModelFit` function.

```{r}
data <- AAPL_volume
data_train <- AAPL_volume[, 1:104]
model_fitted <- uniModelFit(data_train, model, acceleration = TRUE)
```

Once the model is fitted, we use the `uniModelFilter` function to decompose intraday trading signal into daily, seasonal, and intraday dynamic components. This helps us better understand the composition of the intraday signal.

```{r}
filter_result <- uniModelFilter(data_train, model_fitted)
filter_result$plot
```

To see how well our model performs on new data, we use the `uniModelPred` function to do one-step-ahead prediction on the last 20 trading days of the dataset. This function also helps to evaluate the accuracy of the forecast.

```{r}
predict_result <- uniModelPred(data, model_fitted, out.sample = 20)
predict_result$measure
predict_result$plot
```


## Links
Package: [GitHub](https://github.com/convexfi/intradayModeling)

README file: [GitHub-readme](https://github.com/convexfi/intradayModeling/blob/master/README.md).

Vignette: [GitHub-vignette](https://htmlpreview.github.io/?https://github.com/convexfi/intradayModeling/blob/master/vignettes/intradayModel.html).
