<!-- README.md is generated from README.Rmd. Please edit that file -->

# intradayModel

Our package uses state-of-the-art state-space models to facilitate the
modeling and forecasting of financial intraday signals. This package
currently offers a univariate model for intraday trading volume, with
new features on intraday volatility and multivariate models in
development. It is a valuable tool for anyone interested in exploring
intraday, algorithmic, and high-frequency trading.

## Installation

The package can be installed from
[GitHub](https://github.com/convexfi/intradayModeling):

``` r
# install development version from GitHub
devtools::install_github("convexfi/intradayModeling")
```

## Quick Start

To get started, we load the package and some sample data: the 15-minute
intraday trading volume of AAPL from 2019-01-02 to 2019-06-28, covering
124 trading days.

``` r
devtools::load_all()
data(AAPL_volume)
AAPL_volume[1:5, 1:5]
#>          2019-01-02 2019-01-03 2019-01-04 2019-01-07 2019-01-08
#> 09:30 AM   10142172    3434769   20852127   15463747   14719388
#> 09:45 AM    5691840   19751251   13374784    9962816    9515796
#> 10:00 AM    6240374   14743180   11478596    7453044    6145623
#> 10:15 AM    5273488   14841012   16024512    7270399    6031988
#> 10:30 AM    4587159   18041115    8686059    7130980    5479852
```

Next, we define a univariate state-space model using the `uniModelSpec`
function.

``` r
model <- uniModelSpec(fit = TRUE)
```

Then, we use the first 104 trading days to fit the model and the last 20
days to evaluate its forecasting performance. Fitting can be achieved by
the `uniModelFit` function.

``` r
data <- AAPL_volume
data_train <- AAPL_volume[, 1:104]
model_fitted <- uniModelFit(data_train, model, acceleration = TRUE)
#> iter:5 diff:0.002868503
#> iter:10 diff:0.001158477
#> iter:15 diff:0.001227848
#> iter:20 diff:0.0007639725
#> iter:25 diff:0.0005357166
#> iter:30 diff:0.0002927454
#> iter:35 diff:0.0005428917
#> iter:40 diff:0.0001707282
#> iter:45 diff:0.0002356185
#> Success! abstol test passed at 47 iterations.
```

Once the model is fitted, we use the `uniModelFilter` function to
decompose intraday trading signal into daily, seasonal, and intraday
dynamic components. This helps us better understand the composition of
the intraday signal.

``` r
filter_result <- uniModelFilter(data_train, model_fitted)
filter_result$plot
```

<img src="man/figures/README-quick-start-filter-1.png" width="75%" style="display: block; margin: auto;" />

To see how well our model performs on new data, we use the
`uniModelPred` function to do one-step-ahead prediction on the last 20
trading days of the dataset. This function also helps to evaluate the
accuracy of the forecast.

``` r
predict_result <- uniModelPred(data, model_fitted, out.sample = 20)
predict_result$measure
#>        mae      mape    rmse
#> 1 630877.9 0.2083228 1418145
predict_result$plot
```

<img src="man/figures/README-quick-start-pred-1.png" width="75%" style="display: block; margin: auto;" />

## Contributing

We welcome all sorts of contributions. Please feel free to open an issue
to report a bug or discuss a feature request.

## Citation

If you made use of this software please consider citing:

-   Chen, R., Feng, Y., and Palomar, D. (2016). Forecasting intraday
    trading volume: A kalman filter approach.
    <https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3101695>

## Links

Package: [GitHub](https://github.com/convexfi/intradayModeling)

README file:
[GitHub-readme](https://github.com/convexfi/intradayModeling/blob/master/README.md).

Vignette:
[GitHub-vignette](https://htmlpreview.github.io/?https://github.com/convexfi/intradayModeling/blob/master/vignettes/intradayModel.html).
