# Maximum likelihood estimation of transition probabilities

Estimates transition probabilities by maximum likelihood from an
empirical count matrix returned by
[`countEmp`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/countEmp.md)
or
[`countEmpBivariate`](https://boellenruecherm.github.io/dyadicMarkov-public/reference/countEmpBivariate.md).

## Usage

``` r
mleEstimation(empirical)
```

## Arguments

- empirical:

  An empirical count matrix.

## Value

A numeric matrix with class `c("dyadic_mle", "matrix", "array")`
containing estimated transition probabilities with the same dimensions
as `empirical`. It remains usable as an ordinary matrix.

## Details

Each row of `empirical` is normalized independently. Rows with zero
total count are assigned a uniform probability vector, so each row of
the returned matrix sums to one.

## Examples

``` r
chainFM <- c(1L, 2L, 1L, 2L, 2L, 1L)
chainSM <- c(2L, 1L, 2L, 1L, 1L, 2L)
emp <- countEmp(chainFM, chainSM, states = 2L)
mleEstimation(emp)
#>                   next_1 next_2
#> focal_1_partner_1 0.500  0.500 
#> focal_1_partner_2 0.000  1.000 
#> focal_2_partner_1 0.667  0.333 
#> focal_2_partner_2 0.500  0.500 
```
