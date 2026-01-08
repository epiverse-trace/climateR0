# Estimate relative R0 from mean temperature

This function takes an input series of mean temperature values in °C and
returns estimated relative R0 values for a specified vector and
pathogen.

## Usage

``` r
temperature_r0(data, vector_pathogen)
```

## Arguments

- data:

  A numeric vector of mean temperature values in °C

- vector_pathogen:

  Character vector specifying the vector and pathogen of interest This
  can be one of the following: AeaeDENV; AeaeZIKV; AealDENV; AetaRVFV;
  AetaSINV; AetrEEEV; AngaPfal; CxanMVEx; CxanRRVx; CxpiSINV; CxpiWNVxl;
  CxtaWEEV; CxtaWNVx; CxunWNVx

## Value

Numeric vector of relative R0 values corresponding to input temperature
values

## References

Mordecai, E. A., Caldwell, J. M., Grossman, M. K., Lippi, C. A.,
Johnson, L. R., Neira, M., Rohr, J. R., Ryan, S. J., Savage, V.,
Shocket, M. S., Sippy, R., Stewart Ibarra, A. M., Thomas, M. B., Villena
O. (2019). Thermal biology of mosquito-borne disease. Ecol Lett. 22(10)
[doi:10.1111/ele.13335](https://doi.org/10.1111/ele.13335)

## Examples

``` r
# Return relative R0 for Aedeas aegypti and dengue virus for mean
# temperatures from 18-36°C
temperature_r0(c(18,20,22,24,26,28,30,32,34,36), "AeaeDENV")
#> Returning relative R0 for vector: aedes_aegypti and pathogen: dengue_virus
#>  [1] 0.0007878388 0.0400522079 0.1638852609 0.4015152843 0.7112617676
#>  [6] 0.9574281565 0.9645956733 0.6630981443 0.1923898126 0.0000000000
```
