# rRMIS
Package to get data from RMIS cwt system.

# Install 
```{r}

devtools::install_github("Ben-Cox/rRMIS")

```

# Examples
```{r}
library(rRMIS)

d <- get_release_data(first_by=2016, last_by=2016)

d
```

