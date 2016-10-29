---
title: "zoocat"
output: md_document
variant: markdown_github
---



**zoocat** package is an extension package of the **zoo** package. The aim of this package is:

* Manipulate multivariate time serie conveniently.
* Convert between **zoocat** object and data frame.
* Represent multi-dimensional data by two-dimensional format.

For using **zoocat** package, first load it:


```r
library(zoocat)
```

# Construct a **zoocat** object

Using following codes to construct a **zoocat** object.


```r
mat <- matrix(round(rnorm(24), 2), ncol = 4)
ctable <- data.frame(treatment = factor(rep(c('a', 'b'), 2), levels = c('a', 'b')), 
                     site = factor(rep(c('s1', 's2'), each = 2), levels = c('s1', 's2')))
zc <- zoocat(mat, order.by = 2011 : 2014, colattr = ctable, index.name = 'year')
print(zc)
```

```
#> A zoocat object with:
#> - [column attribute fields]: treatment, site
#> - [index variable]: year
#> - [data]:
#>       a_s1  b_s1  a_s2  b_s2
#> 2011 -1.42 -0.12 -0.59 -0.17
#> 2012  0.18 -0.92 -1.27  1.70
#> 2013 -0.53  2.29  0.23 -1.19
#> 2014  1.50 -2.36 -0.94 -0.09
```

Note that all information of each column of the **zoocat** object is stored in a data frame, which is as the argument "colattr" for function "zoocat". This data frame is called column attribute table (**cattr** table).

# Set and get the **cattr** table


```r
cattr(zc)
```

```
#>   treatment site
#> 1         a   s1
#> 2         b   s1
#> 3         a   s2
#> 4         b   s2
```

```r
zc2 <- zc
cattr(zc2) <- data.frame(cattr(zc), observer = 'Jack')
print(zc2)
```

```
#> A zoocat object with:
#> - [column attribute fields]: treatment, site, observer
#> - [index variable]: year
#> - [data]:
#>      a_s1_Jack b_s1_Jack a_s2_Jack b_s2_Jack
#> 2011     -1.42     -0.12     -0.59     -0.17
#> 2012      0.18     -0.92     -1.27      1.70
#> 2013     -0.53      2.29      0.23     -1.19
#> 2014      1.50     -2.36     -0.94     -0.09
```


# Merge by columns


```r
zc2 <- zc + 10
cattr(zc2) <- data.frame(site = 's3', added = rep(TRUE, 4))
zc.merge <- cbind(zc, zc2)
print(zc.merge)
```

```
#> A zoocat object with:
#> - [column attribute fields]: treatment, site, added
#> - [index variable]: year
#> - [data]:
#>      a_s1_NA b_s1_NA a_s2_NA b_s2_NA NA_s3_TRUE NA_s3_TRUE NA_s3_TRUE
#> 2011   -1.42   -0.12   -0.59   -0.17       8.58       9.88       9.41
#> 2012    0.18   -0.92   -1.27    1.70      10.18       9.08       8.73
#> 2013   -0.53    2.29    0.23   -1.19       9.47      12.29      10.23
#> 2014    1.50   -2.36   -0.94   -0.09      11.50       7.64       9.06
#>      NA_s3_TRUE
#> 2011       9.83
#> 2012      11.70
#> 2013       8.81
#> 2014       9.91
```

```r
cattr(zc.merge)
```

```
#>   treatment site added
#> 1         a   s1    NA
#> 2         b   s1    NA
#> 3         a   s2    NA
#> 4         b   s2    NA
#> 5      <NA>   s3  TRUE
#> 6      <NA>   s3  TRUE
#> 7      <NA>   s3  TRUE
#> 8      <NA>   s3  TRUE
```


# Melt and cast

Similar with package **reshape2**, you can melt and cast between data frame and **zoocat**.


```r
df.melt <- melt(zc)
head(df.melt)
```

```
#>   year treatment site value
#> 1 2011         a   s1 -1.42
#> 2 2011         b   s1 -0.12
#> 3 2011         a   s2 -0.59
#> 4 2011         b   s2 -0.17
#> 5 2012         a   s1  0.18
#> 6 2012         b   s1 -0.92
```


```r
cast2zoocat(df.melt, index.var = 'year', value.var = 'value')
```

```
#> A zoocat object with:
#> - [column attribute fields]: treatment, site
#> - [index variable]: year
#> - [data]:
#>       a_s1  a_s2  b_s1  b_s2
#> 2011 -1.42 -0.59 -0.12 -0.17
#> 2012  0.18 -1.27 -0.92  1.70
#> 2013 -0.53  0.23  2.29 -1.19
#> 2014  1.50 -0.94 -2.36 -0.09
```


# Reset the index variable


```r
reset_index_var(zc, index.var = 'treatment')
```

```
#> A zoocat object with:
#> - [column attribute fields]: year, site
#> - [index variable]: treatment
#> - [data]:
#>   2011_s1 2011_s2 2012_s1 2012_s2 2013_s1 2013_s2 2014_s1 2014_s2
#> a   -1.42   -0.59    0.18   -1.27   -0.53    0.23    1.50   -0.94
#> b   -0.12   -0.17   -0.92    1.70    2.29   -1.19   -2.36   -0.09
```

