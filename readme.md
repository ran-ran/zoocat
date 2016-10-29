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
#>      a_s1  b_s1  a_s2  b_s2
#> 2011 0.10 -1.02 -0.46 -1.36
#> 2012 0.52  0.32  0.61 -0.03
#> 2013 0.12 -0.60 -0.23 -1.12
#> 2014 0.45  0.71 -1.84  1.17
```

Note that all information of each column of the **zoocat** object is stored in a data frame, which is as the argument "colattr" for function "zoocat". 
This data frame is called column attribute table (**cattr** table).

# Set and get the **cattr** table

Using following codes to get and set **cattr** table.


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
#> 2011      0.10     -1.02     -0.46     -1.36
#> 2012      0.52      0.32      0.61     -0.03
#> 2013      0.12     -0.60     -0.23     -1.12
#> 2014      0.45      0.71     -1.84      1.17
```


# Merge by columns

Merging by columns is similar with "`zoo`" object.


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
#> 2011    0.10   -1.02   -0.46   -1.36      10.10       8.98       9.54
#> 2012    0.52    0.32    0.61   -0.03      10.52      10.32      10.61
#> 2013    0.12   -0.60   -0.23   -1.12      10.12       9.40       9.77
#> 2014    0.45    0.71   -1.84    1.17      10.45      10.71       8.16
#>      NA_s3_TRUE
#> 2011       8.64
#> 2012       9.97
#> 2013       8.88
#> 2014      11.17
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
#> 1 2011         a   s1  0.10
#> 2 2011         b   s1 -1.02
#> 3 2011         a   s2 -0.46
#> 4 2011         b   s2 -1.36
#> 5 2012         a   s1  0.52
#> 6 2012         b   s1  0.32
```


```r
cast2zoocat(df.melt, index.var = 'year', value.var = 'value')
```

```
#> A zoocat object with:
#> - [column attribute fields]: treatment, site
#> - [index variable]: year
#> - [data]:
#>      a_s1  a_s2  b_s1  b_s2
#> 2011 0.10 -0.46 -1.02 -1.36
#> 2012 0.52  0.61  0.32 -0.03
#> 2013 0.12 -0.23 -0.60 -1.12
#> 2014 0.45 -1.84  0.71  1.17
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
#> a    0.10   -0.46    0.52    0.61    0.12   -0.23    0.45   -1.84
#> b   -1.02   -1.36    0.32   -0.03   -0.60   -1.12    0.71    1.17
```

