---
title: "zoocat"
output: md_document
#output: html_document
variant: markdown_github
---



**zoocat** package is an extension package of the **zoo** package. The aim of this package is:

* Manipulate multivariate time serie conveniently.
* Convert between time series data and data frame.
* Represent multi-dimensional data by two-dimensional format, so we can focus on one dimension.

For using **zoocat** package, first load it:


```r
library(zoocat)
```

# Construct a **zoocat** object

Although column names can be used to identify columns, for "`zoocat`" class, a data frame is used to store information of columns, we call it column attribute table (**cattr** table).
In the **cattr** table, each row represents a column of the core data in the object, and each column represents a column attribute. 
Using following codes to construct a **zoocat** object based on a matrix.


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
#> 2011 -1.86 -1.16 -0.44  0.09
#> 2012  3.10 -1.61 -2.39  0.16
#> 2013 -0.84  0.22 -0.36  0.37
#> 2014  0.21  0.19 -0.35 -0.74
```

It can be seen that there are two column attributes: "treatment" and "site", and the index means "year".
So, each data value corresponds to three underlying variables, i.e., treatment, site and year.

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
#> 2011     -1.86     -1.16     -0.44      0.09
#> 2012      3.10     -1.61     -2.39      0.16
#> 2013     -0.84      0.22     -0.36      0.37
#> 2014      0.21      0.19     -0.35     -0.74
```


# Merge by columns

Merging by columns is similar with "`zoo`" object except that the **cattr** tables need to be merged by rows.
Note that NA is added if it is needed.


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
#> 2011   -1.86   -1.16   -0.44    0.09       8.14       8.84       9.56
#> 2012    3.10   -1.61   -2.39    0.16      13.10       8.39       7.61
#> 2013   -0.84    0.22   -0.36    0.37       9.16      10.22       9.64
#> 2014    0.21    0.19   -0.35   -0.74      10.21      10.19       9.65
#>      NA_s3_TRUE
#> 2011      10.09
#> 2012      10.16
#> 2013      10.37
#> 2014       9.26
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
#> 1 2011         a   s1 -1.86
#> 2 2011         b   s1 -1.16
#> 3 2011         a   s2 -0.44
#> 4 2011         b   s2  0.09
#> 5 2012         a   s1  3.10
#> 6 2012         b   s1 -1.61
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
#> 2011 -1.86 -0.44 -1.16  0.09
#> 2012  3.10 -2.39 -1.61  0.16
#> 2013 -0.84 -0.36  0.22  0.37
#> 2014  0.21 -0.35  0.19 -0.74
```

Note that casting from a data frame is a convenient method to build a "`zoocat`" object.


# Reset the index variable

The index variable can be reset to be a variable in the **cattr** table.


```r
reset_index_var(zc, index.var = 'treatment')
```

```
#> A zoocat object with:
#> - [column attribute fields]: year, site
#> - [index variable]: treatment
#> - [data]:
#>   2011_s1 2011_s2 2012_s1 2012_s2 2013_s1 2013_s2 2014_s1 2014_s2
#> a   -1.86   -0.44    3.10   -2.39   -0.84   -0.36    0.21   -0.35
#> b   -1.16    0.09   -1.61    0.16    0.22    0.37    0.19   -0.74
```

# apply series functions

Functions can be applied for each column of the "`zoocat`" object, and the results will be binded with the **cattr** table.


```r
apply_col(zc, FUN = mean)
```

```
#>   treatment site  output
#> 1         a   s1  0.1525
#> 2         b   s1 -0.5900
#> 3         a   s2 -0.8850
#> 4         b   s2 -0.0300
```

```r
apply_col(zc, FUN = function (x) {c(mean = mean(x), sd = sd(x))})
```

```
#>   treatment site    mean        sd
#> 1         a   s1  0.1525 2.1390243
#> 2         b   s1 -0.5900 0.9362692
#> 3         a   s2 -0.8850 1.0041414
#> 4         b   s2 -0.0300 0.4880574
```

Another method is to apply a function for the whole core data, and the results can be binded with the **cattr** table or index.


```r
apply_core(zc, FUN = colMeans, bind = 'cattr')
```

```
#>   treatment site data.ret
#> 1         a   s1   0.1525
#> 2         b   s1  -0.5900
#> 3         a   s2  -0.8850
#> 4         b   s2  -0.0300
```

```r
apply_core(zc, FUN = rowMeans, bind = 'index')
```

```
#>    2011    2012    2013    2014 
#> -0.8425 -0.1850 -0.1525 -0.1725
```

```r
apply_core(zc, FUN = function (x) {t(x*2)}, bind = c('cattr', 'index'))
```

```
#> A zoocat object with:
#> - [column attribute fields]: treatment, site
#> - [index variable]: index
#> - [data]:
#>       a_s1  b_s1  a_s2  b_s2
#> 2011 -3.72 -2.32 -0.88  0.18
#> 2012  6.20 -3.22 -4.78  0.32
#> 2013 -1.68  0.44 -0.72  0.74
#> 2014  0.42  0.38 -0.70 -1.48
```


# Filter columns

Function `filter_col` can be used to extract columns by specified conditions on the **cattr** table:


```r
filter_col(zc, cond = treatment == 'a' & site == 's2')
```

```
#> A zoocat object with:
#> - [column attribute fields]: treatment, site
#> - [index variable]: year
#> - [data]:
#>       a_s2
#> 2011 -0.44
#> 2012 -2.39
#> 2013 -0.36
#> 2014 -0.35
```



# Others

For "`zoocat`" class, all methods for the "`zoo`" class can be used.
Addtionally, class "`zoomly`", based on "`zoocat`", is designed to manipulate monthly data.
 
