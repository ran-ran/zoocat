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
#> 2011  0.15  0.40 -0.45 -0.84
#> 2012  0.07 -1.65  0.03 -0.74
#> 2013 -0.16 -0.27  0.16  1.45
#> 2014 -1.10 -0.08 -0.75 -1.48
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
#> 2011      0.15      0.40     -0.45     -0.84
#> 2012      0.07     -1.65      0.03     -0.74
#> 2013     -0.16     -0.27      0.16      1.45
#> 2014     -1.10     -0.08     -0.75     -1.48
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
#> 2011    0.15    0.40   -0.45   -0.84      10.15      10.40       9.55
#> 2012    0.07   -1.65    0.03   -0.74      10.07       8.35      10.03
#> 2013   -0.16   -0.27    0.16    1.45       9.84       9.73      10.16
#> 2014   -1.10   -0.08   -0.75   -1.48       8.90       9.92       9.25
#>      NA_s3_TRUE
#> 2011       9.16
#> 2012       9.26
#> 2013      11.45
#> 2014       8.52
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
#> 1 2011         a   s1  0.15
#> 2 2011         b   s1  0.40
#> 3 2011         a   s2 -0.45
#> 4 2011         b   s2 -0.84
#> 5 2012         a   s1  0.07
#> 6 2012         b   s1 -1.65
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
#> 2011  0.15 -0.45  0.40 -0.84
#> 2012  0.07  0.03 -1.65 -0.74
#> 2013 -0.16  0.16 -0.27  1.45
#> 2014 -1.10 -0.75 -0.08 -1.48
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
#> a    0.15   -0.45    0.07    0.03   -0.16    0.16   -1.10   -0.75
#> b    0.40   -0.84   -1.65   -0.74   -0.27    1.45   -0.08   -1.48
```

# apply series functions

Functions can be applied for each column of the "`zoocat`" object, and the results will be binded with the **cattr** table.


```r
apply_col(zc, FUN = mean)
```

```
#>   treatment site  output
#> 1         a   s1 -0.2600
#> 2         b   s1 -0.4000
#> 3         a   s2 -0.2525
#> 4         b   s2 -0.4025
```

```r
apply_col(zc, FUN = function (x) {c(mean = mean(x), sd = sd(x))})
```

```
#>   treatment site    mean        sd
#> 1         a   s1 -0.2600 0.5752101
#> 2         b   s1 -0.4000 0.8797348
#> 3         a   s2 -0.2525 0.4228771
#> 4         b   s2 -0.4025 1.2777682
```

Another method is to apply a function for the whole core data, and the results can be binded with the **cattr** table or index.


```r
apply_core(zc, FUN = colMeans, bind = 'cattr')
```

```
#>   treatment site data.ret
#> 1         a   s1  -0.2600
#> 2         b   s1  -0.4000
#> 3         a   s2  -0.2525
#> 4         b   s2  -0.4025
```

```r
apply_core(zc, FUN = rowMeans, bind = 'index')
```

```
#>    2011    2012    2013    2014 
#> -0.1850 -0.5725  0.2950 -0.8525
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
#> 2011  0.30  0.80 -0.90 -1.68
#> 2012  0.14 -3.30  0.06 -1.48
#> 2013 -0.32 -0.54  0.32  2.90
#> 2014 -2.20 -0.16 -1.50 -2.96
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
#> 2011 -0.45
#> 2012  0.03
#> 2013  0.16
#> 2014 -0.75
```



# Others

For "`zoocat`" class, all methods for the "`zoo`" class can be used.
Addtionally, class "`zoomly`", based on "`zoocat`", is designed to manipulate monthly data.
 
