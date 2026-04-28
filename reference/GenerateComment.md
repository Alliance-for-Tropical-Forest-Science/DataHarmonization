# GenerateComment

Helper function to concatenate comments, separated by ";"

## Usage

``` r
GenerateComment(x, comment)
```

## Arguments

- x:

  a character vector

- comment:

  The string to add in the column (character)

## Value

concatenation of x and comment, separated by a semicolon

## Examples

``` r
GenerateComment(letters, "A")
#>  [1] "a;A" "b;A" "c;A" "d;A" "e;A" "f;A" "g;A" "h;A" "i;A" "j;A" "k;A" "l;A"
#> [13] "m;A" "n;A" "o;A" "p;A" "q;A" "r;A" "s;A" "t;A" "u;A" "v;A" "w;A" "x;A"
#> [25] "y;A" "z;A"
```
