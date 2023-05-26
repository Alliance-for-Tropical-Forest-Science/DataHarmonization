
## Downloading the output data

Clicking on **save all** generates a zip file with all the files you should need.


### Not a zip file? Here is some troubleshooting pointers.

If you are using the online version of the app, it could be that the server is reaching its file size limit. We recommend you run the app within R.

If you are running the app within R and  the file that is offered to you is called dbZIP, it means you need to install Rtools and add it to the PATH.

To figure out if you already have Rtools, run this in R:

```r
pkgbuild::rtools_path()
```

If this returns a character string, you already have Rtools installed and the character string is the path to where Rtools is located on your machine (e.g. mine is "C:\\rtools40/usr/bin"). 

If this returns nothing, then you need to install Rtools and this function should do it: https://www.rdocumentation.org/packages/installr/versions/0.23.4/topics/install.Rtools

Once you know where Rtools is on your machine, you need to add its location to the PATH. You can do so by running the following code in R:

``` r
Sys.setenv(PATH = paste("C:\\rtools40/usr/bin", Sys.getenv("PATH"), sep=";"))
````
Remember to replace "C:\\rtools40/usr/bin" by what is appropriate for you.


