# README

ShinyApp is provided in support of the publication:
2020 Silbern et al

### Before start
uncompress "Lineplots_BTX.zip" and "Lineplots_CaEGTA.zip" folders

### Content
"\Lineplots_BTX" and "\Lineplots_CaEGTA" contains plots (svg) of log2 fold changes of phosphorylation sites per protein in Mock/BTX and Ca/EGTA experiments, respectively. File names are Uniprot protein accessions.  
\*.Rdata data required by the App  
"app.R" app R file

### Execution
Start command line, set working directory to the location of app.R  
Run Rscript app.R.  
(provide correct location of Rscript.exe, e.g. R\R-[version]\bin\x64\Rscript.exe)  
Open internet browser and navigate to the provided url (e.g. http://127.0.0.1:[port number])  
When done, close command line window.

### Required packages

- data.table (v. 1.12.8),
- shiny (v. 1.4.0),
- ggplot2 (v. 3.2.1),

### Session Info
```
R version 3.6.2 (2019-12-12)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17763)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] data.table_1.12.8 ggplot2_3.2.1     shiny_1.4.0      

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.3       rstudioapi_0.10  magrittr_1.5     tidyselect_0.2.5 munsell_0.5.0    colorspace_1.4-1
 [7] xtable_1.8-4     R6_2.4.1         rlang_0.4.2      fastmap_1.0.1    dplyr_0.8.3      tools_3.6.2     
[13] grid_3.6.2       gtable_0.3.0     withr_2.1.2      htmltools_0.4.0  assertthat_0.2.1 lazyeval_0.2.2  
[19] digest_0.6.23    tibble_2.1.3     lifecycle_0.1.0  crayon_1.3.4     purrr_0.3.3      later_1.0.0     
[25] promises_1.1.0   glue_1.3.1       mime_0.7         compiler_3.6.2   pillar_1.4.2     scales_1.1.0    
[31] httpuv_1.5.2     pkgconfig_2.0.3 