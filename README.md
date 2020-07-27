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
