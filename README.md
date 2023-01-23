# driect_indirect_bednet_protection

This is the code to recreate the analysis in [Quantifying the direct and indirect protection provided by insecticide treated bed nets against malaria](https://www.medrxiv.org/content/10.1101/2022.01.21.22269650v1).

Files 01, 02 and 05-09 use the [Imperial College London deterministic malaria model](https://github.com/mrc-ide/deterministic-malaria-model). This can be installed following the instructions [here](https://github.com/mrc-ide/deterministic-malaria-model).

Files 03 and 04 download and analyses [Demographic Health Survey data](https://dhsprogram.com). Access to this data can be requested from the previous link.  We include code to download the data using the [rdhs](https://cran.r-project.org/web/packages/rdhs/index.html) R package once access has been granted. Installation and set up is needed for the rdhs package and instructions can be found [here](https://docs.ropensci.org/rdhs/).
