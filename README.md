# Temporally-explicit model of carbon balance of the wood sector in tropical forests at the scale of a territory 

This repository presents a temporally-explicit and territory scale model of carbon balance of the wood sector in a tropical territory. 
It allows modeling past carbon fluxes on the basis of logging data (area logged, logging intensity) and simulating the carbon balance of prospective scenarii of development of the timber production sector.
It was calibrated with long-term local data using Bayesian inference. 
The model accounts for carbon fluxes from selective logging in natural forest, timber plantation, first transformation and avoided emissions through energy substitution. 
All input data and parameters apply to the case of French Guiana, but the model can easily be adapted to other contexts.

The functionning of the model and all the functions is presented [here](03_Rmarkdown/Full_C_model/Full_C_model.pdf).

The folder [01_data](01_data) contains all model input data.
The folder [02_functions](02_functions) contains all the functions.
The folder [03_Rmarkdown](03_Rmarkdown) countains the description of the model.
Output of the model are stored in the folder [04_Results](04_Results).

Model and code developed by: [Géraldine Derroire](https://github.com/GeraldineDerroire), [Camile Piponiot](https://github.com/cpiponiot), [Bruno Hérault](https://github.com/BrunoHerault), with expert knowledge from Laurent Descroix
