## <span style="color:red">**Disclaimer. This model does not represent an agency policy position nor management decision, has not been endorsed any agency, and should not be interpreted as such.** </span>

## This is a reposotory for the development of a simulation model of harvest during the Columbia River summer Chinook managment period.

The `data-raw` folder contains:

- The `Model description.Rmd` file documents the data and process used to develop the simulation model. It also generates the `Internal_data.rda` file, which lives in the `r` folder and is described below.
- `app.r` defines a shiny app for comparing the outcomes of alternative harvest control rules.
- `Modeling.Rmd` is a bit of "scrap paper". I used it for data exploration and analysis during the process of model development and and keeping it here in case I need to go back to anything.


The `r` folder contains functions for the simulation model.   

- The main function for simulating is `pop_sim()`
- Other functions are called by `pop_sim()` or are for summarizing and plotting outputs
- The `HCR_feedback.r` file defines and shiny server and UI function for a module where different harvest control rules can be defined, simulations run, and harvest and escapement trajectories plotted.
- `Internal_data.rda`contians data used in population simulations (e.g., parameters, random errors, etc.) and by the shiny app (e.g., simulation results).


Questions?
Contact Mark Sorel
mark.sorel@dfw.wa.gov
