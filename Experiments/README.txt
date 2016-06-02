The "GENERAL" scripts are designed to simulate any experiment where the input data file is a CSV file of a particular format, exemplified by "Sample.csv" which is included here in the "GENERAL" folder.

The required data columns are:  Type (either "Target" or "Filler"), GroupingOrder (a number N means "Nth instance" of the associated word), Condition (it is highly experiment-dependent what this means), SoundFile (which word was uttered; the "data_prep" script as-is can only handle single-word utterances, though extensions to handle multi-word utterances shouldn't be difficult), and "ItemN", e.g. Item1, Item2, Item3, etc. up to N possible items per instance, where an "item" is a visible meaning associated with that instance.

VERY IMPORTANT:  The data file must be arranged such that "Item1" corresponds to the correct intended meaning for every target word!

OPTIONAL:  A "Subject" column may be included if multiple subjects are represented in the experiment design, i.e. if there are different lists.  But keep in mind, there must always be a single correct meaning for each word, e.g. "heek" must always mean the same thing across lists, or else the script will break.


****


TO GENERATE DATA FROM A NUMBER OF SIMULATIONS:

1)  Set your R working directory to the folder that contains all the experimental scripts; this can be done in the R terminal with “setwd()”, e.g.:

setwd("/Users/johndoe/Documents/Published Code/Experiments")

Of course you need to specify your own file path depending on where you have saved this folder.


2)  First load the "data_prep" R script:

source("data_prep.r")


3)  Then, generate your data files to be used by the model, using the "generate_data" function and the file path for your input CSV:

generate_data("Sample.csv")


4)  After the data is generated, load the model you want to run by loading the appropriate R script, e.g.:

source("PURSUIT.r")


5)  The model scripts specify a function “runsims”.  FOR THE PROPOSE/VERIFY, this takes two arguments:  (i) the number of simulations to run, and (ii) a filename for the resulting CSV file (do not include extension when entering this argument)  FOR ALL OTHER MODELS, this takes three arguments:  (i) number of simulations, (ii) a "memory" parameter specifying how likely the "subject" is to remember her hypothesis during the experiment, and (iii) the filename.

The "memory" parameter is omitted from the PV simulations because memory retrieval is explicitly built in to the structure of that model.

For example, to run 100 iterations of an experiment with memory=0.75 and store the results in a spreadsheet named “myfile.csv”, just load the data and the model script and then run the following command in the R terminal:

runsims(100,memory=0.75,"myfile")

This will write the result file straight to the working directory.