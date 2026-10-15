# Shopping_Local_Trusted_Traders
This repository contains the code and data for the analyses performed in the article ‘Shopping Local and Trusted Traders. Identifying Production Zones and Trade Routes of Northern Italian Terra Sigillata Using Similarity Indices’. 


## Data
The data stored here forms the basis of the analyses undertaken in the article. The primary source of the data was the Römisch-Germanisches Zentralmuseum (RGZM) database ‘Names on Terra Sigillata’. The original database can be found [here]. (https://www1.rgzm.de/samian/home/frames.htm) The dataset was supplemented by a corpus of sigillata stamps gathered from excavation reports not included in the RGZM database, which enhanced existing assemblages present in the database (such as Cremona and Turin).

The database contains 1844 Northern Italian terra sigillata stamps from 26 sites across Northern Italy. It is stored here in csv. format. The file name is northernstamps.csv

### Structure

The CSV file contains nine columns containing the following information: 

**id:** Unique identifier for each stamp in the dataset.

**rgzm_id:** A stamp's ID within the RGZM dataset (if applicable).

**pottername:** Reconstructed name of the potter attested by the stamp.

**production:** Specific location of the sherd's production (if known).

**production_area:** Sub-produciton of terra sigillata (e.g. Arretine, Northern Italic, South Gallic etc.)

**date:** Chronology for the activity of the potter (if known).

**lower_date** Starting date for activity of the potter (if known).

**ending_date** Ending date for activity of the potter (if known).

**die:** Unique die number for the stamp from Oxé, A., Comfort, H. and Kenrick, P.M. 2000. *Corpus vasorum Arretinorum: a catalogue of the signatures, shapes and chronology of Italian sigillata*. Bonn: Habelt.

**site:** Location where the stamp was found.

**bibref:** The publication the stamp is recored in (if not included in the RGZM database).

## Notebooks
The R notebooks used to generate the data analysis figures are stored in this repository.

Each notebook reproduces one figures, numbered in the same way as in the publication.

Each notebook is structured in the same way:

Import packages

Read in data

Data analysis

Plot the graphs
