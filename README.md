# mortality-5to19

## Developer instructions

-   Clone repository to computer
-   Open `/src/prepare-session/set-inputs`
    -   Set path to CA CODE data warehouse folder
    -   Set age-sex group for estimation
    -   Set years of estimation
-   Run make file
-   View results locally in `/gen/results/output` and `/gen/visualizations/output`

## Directory structure

This project framework was conceptualized using resources from the [Tilburg Science Hub](https://tilburgsciencehub.com/), in accordance with recommended workflow and data management principles for research projects.

### Source code

Source code is made available in the `src` folder, with sub-folders for each stage of the project pipeline. Source code contains all code that is required to execute the project's pipeline. There is a `make.R` file in the main directory folder which sources code in correct order. 

Our pipeline consists of seven main stages:

-   `prepare-session`
-   `data-management`
-   `estimation`
-   `prediction`
-   `squeezing`
-   `uncertainty`
-   `results`

There are additional folders in `/src` which contain code not referenced in the `make.R` file. These folders are:

-   `aggregation` : Age/sex aggregation of estimates. Can only be run after results are generated for all age/sex groups.
-   `visualizations` : Code used to generate visualizations after producing results.

### Generated files

Generated files are all files that are created by running the source code (`/src`) on the raw data (`/data`). They are stored in the `gen` folder. The `/gen` subdirectories match the pipeline stages.

Each subdirectory in `gen` contains the following subdirectories:

-   `input`: any required input files to run this step of the pipeline
-   `temp`: temporary files, such as an Excel dataset that needs to be converted into a CSV
-   `output`: stores the final result of the pipeline stage
-   `audit`: quality checks, diagnostic information on the performance of each step in the pipeline.

