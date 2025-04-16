## Data

As this repository is public and I’m not allowed to publicly distribute the data, to replicate the results please go to [ICPSR Study 38925](https://www.icpsr.umich.edu/web/NACJD/studies/38925) to download the raw data in delimiter format (data is available for Brown affiliates).

I use the file `38925-0003-Data.tsv`. Run the `data_manipulations.R` script to generate `full_data.csv`, which is identical to the cleaned merged dataset `data.csv` included in the `data` folder. For variable identification, please refer to the comments in the `data_manipulations.R` file.

## Main.R

Using `data.csv`, `Main.R` produces **4 figures** and **2 tables**, which are saved in the `Figures` and `Tables` folders, respectively.
