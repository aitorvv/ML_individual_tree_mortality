
# Comparing Machine Learning algorithms for assessing individual tree mortality: code and data

<!--
DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10216009.svg)](https://doi.org/10.5281/zenodo.10216009)


:bulb: Have a look at the original poster [here](http://dx.doi.org/10.13140/RG.2.2.27865.94564).

:bookmark: Poster DOI: http://dx.doi.org/10.13140/RG.2.2.27865.94564

:open_file_folder: Repository DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10216009.svg)](https://doi.org/10.5281/zenodo.10216009)
-->

---

## :sparkles: Highlights 

- 6 different Machine Learning algorithms were compared in predicting individual tree mortality. 
- Effects of dataset size, variable set, thinning, inventory length, and cross-validation were studied. 
- Random Forest reached a higher performance level in all the case studies proposed except on cross-validation. 
- Logistic binomial Regression seems to be a more robust algorithm regarding cross-validation. 

---

## :book: Abstract

Tree mortality is a crucial process in forest dynamics and a key component of forest growth models and simulators. Factors like competition, drought and pathogens are driving tree mortality, but the underlying mechanism is difficult to model. The current environmental changes are even complicating model approaches as they influence alter all the factors involving mortality. However, innovative classification algorithms can go deep into data to find patterns able to model or even explain their relationship. We use Logistic binomial Regression as the reference algorithm for predicting individual tree mortality, but different Machine Learning alternatives already applied to other forest modelling topics can be used for this purpose. Here, we compare the performance of five different Machine Learning algorithms with Logistic binomial Regression in individual tree mortality classification: Decision Trees, Random Forest, Naive Bayes, K-Nearest Neighbour, and Support Vector Machine. Different case studies involving dataset size, number of variables, thinning degrees and inventory record length were conducted to assess the performance of each algorithm. Additionally, a cross-validation among thinning degrees was performed. Our results demonstrate the potential of Machine Learning in assessing tree mortality, while results interpretability can reduce its utility.  

---

## :file_folder: Repository Contents

- :floppy_disk: **1_data**:
    
    - :sunny: climate data obtained from [WorldClim data](https://www.worldclim.org/data/index.html) (not provided here)
        
    - :deciduous_tree: tree and plot data (*test data from **DEN** experimental plots, original data under serious request*)
      
      - *0_raw* contains the original data (*DEN experiemtnal plots*) regarding coordinates, tree inventory, plot age and thinning grades
      - *tmp_DEN* are different *checkpoints* saved after the use of each R code file, in order to explore the evolution of the data used. It works with the test information until the analysis (section 5)
      - *1_original_df* contains the results of the original analysis (full dataset) with all the metrics tested in that study (but without tree and plot original data). It can be used from section 6 in advance

- :computer: **2_code**: detailed R scripts, purpose, inputs and outputs are summarized below. 

:warning: Code was prepared to be used with the full dataset; code from section 5 to the end was not adapted to be used with the test dataset, but from section 6 in advance it can be used with the checkpoints of the original dataset (*1_original_df*) :warning:

| Script Name     | Purpose               | Input                    | Output                   |
|-----------------|-----------------------|--------------------------|--------------------------|
| `0_data_curation.r` | Manage initial data: structure, IDs, input missing data | `1_raw/final/VF_daten.xlsx` `1_raw/final/Fi-Daten__age.xlsx` | `3_final/0_initial_df_clean/initial_df_clean.csv`
| `1.0_neighborhood_main.r` `1.1_neighborhood_functions.r` | Calculate variables needed for the analysis using a subplot of 0.33*h radii around each tree | `1_data/3_final/0_initial_df_clean/initial_df_clean.csv` | `1_data/3_final/1_neighborhood/*` `trees_r33.csv` `subplot_stats_r33.csv` `neighborhood_stats_r33.csv` |
| `2_climate_data.r` | Calculate climate variables by plot location. :warning: It requires to download several climate information from WorldClim previously, write to the authors for more details :warning:  | `1_raw/final/Koordinaten.xlsx` | `1_data/3_final/2_clima/df_complete_r33.csv` |
| `3_feature_visualization.r` | Code to make graphs and manually study variable relationships | `1_data/3_final/2_clima/df_complete_r33.csv` | None |
| `4.0_split_dataset.r` `4.1_split_variables.r` `4.2_functions_var_combis.r` | Code to split datasets (size and thinning) and variables to develop case studies, excluding first plot measurement when needed. :warning: It doesn't work as desired with the test dataset, as most of the information is missing :warning: | `1_data/3_final/2_clima/df_complete_r33.csv` | `1_data/3_final/4_datasets/*` |
| `5.0_run_analysis.r` `5.1_LR_analysis.r` `5.2_DT_analysis.r` `5.3_RF_analysis.r` `5.4_NB_analysis.r` `5.5_KNN_analysis.r` `5.6_SVM_analysis.r` | Code to run all the analysis (except ANN) on R for the different case studies | `1_data/3_final/4_datasets/*` | `1_data/3_final/5_analysis/**case_study**/*` `metrics.RData` `models.RData` |
| `6_HPC` | Code used to run all the simulations on iuFOR HPC, splitted by study case | `1_data/3_final/4_datasets/*` | `1_data/3_final/5_analysis/**case_study**/*` `metrics.RData` `models.RData` |
| `7_metrics_compilation.r` | Get metrics from R analysis, calculate ANN metrics, and create a checkpoint | `1_data/3_final/5_analysis/*` `**case_study**/metrics.RData` `ann/preds/**case_study**/*` `ann/timer/**case_study**/*` | `1_data/3_final/6_final_results/**case_study**/final_metrics.RData` |
| `8.0_performance_graphs.r` `8.1_functions_performance_graphs.r` `8.2_classifiers_comparison.r` `8.3_graph_functions.r` `8.4_application_thinning.r` `8.5_application_thinning_comparison.r` `8.6_time_and_performance_graphs.r` | Code to compare the metrics for each analysis and case study using different graphs. Graphs of the original paper used code *8.5* and *8.6* | `1_data/3_final/6_final_results/**case_study**/final_metrics.RData` | `2_scripts/4_figures/*` |
| `9.0_location_map.r` `9.1_neighbour_graphs.r` `9.2_mortality_graphs.r` `9.3_df_mortality_rates` `9.4_paper_tables.r`| Code used to perform graphs of location, neighborhood and get information to provide tables in the original paper. | `1_raw/final/Koordinaten.xlsx` `1_data/3_final/2_clima/df_complete_r33.csv` `1_data/3_final/1_neighborhood/trees_r33.csv` | `2_scripts/4_figures/*` |
<!--
| `5.7.1_ANN_datasets.r` `5.7.2_ANN_analysis_HPC.py` | Data preparation (train and test) to ANN for each study case and analysis using Python | `1_data/3_final/4_datasets/*` | `1_data/3_final/5_analysis/ann/*/**case_study**/` *timer, input/train-test, history, preds, **case_study**/ann_time_list* |
-->

- :bar_chart: **3_figures**: graphs and figures used in the article and additional information:
  
  - *final_figures* contains the figures used in the original paper
  - *tmp_figures* contains different graphs used to understand the results of each analysis and metric

- :books: **4_bibliography**: recompilation of all the references used in the article

---

## :thinking: How to Use

In order to reproduce the analysis, users must:

- :floppy_disk: **Data**: 
    - According to the original paper conditions (*Due to the sensitive nature of the data, raw data would remain available only under serious requests.*), data must be requested from the authors
    - WordClim data needed to develop simulations must be downloaded from its [original website](https://www.worldclim.org/data/index.html)

- :computer: **Prerequisites: installation and code**: *[R](https://cran.r-project.org/bin/windows/base/)* must be installed to run the code with the used libraries across each script (*[RStudio](https://posit.co/download/rstudio-desktop/)* was also used to develop the code). Some analyses (specifically when training RF models) will request high computation power, which can provoke out-of-memory in a normal computer. Access to high-computing services is highly recommended in those cases.

- :scroll: **Usage**: The previous chapter details the purpose, input and output of each script.

---

## :books: Additional Information

A flowchart detailing the training and testing process (*scripts from groups 5 and 6*) is shown here: 

![flowchart](./3_figures/fig4_modelling_flowchart.png)

---

## :information_source: License

The content of this repository is under the [MIT license](./LICENSE).

---

## :link: About the authors

[Aitor Vázquez Veloso](https://github.com/aitorvv): 
[ORCID](https://orcid.org/0000-0003-0227-506X) \\
[UVa](https://portaldelaciencia.uva.es/investigadores/178830/detalle)

Astor Toraño Caicoya: 
[ORDID](https://orcid.org/0000-0002-9658-8990) \\
[TUM](https://www.waldwachstum.wzw.tum.de/en/staff/astor-torano-caicoya/)

[Felipe Bravo Oviedo](https://github.com/Felipe-Bravo): 
[ORCID](https://orcid.org/0000-0001-7348-6695) \\
[UVa](https://portaldelaciencia.uva.es/investigadores/181874/detalle)

Peter Biber:
[ORDID](https://orcid.org/0000-0002-9700-8708) \\
[TUM](https://www.waldwachstum.wzw.tum.de/en/staff/peter-biber/)

Enno Uhl:
[ORDID](https://orcid.org/0000-0002-7847-923X) \\
[TUM](https://www.waldwachstum.wzw.tum.de/en/staff/enno-uhl/)

Hans Pretzsch:
[ORDID](https://orcid.org/0000-0002-4958-1868) \\
[TUM](https://www.waldwachstum.wzw.tum.de/en/staff/hans-pretzsch/)


---

[Comparing Machine Learning algorithms for assessing individual tree mortality](https://github.com/aitorvv/ML_individual_tree_mortality) 