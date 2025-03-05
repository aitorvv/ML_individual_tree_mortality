# Does Machine Learning outperform Logistic Regression in predicting individual tree mortality? 

### :computer: :floppy_disk: :bar_chart: *Original data, code and results related to the study*

---

<!--
DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10216009.svg)](https://doi.org/10.5281/zenodo.10216009)
-->

<!--
:open_file_folder: Repository DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14725328.svg)](https://doi.org/10.5281/zenodo.14725328)
-->

---

## :sparkles: Highlights 

- 6 different Machine Learning algorithms were compared in predicting individual tree mortality. 
- Effects of dataset size, variable set, thinning, inventory length, and cross-validation were studied. 
- Random Forest reached a higher performance level in all the case studies proposed except on cross-validation. 
- Logistic binomial Regression seems to be a more robust algorithm regarding cross-validation. 

---

## :book: Abstract

Tree mortality is a crucial process in forest dynamics and a key component of forest growth models and simulators. Factors like competition, drought, and pathogens drive tree mortality, but the underlying mechanism is challenging to model. The current environmental changes are even complicating model approaches as they influence and alter all the factors involving mortality. However, innovative classification algorithms can go deep into data to find patterns that can model or even explain their relationship. We use Logistic binomial Regression as the reference algorithm for predicting individual tree mortality. However, different machine learning (ML) alternatives already applied to other forest modeling topics can be used for this purpose. Here, we compare the performance of five different ML algorithms (Decision Trees, Random Forest, Naive Bayes, K-Nearest Neighbour, and Support Vector Machine) against Logistic binomial Regression in individual tree mortality classification under 40 different case studies and a cross-validation case study. The data used corresponds to Norway spruce long-term experimental plots, which have a total of 75,522 tree records and a 10.28% mortality rate on average. Through different case studies, when more variables were used, general performance improved as expected, while more extensive datasets decreased the performance level of the algorithms. Performance was also higher when plots remained without management compared to thinned ones. Random Forest outperformed the other algorithms in all the cases except cross-validation, where it was the weaker one. Our results demonstrate the potential of ML in assessing tree mortality. When the model application is not clearly defined and/or model interpretability is needed, Logistic binomial Regression is still the best tool for evaluating individual tree mortality.

---


## :file_folder: Repository Contents

- :open_file_folder: [***1_data***](./1_data/): raw and processed data, check [here](./data/README.md) for a detailed description
- :open_file_folder: [***2_code***](./2_code/): compilation of the code used for data curation, analysis and outputs included in the document, check [here](./scripts/README.md) for a detailed description
- :open_file_folder: [***3_figures***](./3_figures/): figures, charts, tables and additional resources included in the document, check [here](./output/README.md) for a detailed description
- :open_file_folder: [***4_bibliography***](./4_bibliography/): compilation of all the literature cited or consulted during the creation of the document

---

## :thinking: How to use the resouces of that repository

:dizzy: To download the information of that repository, you can follow this [guide](https://docs.github.com/en/repositories/working-with-files/using-files/downloading-source-code-archives).

:recycle: To reproduce the analysis, users must:

- :floppy_disk: **Data**: 
    - WorldClim data required for the simulations must be downloaded from its [official website](https://www.worldclim.org/data/index.html)

- :computer: **Prerequisites: installation and code**: *[R](https://cran.r-project.org/bin/windows/base/)* must be installed to run the code with the used libraries across each script (*[RStudio](https://posit.co/download/rstudio-desktop/)* was also used to develop the code). Some analyses (specifically when training RF models) will request high computation power, which can provoke out-of-memory in a normal computer. Access to high-computing services is highly recommended in those cases.

- :scroll: **Usage**: follow the numerical order of the scripts to reproduce each step correctly

---

## :link: About the authors

#### Aitor Vázquez Veloso:

[![](https://github.com/aitorvv.png?size=50)](https://github.com/aitorvv) 

[![Email](https://img.shields.io/badge/Email-D14836?logo=gmail&logoColor=white)](mailto:aitor.vazquez.veloso@uva.es)
[![ORCID](https://img.shields.io/badge/ORCID-green?logo=orcid)](https://orcid.org/0000-0003-0227-506X)
[![Google Scholar](https://img.shields.io/badge/Google%20Scholar-4285F4?logo=google-scholar&logoColor=white)](https://scholar.google.com/citations?user=XNMn1cUAAAAJ&hl=es&oi=ao)
[![ResearchGate](https://img.shields.io/badge/ResearchGate-00CCBB?logo=researchgate&logoColor=white)](https://www.researchgate.net/profile/Aitor_Vazquez_Veloso)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-blue?logo=linkedin)](https://linkedin.com/in/aitorvazquezveloso/)
[![X](https://img.shields.io/badge/X-1DA1F2?logo=x&logoColor=white)](https://twitter.com/aitorvv)
[<img src="https://media.licdn.com/dms/image/v2/D4D0BAQFazHOlOJO50A/company-logo_200_200/company-logo_200_200/0/1692170343519/universidad_de_valladolid_logo?e=1747872000&v=beta&t=1mTS-xC7h3L_DQATdt6hpqjWGgW_Am3MXKnjYwcOVZs" alt="Description" width="22">](https://portaldelaciencia.uva.es/investigadores/178830/detalle)

#### Astor Toraño Caicoya:

<img src="https://www.lss.ls.tum.de/fileadmin/_processed_/f/0/csm_Picture20_57f925f9ae.webp" alt="Description" width="50"> 

[![ORCID](https://img.shields.io/badge/ORCID-green?logo=orcid)](https://orcid.org/0000-0002-9658-8990) 
[![ResearchGate](https://img.shields.io/badge/ResearchGate-00CCBB?logo=researchgate&logoColor=white)](https://www.researchgate.net/profile/Astor-Torano-Caicoya) 
[![LinkedIn](https://img.shields.io/badge/LinkedIn-blue?logo=linkedin)](https://www.linkedin.com/in/toranoac) 
[<img src="https://media.licdn.com/dms/image/v2/D4D0BAQEPj4W4lIWpzQ/company-logo_200_200/company-logo_200_200/0/1719581261705/technische_universitat_munchen_logo?e=1747872000&v=beta&t=qhZOKI6W0rq_w2zi1Ny9LYLtP8N6HiU7q-kFebd6hUI" alt="Description" width="22">](https://www.waldwachstum.wzw.tum.de/en/staff/astor-torano-caicoya/)

#### Felipe Bravo Oviedo:

[![](https://github.com/Felipe-Bravo.png?size=50)](https://github.com/Felipe-Bravo) 

[![ORCID](https://img.shields.io/badge/ORCID-green?logo=orcid)](https://orcid.org/0000-0001-7348-6695) 
[![ResearchGate](https://img.shields.io/badge/ResearchGate-00CCBB?logo=researchgate&logoColor=white)](https://www.researchgate.net/profile/Felipe-Bravo-11) 
[![LinkedIn](https://img.shields.io/badge/LinkedIn-blue?logo=linkedin)](https://www.linkedin.com/in/felipebravooviedo) 
[![X](https://img.shields.io/badge/X-1DA1F2?logo=x&logoColor=white)](https://twitter.com/fbravo_SFM) 
[<img src="https://media.licdn.com/dms/image/v2/D4D0BAQFazHOlOJO50A/company-logo_200_200/company-logo_200_200/0/1692170343519/universidad_de_valladolid_logo?e=1747872000&v=beta&t=1mTS-xC7h3L_DQATdt6hpqjWGgW_Am3MXKnjYwcOVZs" alt="Description" width="22">](https://portaldelaciencia.uva.es/investigadores/181874/detalle)

#### Peter Biber:

<img src="https://www.waldwachstum.wzw.tum.de/fileadmin/_processed_/a/b/csm_Biber_WunschNeu_53b390f147.jpg" alt="Description" width="50"> 

[![ORCID](https://img.shields.io/badge/ORCID-green?logo=orcid)](https://orcid.org/0000-0002-9700-8708) 
[![ResearchGate](https://img.shields.io/badge/ResearchGate-00CCBB?logo=researchgate&logoColor=white)](https://www.researchgate.net/profile/Peter-Biber-3) 
[<img src="https://media.licdn.com/dms/image/v2/D4D0BAQEPj4W4lIWpzQ/company-logo_200_200/company-logo_200_200/0/1719581261705/technische_universitat_munchen_logo?e=1747872000&v=beta&t=qhZOKI6W0rq_w2zi1Ny9LYLtP8N6HiU7q-kFebd6hUI" alt="Description" width="22">](https://www.waldwachstum.wzw.tum.de/personen/peter-biber/)

#### Enno Uhl:

<img src="https://i1.rgstatic.net/ii/profile.image/280045881970690-1443779549227_Q128/Enno-Uhl.jpg" alt="Description" width="50"> 

[![ORCID](https://img.shields.io/badge/ORCID-green?logo=orcid)](https://orcid.org/0000-0002-9700-8708) 
[![ResearchGate](https://img.shields.io/badge/ResearchGate-00CCBB?logo=researchgate&logoColor=white)](https://www.researchgate.net/profile/Enno-Uhl) 

#### Hans Pretzsch:

<img src="https://www.professoren.tum.de/fileadmin/w00bgr/www/pics/PretzschHans.jpg" alt="Description" width="50"> 

[![ORCID](https://img.shields.io/badge/ORCID-green?logo=orcid)](https://orcid.org/0000-0002-4958-1868) 
[![ResearchGate](https://img.shields.io/badge/ResearchGate-00CCBB?logo=researchgate&logoColor=white)](https://www.researchgate.net/scientific-contributions/Hans-Pretzsch-38528857) 
[<img src="https://media.licdn.com/dms/image/v2/D4D0BAQEPj4W4lIWpzQ/company-logo_200_200/company-logo_200_200/0/1719581261705/technische_universitat_munchen_logo?e=1747872000&v=beta&t=qhZOKI6W0rq_w2zi1Ny9LYLtP8N6HiU7q-kFebd6hUI" alt="Description" width="22">](https://www.waldwachstum.wzw.tum.de/en/staff/hans-pretzsch/)

---

## ℹ License 

[![MIT License](https://img.shields.io/badge/license-MIT-red.svg)](./LICENSE)

The content of this repository is under the [MIT license](./LICENSE).


---

## :pencil: How to cite this repository?

You can use the [citation file](CITATION.cff) or copy the citation directly into APA or BibTeX using the bottom *Cite this repository* on the right hand side of the repository content, [here are more details](https://docs.github.com/es/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-citation-files).


---

[Does Machine Learning outperform Logistic Regression in predicting individual tree mortality?](https://github.com/aitorvv/ML_individual_tree_mortality) 
