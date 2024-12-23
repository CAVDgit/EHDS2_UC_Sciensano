# HealthData@EU Pilot (EHDS2) - Sciensano Use Case
The repository contains the scripts that will be used in the HealthData@EU Pilot Sciensano Use Case.
It has been created to easier the files sharing to perform specificaly this activity among contributors and users.

## The HealthData@EU Pilot project https://ehds2pilot.eu/

The HealthData@EU Pilot project is a two-year long European project co-financed by the EU4Health programme. 
It will build a pilot version of the European Health Data Space (EHDS) infrastructure for the secondary use of health data “HealthData@EU” which will serve research, innovation, policy making and regulatory purposes. 
The project will connect data platforms in a network infrastructure and develop services supporting the user journey for research projects using health data from various EU Member States. 
It will also provide guidelines for data standards, data quality, data security and data transfer to support this cross-border infrastructure. 
Priority services include a metadata discovery service and a common health data access request. 
The consortium will collaborate closely with the European Commission and their team working on developing the central services for secondary use of health data. 

In order to illustrate the feasibility and the potential of reusing data from several European countries, the project will run five use cases. 
Based on this experiment, the HealthData@EU Pilot will provide complete evaluation of the project and build recommendations for the European Commission about data standards, legal requirements, costs and economic model needed to scale up the tested network.

The five use cases of the HealthData@EU Pilot have been selected on the recommendation of the European Commission. They aim to demonstrate the feasibility and potential impact of exploiting health data in several countries for public health, research, innovation and healthcare system improvement. 
They will serve as a learning source for the creation of services at node and central service level.

## The Sciensano use case : COVID-19 testing, vaccination and hospitalisation

Supported by Sciensano, the purpose of the third use case is to compare the general population and the vulnerable subpopulations use of testing, vaccination and hospitalisation capacities and possible heterogeneity according to socio-economic factors. 
This use case will mobilise data from six countries : Belgium, Denmark, Croatia, Finland, Hungary and France.

### Analysis pipeline
#### Based on federated infrastructure PHIRI template
This use case follow the PHIRI federated infrastructure methodology https://www.phiri.eu/wp7 
The PHIRI federated architecture consists of
a number of country nodes
acting as Data Hubs, and a central
orchestrating hub at Sciensano: (1) The
orchestrating hub develops, implements and
shares the analytical pipeline and provides
support to the federated research
infrastructure for its deployment; (2) Nodes
deploy the pipeline local analyses on their
premises; (3) intermediate outputs (e.g.,
aggregated results or models) obtained from the local analyses are sent to the Central Hub, so, no
sensitive data is shared across the federation of nodes but only digital objects; (4) the central hub
can perform meta-analyses with those intermediate outputs if required.

This stepwise approach includes the following:
1. Formalising the Research Question,
2. Iteratively building a Common Data Model specification with the contribution of all participants in each research question,
3. Generating a mockup dataset following the specifications of the agreed common data model,
4. Iteratively developing and testing the scripts (code) implementing the Data Quality Analysis using the mockup dataset, tailored to the quality requirements of the research question,
5. Iteratively developing and testing the scripts (code) of the statistical analysis using the mockup dataset,
6. Deploying the use case in each participant node by distributing the common data model, the mockup dataset (as data example), and the data quality analysis and statistical analysis script in a reproducible way,
7. Finally, producing an analysis report in all the nodes.

#### Description of the files
For the purpose of the use case, the pipeline is composed of several scripts:
1. **Data quality script** (Quarto R) -> output : html document
2. **Data analysis script** (R) -> output : csv file containing only aggregate data
3. **Node report script** (Quarto R) -> output : html document
4. **Shape file** (shp) -> used by the Node report script to generate the choropleth maps.
5. **Mockup data file** (csv) -> Mockup data given as an example for the use case.
6. **Mockup quality report** (html) -> Report based on mockup data.
7. **Mockup final report** (html) -> Report based on mockup data.

Files in part1 folder: These scripts should be executed on the individual data in the secure processing environment.

##### Data Quality script ```EHDS2_pilot_UC_1_data_quality.qmd```
Evaluation of the compliance of individual data with the common data model. Based on the quality script designed by [BY-COVID](https://github.com/MarjanMeurisse/BY-COVID_WP5_T5.2_baseline-use-case).

##### Data analysis script ```EHDS2_pilot_UC_2_data_analysis.R```
Analysis script that will generate the aggregate output for a node.

Files in **part2** folder :
##### Node report script ```EHDS2_pilot_UC_3_final_report.qmd```
Report script that will generate an interactive HTML document addressing the research questions.

##### Shape dimensions files ```EHDS2_pilot_UC1_nuts_code.shp```, ```EHDS2_pilot_UC1_nuts_code.dbf```, ```EHDS2_pilot_UC1_nuts_code.prj```, ```EHDS2_pilot_UC1_nuts_code.shx```
Dependencies necessary for the node report script.


#### Instructions
The analytical pipeline is divided into 2 parts. The part1 should be executed in the secure processing environment because it will use sensitive data to generate non-sensitive data. Part2 should be executed outside the secure processing environment because it requires more dependencies that may not always be available inside the secure processing environment.

Part1:
  1. Import the data quality ```EHDS2_pilot_UC_1_data_quality.qmd``` and data analysis ```EHDS2_pilot_UC_2_data_analysis.R``` scripts into the secure processing environment.
  2. Place in the same folder: your individual data (csv) compliant with the common data model, the quality script, and the data analysis script.
  3. Data quality script ```EHDS2_pilot_UC_1_data_quality.qmd```:
     - Open the script using RStudio and set the working directory to the actual location of the script.
     - Modify the filename in the script (file_path <- "EHDS2_pilot_UC1_mockup_data_BE.csv") to the actual filename of your csv file.
     - Execute the script to generate the HTML quality report.
  4. Data analysis script ```EHDS2_pilot_UC_2_data_analysis.R```:
     - Open the script using RStudio and set the working directory to the actual location of the script.
     - Modify the filename in the script (default: file_path <- "EHDS2_pilot_UC1_mockup_data_BE.csv") to the actual filename of your csv file.
     - Set the node country (default: country <- 'BE'). Possible value : BE, FR, FI, HU, HR, DK.
     - (optional) Adapt the cut-off/threshold to exclude aggregated data with a number of individuals lower than this threshold (default: threshold <- 10).
     - Execute the script to generate the aggregated data (```EHDS2_pilot_UC1_data_BE.csv```) that can be exported outside the secure processing environment.
    
Part2:
  1. Place in the same folder : the aggregated data (```EHDS2_pilot_UC1_data_BE.csv```), the node report script ```EHDS2_pilot_UC_3_final_report.qmd``` and the shape files ```EHDS2_pilot_UC1_nuts_code.shp```, ```EHDS2_pilot_UC1_nuts_code.dbf```, ```EHDS2_pilot_UC1_nuts_code.prj```, ```EHDS2_pilot_UC1_nuts_code.shx```.
  2. Open the script using RStudio and set the working directory to the actual location of the script.
  3. Execute the script to generate the final HTML report.

The analytical pipeline can be executed for testing purposes using the mockup data ```EHDS2_pilot_UC1_mockup_data_BE.csv``` provided on this github.

##### Mockup data ```EHDS2_pilot_UC1_mockup_data_BE.csv```
Belgium mockup data compliant with the common data model defined in the use case is provided to showcase the analytical pipeline. The data are fictitious without any realistic correlations. It contains 200k individuals with random empty values.

##### Mockup quality report ```EHDS2_pilot_UC_1_mockup_quality_report.html```
A quality report based on the mockup data is available for download as example.

##### Mockup final report ```EHDS2_pilot_UC_3_mockup_final_report.html```
A final report based on the mockup data is available for download as example.

    
#### Privacy Measures in Data Processing
The script incorporates several privacy measures to ensure that sensitive data is protected and anonymized:

Privacy-Rounded Totals : The total number of individuals (individuals_nm) and the counts for vaccinated and non-vaccinated individuals are rounded up, with further adjustments to ensure that this rounding always results in a change of at least a specified threshold of individuals. This rounding prevents exact back-calculation of individual counts, protecting against re-identification risks.

Threshold-Based Exclusion: Any aggregated data group with fewer than a specified threshold of individuals is excluded from the final output. These groups are marked with n = -1, and any associated statistics, such as median or quartiles, are not calculated, thus preventing the identification of small subgroups.

Threshold-Median and Threshold-Quartile Functions: The script includes custom functions, threshold_median and threshold_quartile, which only compute median and quartile values if they are based on a sufficient number of individuals (meeting the specified threshold). If the count of individuals supporting these statistics is below the threshold, the function returns NA, ensuring that potentially sensitive statistical information is not disclosed for small groups.

Aggregation of Data : The script only outputs aggregated data rather than individual-level data, reducing the risk of exposing sensitive information. Statistical summaries are produced for broader categories such as age, education, and income levels, with any group-level statistics suppressed if they do not meet the threshold criteria.

These measures collectively ensure that the processed data retains its utility for analysis while significantly reducing the risk of identifying individuals, thereby aligning with best practices for data anonymization and privacy protection.



