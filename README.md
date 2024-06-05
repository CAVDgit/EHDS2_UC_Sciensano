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
4. **Shape file** (shp) -> used by the Node report script to generate the choropethe maps.
5. **Mockup data file** (csv) -> Mockup data given as example for the use case.

Files in **part1** folder :
These scripts should be excutated on the individual data into the secure processing environnement.
##### Data Quality script ```EHDS2_pilot_UC_1_data_quality.qmd```
Evaluation of the compliance of individual data with the common data model. Based on the quality script designed by [BY-COVID](https://github.com/MarjanMeurisse/BY-COVID_WP5_T5.2_baseline-use-case).

##### Data analysis script ```EHDS2_pilot_UC_2_data_analysis.R```
Analysis script that will generate the aggregate output for a node.

Files in **part2** folder :
##### Node report script ```EHDS2_pilot_UC_3_final_report.qmd```
Report script that will generate an interactive HTML document addressing the research questions.

##### Shape dimensions file ```EHDS2_pilot_UC1_nuts_code.shp```
Dependency necessary for the node report script.


#### Instructions
The analytical pipeline is divided into 2 parts.
The part1 should be executated into the secure processing environnement because it will use sensitive data to generate non-sensitive data.
The part2 should be executated outside the secure processing environnement because it requires more depedencies that may not always be available inside secure processing environnement.

Part1:
  1. Import the data quality ```EHDS2_pilot_UC_1_data_quality.qmd``` and data analysis ```EHDS2_pilot_UC_2_data_analysis.R``` scripts into the secure processing environment.
  2. Place into the same folder : your individual data (csv) compliant to the command data model, the quality script and the data analysis script.
  3. Data quality script ```EHDS2_pilot_UC_1_data_quality.qmd```:
     - Open the script using RStudio and set the working directory to the actual location of the script.
     - Modify the filename in the script (file_path <- "EHDS2_pilot_UC1_mockup_data_BE.csv") to the actual filename of your csv file.
     - Execute the script to generate the HTML quality report.
  4. Data analysis script ```EHDS2_pilot_UC_2_data_analysis.R```:
     - Open the script using RStudio and set the working set the working directory to the actual location of the script.
     - Modify the filename in the script (default: file_path <- "EHDS2_pilot_UC1_mockup_data_BE.csv") to the actual filename of your csv file.
     - Set the node country (default: country <- 'BE'). Possible value : BE, FR, FI, HU, HR, DK.
     - (optional) Adapt the cut-off/threshold to excluded aggregated data with number of individual inferior to this threshold. (default: threshold <- 10)
     - Execute the script to generated the aggregated data (```EHDS2_pilot_UC1_data_BE.csv```) that can be exported outside the secure processing environnement.

Part2:
  1. Place into the same folder : the aggregated data (```EHDS2_pilot_UC1_data_BE.csv```), the node report script ```EHDS2_pilot_UC_3_final_report.qmd``` and the shape file ```EHDS2_pilot_UC1_nuts_code.shp```.
  2. Open the script using RStudio and set the working directory to the actual location of the script.
  3. Execute the script to generate the HTML final report report.

The analytical can be executated for testing purpose using the mockup data ```EHDS2_pilot_UC1_mockup_data_BE.csv``` provided on this github.

##### Mockup data ```EHDS2_pilot_UC1_mockup_data_BE.csv```
Belgium mockup data compliant with the common data model defined in the use case are provided to showcase the analytical pipeline. 
The data are fictitious without any realistic correlations. It contains 200k individuals with random empty values.
