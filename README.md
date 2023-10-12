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
3. Generating a synthetic dataset following the specifications of the agreed common data model,
4. Iteratively developing and testing the scripts (code) implementing the Data Quality Analysis using the synthetic dataset, tailored to the quality requirements of the research question,
5. Iteratively developing and testing the scripts (code) of the statistical analysis using the synthetic dataset,
6. Deploying the use case in each participant node by distributing the common data model, the synthetic dataset (as data example), and the data quality analysis and statistical analysis script in a reproducible way,
7. Collecting the local (on-premise) outputs produced by the local analysis of each participant nodes to summarise them by the use case leader,
8. Finally, producing the final report based on the meta-analysis of the collection of local outputs from all the nodes.

#### Description of the files

