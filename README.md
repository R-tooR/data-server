# data-server

An experimental application whose aim is to calculate stock indicators, and make price movement predicitions basing on indicators values. 
Due to money needed for amount of potential requests it needs to retrieve the data, hardcoded files are provided to run.

In order to run you need to have installed:
- Java 15 (openjdk)
- Scala 2.12 (IntelliJ plugin should also work)
- sbt plugin manager

## To run:
1) Clone this repository
2) Build project
3) Run `java -cp data-server Server -appconfig=path/to/appConfiguration.properties`

Application should start, and listen on port specified in `appConfiguration.properties` file


### Important:
The file `appConfiguration.properties` contains configuration of parameters related to connection to database and logic of application.
Property specifying path to directory of files containing historical data is: `stock.data.path` (in `appConfiguratoin.properties`). 
Data-server has to have access to those files in order to run.
Such files can be downloaded here: https://drive.google.com/file/d/1lURijN_cLSf1nntuuLDZGR06HCd7PoR_/view?usp=sharing
