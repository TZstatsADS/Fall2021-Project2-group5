# Project 2: Shiny App Development

### [Project Description](doc/project2_desc.md)

In this second project of GR5243 Applied Data Science, we develop a *Exploratory Data Analysis and Visualization* shiny app on the work of a **NYC government agency/program** of your choice using NYC open data released on the [NYC Open Data By Agency](https://opendata.cityofnewyork.us/data/) website. In particular, many agencies have adjusted their work or rolled out new programs due to COVID, your app should provide ways for a user to explore quantitative measures of how covid has impacted daily life in NYC from different prospectives. See [Project 2 Description](doc/project2_desc.md) for more details.  

The **learning goals** for this project are:

- business intelligence for data science
- data cleaning
- data visualization
- systems development/design life cycle
- shiny app/shiny server

## The Department of Homeless Services during the Pandemic
Term: Fall 2021

+ Team # 5
+ **The Department of Homeless Services during the Pandemic**:
	+ Melissa Bischoff
	+ Xueying Chen
	+ Jing Lu
	+ Yalin Wang
	+ Yarong Wang

+ **Project summary**: Our project will take a look at homelessness prior to and during the pandemic and what measures the NYC Department of Homeless Services took to mitigate homelessness during the pandemic. We will benchmark homelessness during the pandemic to homelessness prior to the pandemic to determine whether these measures that the department took were useful in mitigating homelessness.

+ **Contribution statement**: All team members collaborated on picking the NYC agency that we wanted to do our project on. Each member brought a few datasets from one agency to a meeting to discuss. We settled on using the Department of Homeless Services and used the datasets that Melissa and Jing had found. Each member chose one of these datasets from this agency to do analyses on. Melissa chose the dataset that reports average monthly amount of individuals in shelters for July 2015 to May 2021. With this data she ran a polynomial regression on the data prior to covid to predict what the baseline homelessness would look like if the pandemic hadn't hit and made it into a graph. Xueying worked with the data on homeless shelters in each borough over time. With this data, she created an interactive graph in the Shiny app that maps homeless shelters over time in NYC. Jing used the dataset that has locations of free meals that the Department of Homeless Services gave out during the pandemic. He created an interactive map that shows the locations of these free meals. Yarong used data on covid cases to create an interactive data exploring map to see covid cases by each borough and over time. Yarong and Jing helped Melissa put her graph into the shiny app.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.
