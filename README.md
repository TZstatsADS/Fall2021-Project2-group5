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
+ **Shiny app URL**(https://fall2021-project2-group5.shinyapps.io/Project2-group5/)

+ Team # 5
+ **The Department of Homeless Services during the Pandemic**:
	+ Melissa Bischoff (mb4786)
	+ Xueying Chen (xc2578)
	+ Jing Lu (jl5886)
	+ Yalin Wang (yw3727)
	+ Yarong Wang (yw3598)

+ **Project summary**: Our project will take a look at homelessness prior to and during the pandemic and what measures the NYC Department of Homeless Services took to mitigate homelessness during the pandemic. We will benchmark homelessness during the pandemic to homelessness prior to the pandemic to determine whether these measures that the department took were useful in mitigating homelessness.

+ **Contribution statement**: All team members collaborated on picking the NYC agency that we wanted to do our project on. Each member brought a few datasets from one agency to a meeting to discuss. We settled on using the Department of Homeless Services and used the datasets that Melissa and Jing had found. Each member chose one of these datasets from this agency to do analyses on. **Melissa** chose the dataset that reports average monthly amount of individuals in shelters. With this data she ran a polynomial regression on the data prior to covid to predict what the baseline homelessness would look like if the pandemic hadn't hit and made it into a graph. She also worked on the write-ups of the homepage and conclusion. **Xueying** worked with the data on homeless shelters in each borough over time. With this data, she created an interactive table in the Shiny app that maps homeless shelters over time in NYC. **Jing** used the dataset that has locations of free meals that the Department of Homeless Services gave out during the pandemic. He created an interactive map that shows the locations of these free meals. **Yarong (yw3598)** used data on COVID cases to create several interactive data exploring highcharts to see COVID cases by each borough and over time. She also created the Appendix page in the Shiny app. **Yarong (yw3598) and Jing** helped Melissa put her graph into the shiny app.**Yarong (yw3598) and Jing** also designed and modified the ui function of the whole shiny app project, debug the code of the whole project, and taught team members to figured out the frame of the whole shiny app. **Jing and Xueying** worked on publishing our shiny app. All team members approve our work presented in our GitHub repository including this contribution statement.

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
