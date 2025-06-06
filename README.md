# About this Repository

## File Directory 

#### Quarto files (Site content)
* Visualizations.qmd: Main meat of the analysis. Contains code for visualizations page. Pulls helpers from /functions. 
* DI.qmd: contains code for DI explanation page 
* index.qmd: contains code for home page

#### Stylin'
* _quarto.yml, _quarto-english.yml, _quarto-french.yml: YAML config files that contain instructions for page layout
* styles.css: a few css instrcutions for page formatting 
* /images: Name says it all. Favicons and such. 


#### Let's talk data 
* /intermediate data: Where a few intermediate tables are stored. Look here raw binned salary data, DI's, and medians. All created in Visualizations.qmd. 
* /supplementary_tables: Contains information needed to work with the TBS data. Imputated values, salary bins per year, quintile ranges, and labels to translate English figures to French. 
* /cached_data: holds counts data after webscraping from TBS site

#### Other bits and bobs
* /functions: contains functions that webscrape data and calculate medians and DI's. all of these are helps in the Visualizations.qmd file
* /docs: where html files render to


## Deploying the Page with Github Pages and Quarto 

The page is built with Quarto. The various .qmd files correspond to sections of the webpage. Steps to deploying the page as as follows: 

1. Make a change to the repo files.
2. Render both language profiles with `quarto render --profile english` and `quarto render --profile french`. The site's translation button will not work when hosted locally, but will once both rendered profiles are in /docs (where html files are rendered to) in the repo. 
3. Push changes to main, including any changes to /docs. 
4. Ensure that Github pages is deploying from docs ([as descirbed in the Quarto documentation here.](https://quarto.org/docs/publishing/github-pages.html#render-to-docs))
5. Enjoy your updated site. 


## Profiles for Multilingual Site

Content marked by html divs differentiate between English and french content. _quarto-english.yml and _quarto-french.yml establish the two profiles. 

`::: {.content-visible when-profile="english"}
English content here
:::
`

`::: {.content-visible when-profile="french"}
French content here
:::
`
[More info on Quarto profiles for rendering multilingual sites at Mario Angst's site.](https://quarto-dev.marioangst.com/en/blog/posts/multi-language-quarto/)