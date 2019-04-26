1. dataIngestion.py
* Input files: "Open-old.csv", "Reproduce-old.csv"
* Output files: "Open.csv", "Reproduce.csv"

2. dataTransformation.R
* Input files: "Open.csv", "Reproduce.csv"
* Output files: "openRaw.graphml", "reproduceRaw.graphml", "duplicated_remove.csv", "newdataCombined.csv"

3. visualize.py
* Input files: "openRaw.graphml", "reproduceRaw.graphml"
* Output files: "visualize/openScience.pdf", "visualize/reproducibility.pdf"
***

Once you are inside the Binder [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/iuni-cadre/ReproducibilityDemo.git/e6112131c952a118f8a123f21f8b239f69d31ec6) environment, start a new Jupyter lab session. Navigate to the code-data folder and open the "dataIngestion.ipynb" notebook.

The first 5 cells of the notebook will not run because it requires Azure Blob credentials. But we already have the files downloaded as "Open-old.csv" and "Reproduce-old.csv".

Running cells 6-11 will reformat the abstract encoding and reorder the authors. The resulting output files include "Open.csv" and "Reproduce.csv".

Now open the "dataTransformation.ipynb" notebook, which will use these two files as inputs. The first 3 cells did some data exploration with histograms and filters. 

Based on the findings, the next 4 cells clean the data according to "DocType" and "Year", as well as duplicated article titles. The cleaned data is then used to construct networks as "openRaw.graphml" and "reproduceRaw.graphml". These graphml files are in turn fed into the "visualize.py" which calls a Gephi package to produce visualizations "openScience.pdf" and "reproducibility.pdf".

Finally, after some network analysis, we merge the data sets into "newdataCombined.csv" for downstream gender detection.

![Open Science](https://github.com/iuni-cadre/ReproducibilityDemo/blob/master/code-data/visualize/openScience.png)

![Reproducibility](https://github.com/iuni-cadre/ReproducibilityDemo/blob/master/code-data/visualize/reproducibility.png)