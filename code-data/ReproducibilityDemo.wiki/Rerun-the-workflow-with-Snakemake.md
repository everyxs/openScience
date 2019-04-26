* Input files: "Open-old.csv", "Reproduce-old.csv"
* Output files: "visualize/openScience.pdf", "visualize/reproducibility.pdf", "OpenSci3.csv", "Pie.csv"

***

Once you are inside the Binder [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/iuni-cadre/ReproducibilityDemo.git/e6112131c952a118f8a123f21f8b239f69d31ec6) environment, start a new terminal. Then run the following commands:

```
cd code-data
snakemake
snakemake --forceall --dag | dot -Tpng > workflow.png
```
You can delete output files like "OpenSci3.csv" and reproduce them, including the following DAG "workflow.png".

![](https://github.com/iuni-cadre/ReproducibilityDemo/blob/master/code-data/workflow.png)