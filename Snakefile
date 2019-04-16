rule all:
    input: 
        "Open-old.csv",
        "plots.pdf"

rule readData:
    input:
        "Open-old.csv"
    output:
        "Open.csv"
    script:
        "dataIngestion.py"
        
rule plotData:
    input:
        rules.readData.output
    output:
        "plots.pdf"
    script:
        "dataTransformation.R"
