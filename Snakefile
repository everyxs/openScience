rule outputs:
    input: 
        "dist/openScience.pdf",
        "dist/reproducibility.pdf",
        "code-data/OpenSci3.csv",
        "code-data/Pie.csv"

rule dataIngestion:
    input:
        "Open-old.csv", "Reproduce-old.csv"
    output:
        "Open.csv", "Reproduce.csv"
    script:
        "dataIngestion.py"
        
rule dataTransformation:
    input:
        rules.dataIngestion.output
    output:
        open = "openRaw.graphml", rep = "reproduceRaw.graphml", "duplicated_remove.csv",  genderInput = "newdataCombined.csv"
    script:
        "dataTransformation.R"

rule genderDetect:
    input:
        rules.dataTransformation.output.genderInput
    output:
        "OpenSci3.csv", "Pie.csv"
    script:
        "genderDetect.R"
        
rule visualize:
    input:
        rules.dataTransformation.output.open, rules.dataTransformation.output.rep
    output:
        "dist/openScience.pdf", "dist/reproducibility.pdf"
    shell:
        "dist/java -jar "GephiVisualization.jar"