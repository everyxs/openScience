rule outputs:
    input: 
        "dist/openScience.pdf",
        "dist/reproducibility.pdf",
        "code-data/OpenSci3.csv",
        "code-data/Pie.csv"

rule dataIngestion:
    input:
        "code-data/Open-old.csv", "code-data/Reproduce-old.csv"
    output:
        "code-data/Open.csv", "code-data/Reproduce.csv"
    script:
        "code-data/dataIngestion.py"
        
rule dataTransformation:
    input:
        rules.dataIngestion.output
    output:
        open = "code-data/openRaw.graphml", rep = "code-data/reproduceRaw.graphml", "code-data/duplicated_remove.csv",  genderInput = "code-data/newdataCombined.csv"
    script:
        "code-data/dataTransformation.R"

rule genderDetect:
    input:
        rules.dataTransformation.output.genderInput
    output:
        "code-data/OpenSci3.csv", "code-data/Pie.csv"
    script:
        "code-data/genderDetect.R"
        
rule visualize:
    input:
        rules.dataTransformation.output.open, rules.dataTransformation.output.rep
    output:
        "dist/openScience.pdf", "dist/reproducibility.pdf"
    script:
        "dist/visualize.py"