rule input:
    input:
        "code-data/Open-old.csv"
    output:
        "code-data/Open.csv"
    script:
        "code-data/dataIngestion.py"
rule output:
    input:
        "code-data/Open.csv"
    output:
        "code-data/plots.pdf"
    script:
        "code-data/dataTransformation.R"
