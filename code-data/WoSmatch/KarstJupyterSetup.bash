ssh yan30@karst.uits.iu.edu

qstat
qsub spark_test.pbs

module load java #load sun jdk10 for hadoop
module load hadoop
module load spark
module unload java #use openjdk instead for scala interactive

cd /N/dc2/projects/IUNI_MSAcademic/
module swap python/3.6.8
export PATH=$PATH:~/.local/bin
pip install jupyterlab --user
jupyter serverextension enable --py jupyterlab

pip install --user Toree
export PATH=~/.local/bin:$PATH
jupyter toree install --spark_home=$SPARK_HOME --user
jupyter kernelspec list
jupyter toree install --replace --spark_home=$SPARK_HOME --kernel_name="Spark" --spark_opts="--packages com.databricks:spark-xml_2.11:0.5.0 --master spark://c240.karst.uits.iu.edu:7077 --driver-memory 8G --executor-memory 15G --executor-cores 4 --conf spark.driver.maxResultSize=8g" --user
jupyter notebook --no-browser --port=8000
-----------To check | kill ports, use
lsof -ti:8000 | xargs kill -9

Open another terminal locally,
ssh -N -f -L localhost:8001:127.0.0.1:8000 yan30@karst.uits.iu.edu
Point your browser to localhost:8001
