ssh yan30@karst.uits.iu.edu
pip install --user Toree
export PATH=~/.local/bin:$PATH
jupyter toree install --spark_home=$SPARK_HOME
jupyter kernelspec list
jupyter toree install --replace --spark_home=$SPARK_HOME --kernel_name="Spark" --spark_opts="--packages com.databricks:spark-xml_2.11:0.5.0 --master spark://c240.karst.uits.iu.edu:7077 --driver-memory 32G --executor-memory 5G --executor-cores 3 --conf spark.driver.maxResultSize=8g" --user
jupyter notebook --no-browser --port=8000
-----------To check | kill ports, use
lsof -ti:8000 | xargs kill -9

Open another terminal locally,
ssh -N -f -L localhost:8001:localhost:8000 yan30@karst.uits.iu.edu
Point your browser to localhost:8001
