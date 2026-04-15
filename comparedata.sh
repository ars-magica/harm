
cd Data
for i in *
do
   echo $i
   diff $i ../../hibernia/Data/$i
done

