cd tests
rm -rf *

echo "1+1" > add.2.1
echo "2-1" > sub.1.2
echo "2*3" > times.6.3
echo "4/2" > div.2.4
echo "1 < 3" > inf.true.5
echo "3 <= 4" > infeq.true.6
echo "5 > 6" > sup.false.7
echo "6 >= 8" > supeq.false.8
echo "true || false" > or.true.9
echo "false && true" > and.false.10
echo "let x = 1 < 2 in x == false" > letin1.false.11 
echo "let x = (1 + 1) in (x > (x + x))" > letin2.false.12
echo "if 1 == 1 then true else false fi" > simplify1.true.13
echo "if 1 == 1 then 2 > 3 else false fi" > simplify2.false.14

for file in *; do
    NAME=`echo $file | sed -r 's/([a-z0-9]*)\.([a-z0-9]*)\.([a-z0-9]*)/\1/'`
    RESULT=`echo $file | sed -r 's/([a-z0-9]*)\.([a-z0-9]*)\.([a-z0-9]*)/\2/'`
    NUMBER=`echo $file | sed -r 's/([a-z0-9]*)\.([a-z0-9]*)\.([a-z0-9]*)/\3/'`

    echo "Testing $NAME #$NUMBER "
    
    cat ./$file | ../exec

    echo "Result should be : $RESULT";
done

cd ..

