set -e

echo "/************/"
echo "/* BUILDING */"
echo "/************/"
echo ""

cd `dirname $0`

cd build

cmake ..

make

echo ""
echo "/***********/"
echo "/* TESTING */"
echo "/***********/"
echo ""

# ctest ..

make Experimental

echo ""
echo "/***********/"
echo "/* SUCCESS */"
echo "/***********/"
echo ""
