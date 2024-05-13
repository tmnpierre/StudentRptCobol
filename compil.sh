export COB_LDFLAGS=-Wl,--no-as-needed
export COBCPY=./Copybook

ocesql GradesReport.cbl GradesReport.cob
cobc -locesql -x -o runGradesReport GradesReport.cob

./runGradesReport
cat output.dat