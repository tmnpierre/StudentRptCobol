       IDENTIFICATION DIVISION.
       PROGRAM-ID. GradesReport.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ReportFile ASSIGN TO "output.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ReportFile.
       01 ReportRecord PIC X(150).

       WORKING-STORAGE SECTION.

       01 DB-ConnectionInfo.
           05 DB-Username PIC X(30) VALUE "cobol".
           05 DB-Password PIC X(10) VALUE SPACES.
           05 DB-Name     PIC X(30) VALUE "student".

       01 Student-Details.
           05 Student-ID PIC 9(4).
           05 Lastname   PIC X(20).
           05 Firstname  PIC X(20).

       01 Course-Details.
           05 Course-ID     PIC 9(4).
           05 Label-Course  PIC X(35).
           05 Coef-Course   PIC 9(3)v9.
           05 Average-Grade PIC 99v99.

       01 Header-Line PIC X(150) VALUE ALL "*".
       01 Title-Line  PIC X(150) VALUE SPACES.
       01 Column-Header PIC X(150) VALUE 
           "ID         Last Name                  First Name".
       01 Student-Report-Line PIC X(150).
       01 COURSE-POSITION PIC 9(3) VALUE 48.

OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(043) VALUE "SELECT id, lastname, firstname"
OCESQL  &  " FROM student".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(049) VALUE "SELECT id, label, coef, averag"
OCESQL  &  "e_grade FROM course".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN OUTPUT ReportFile

           PERFORM 1000-Connect-To-DB
           PERFORM 3000-Generate-Report-Header
           PERFORM 2100-Fetch-Course.
           PERFORM 2000-Fetch-Students
           PERFORM 9000-Close-DB
           CLOSE ReportFile
           STOP RUN.

       1000-Connect-To-DB.
OCESQL*    EXEC SQL
OCESQL*        CONNECT :DB-Username IDENTIFIED BY :DB-Password
OCESQL*        USING :DB-Name
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE DB-Username
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE DB-Password
OCESQL          BY VALUE 10
OCESQL          BY REFERENCE DB-Name
OCESQL          BY VALUE 30
OCESQL     END-CALL.

       2000-Fetch-Students.
OCESQL*    EXEC SQL
OCESQL*        DECLARE STUDENT_CURSOR CURSOR FOR
OCESQL*        SELECT id, lastname, firstname FROM student
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "GradesReport_STUDENT_CURSOR" & x"00"
OCESQL          BY REFERENCE SQ0001
OCESQL     END-CALL.
OCESQL*    EXEC SQL
OCESQL*        OPEN STUDENT_CURSOR
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "GradesReport_STUDENT_CURSOR" & x"00"
OCESQL     END-CALL.

           PERFORM UNTIL SQLCODE = 100
OCESQL*        EXEC SQL FETCH STUDENT_CURSOR INTO :Student-ID,
OCESQL*                                           :Lastname,
OCESQL*                                           :Firstname
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE Student-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 20
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE Lastname
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 20
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE Firstname
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "GradesReport_STUDENT_CURSOR" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
               IF SQLCODE = 0
                   PERFORM 3100-Generate-Student-Report
               END-IF
           END-PERFORM.

OCESQL*    EXEC SQL
OCESQL*        CLOSE STUDENT_CURSOR
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "GradesReport_STUDENT_CURSOR" & x"00"
OCESQL     END-CALL
OCESQL    .

       2100-Fetch-Course.
OCESQL*    EXEC SQL
OCESQL*        DECLARE COURSE_CURSOR CURSOR FOR
OCESQL*        SELECT id, label, coef, average_grade FROM course
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "GradesReport_COURSE_CURSOR" & x"00"
OCESQL          BY REFERENCE SQ0002
OCESQL     END-CALL.
OCESQL*    EXEC SQL
OCESQL*        OPEN COURSE_CURSOR
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "GradesReport_COURSE_CURSOR" & x"00"
OCESQL     END-CALL.

           PERFORM UNTIL SQLCODE = 100
OCESQL*        EXEC SQL FETCH COURSE_CURSOR INTO :Course-ID,
OCESQL*                                          :Label-Course,
OCESQL*                                          :Coef-Course,
OCESQL*                                          :Average-Grade
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE Course-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 35
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE Label-Course
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE Coef-Course
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE Average-Grade
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "GradesReport_COURSE_CURSOR" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
               IF SQLCODE = 0
                   PERFORM 3200-Generate-Course-Report
                   COMPUTE COURSE-POSITION = COURSE-POSITION + 25
               END-IF
           END-PERFORM.
           
           WRITE ReportRecord FROM Student-Report-Line. 
           
OCESQL*    EXEC SQL
OCESQL*        CLOSE COURSE_CURSOR
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "GradesReport_COURSE_CURSOR" & x"00"
OCESQL     END-CALL
OCESQL    .

       3000-Generate-Report-Header.
           MOVE Header-Line TO ReportRecord.
           WRITE ReportRecord FROM ReportRecord.

           STRING "BULLETIN DE NOTES" INTO Title-Line(67:17) 
           WRITE ReportRecord FROM Title-Line.

           MOVE Header-Line TO ReportRecord.
           WRITE ReportRecord FROM ReportRecord.

           MOVE Column-Header TO ReportRecord.
           WRITE ReportRecord FROM ReportRecord.

           MOVE Header-Line TO ReportRecord.
           WRITE ReportRecord FROM ReportRecord.

       3100-Generate-Student-Report.
           MOVE SPACES TO Student-Report-Line
           STRING Student-ID DELIMITED BY SIZE
                  "       "
                  Lastname DELIMITED BY SIZE
                  "       "
                  Firstname DELIMITED BY SIZE
                  INTO Student-Report-Line
           WRITE ReportRecord FROM Student-Report-Line.

       3200-Generate-Course-Report.
           MOVE Label-Course TO 
               Student-Report-Line (COURSE-POSITION:25).

       9000-Close-DB.
OCESQL*    EXEC SQL
OCESQL*        DISCONNECT ALL
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.
           END-EXEC.
           END-EXEC.
