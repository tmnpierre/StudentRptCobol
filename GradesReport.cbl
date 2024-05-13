       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADESREPORT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORTFILE ASSIGN TO "output.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD REPORTFILE.
       01 REPORTRECORD PIC X(200).
       
       WORKING-STORAGE SECTION.
       
       01 DB-CONNECTIONINFO.
           05 DB-USERNAME PIC X(30) VALUE "cobol".
           05 DB-PASSWORD PIC X(10) VALUE SPACES.
           05 DB-NAME     PIC X(30) VALUE "student".
       
       01 STUDENT-DETAILS.
           05 STUDENT-ID PIC 9(4).
           05 LASTNAME   PIC X(35).
           05 FIRSTNAME  PIC X(35).
           05 TOTAL-GRADE PIC 99V99.
       
       01 COURSE-DETAILS.
           05 COURSE-ID     PIC 9(4).
           05 LABEL-COURSE  PIC X(35).
           05 COEF-COURSE   PIC 9(3)V9.
           05 AVERAGE-GRADE PIC 99V99.
       
       01 GRADE-DETAILS.
           05 GRADE-STUDENT-ID PIC 9(4).
           05 GRADE-COURSE-ID  PIC 9(4).
           05 GRADE-VALUE      PIC 99V99.
       
       01 HEADER-LINE PIC X(200) VALUE ALL "*".
       01 TITLE-LINE  PIC X(200) VALUE SPACES.
       01 COLUMN-HEADER PIC X(200) VALUE 
           "NOM        PRENOM     MOYENNE     C1        C2        C3"
           "        C4        C5        C6".
       01 STUDENT-REPORT-LINE PIC X(200).
       01 COURSES-LINE PIC X(48).
       
       01 WS-COURSE-NAMES OCCURS 6 TIMES PIC X(8).
       01 WS-GRADE PIC 99V99 VALUE 0.
       01 WS-STUDENT-COUNTER PIC 9(3) VALUE 0.
       01 WS-TOTAL-AVERAGE PIC 99V99 VALUE 0.
       01 WS-CLASS-AVERAGE PIC 99V99 VALUE 0.
       
       01 COURSE-LIST.
           05 COURSE-INFO OCCURS 6 TIMES.
               10 COURSE-ID PIC 9(4).
               10 COURSE-LABEL PIC X(35).
               10 COURSE-COEF PIC 9(3)V9.
               10 COURSE-AVERAGE PIC 99V99.
       
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN OUTPUT REPORTFILE
       
           PERFORM 1000-CONNECT-TO-DB
           PERFORM 3000-GENERATE-REPORT-HEADER
           PERFORM 2100-FETCH-COURSES
           PERFORM 2000-FETCH-STUDENTS
           PERFORM 4000-GENERATE-CLASS-AVERAGE
           PERFORM 5000-GENERATE-COURSE-DETAILS
           PERFORM 9000-CLOSE-DB
           CLOSE REPORTFILE
           STOP RUN.
       
       1000-CONNECT-TO-DB.
           EXEC SQL
               CONNECT :DB-USERNAME IDENTIFIED BY :DB-PASSWORD
               USING :DB-NAME
           END-EXEC.
       
       2000-FETCH-STUDENTS.
           EXEC SQL
               DECLARE STUDENT_CURSOR CURSOR FOR
               SELECT ID, LASTNAME, FIRSTNAME, TOTAL_GRADE FROM STUDENT
           END-EXEC.
           EXEC SQL
               OPEN STUDENT_CURSOR
           END-EXEC.
       
           PERFORM UNTIL SQLCODE = 100
               EXEC SQL FETCH STUDENT_CURSOR INTO :STUDENT-ID,
                                                 :LASTNAME,
                                                 :FIRSTNAME,
                                                 :TOTAL-GRADE
               END-EXEC
               IF SQLCODE = 0
                   ADD 1 TO WS-STUDENT-COUNTER
                   ADD TOTAL-GRADE TO WS-TOTAL-AVERAGE
                   PERFORM 2200-FETCH-GRADES
                   PERFORM 3100-GENERATE-STUDENT-REPORT
               END-IF
           END-PERFORM.
       
           EXEC SQL
               CLOSE STUDENT_CURSOR
           END-EXEC.
       
       2100-FETCH-COURSES.
           EXEC SQL
               DECLARE COURSE_CURSOR CURSOR FOR
               SELECT ID, LABEL, COEF, AVERAGE_GRADE FROM COURSE
           END-EXEC.
           EXEC SQL
               OPEN COURSE_CURSOR
           END-EXEC.
       
           PERFORM VARYING COURSE-ID FROM 1 BY 1 UNTIL COURSE-ID > 6
               EXEC SQL FETCH COURSE_CURSOR INTO
                   :COURSE-LIST(COURSE-ID)-COURSE-ID,
                   :COURSE-LIST(COURSE-ID)-COURSE-LABEL,
                   :COURSE-LIST(COURSE-ID)-COEF-COURSE,
                   :COURSE-LIST(COURSE-ID)-COURSE-AVERAGE
               END-EXEC
           END-PERFORM.
       
           EXEC SQL
               CLOSE COURSE_CURSOR
           END-EXEC.
       
       2200-FETCH-GRADES.
           EXEC SQL
               DECLARE GRADE_CURSOR CURSOR FOR
               SELECT COURSE_ID, GRADE FROM GRADE WHERE STUDENT_ID = 
                       :STUDENT-ID
           END-EXEC.
           EXEC SQL
               OPEN GRADE_CURSOR
           END-EXEC.
       
           MOVE SPACES TO COURSES-LINE
           PERFORM VARYING COURSE-ID FROM 1 BY 1 UNTIL COURSE-ID > 6
               MOVE 0 TO WS-GRADE
               EXEC SQL FETCH GRADE_CURSOR INTO :GRADE-COURSE-ID, 
                                                :GRADE-VALUE
               END-EXEC
               IF SQLCODE = 0
                   STRING GRADE-VALUE DELIMITED BY SIZE 
                       INTO COURSES-LINE (COURSE-ID:8)
               END-IF
           END-PERFORM.
       
           EXEC SQL
               CLOSE GRADE_CURSOR
           END-EXEC.
       
       3000-GENERATE-REPORT-HEADER.
           MOVE HEADER-LINE TO REPORTRECORD.
           WRITE REPORTRECORD FROM REPORTRECORD.
       
           STRING "BULLETIN DE NOTES" INTO TITLE-LINE(65:17)
           WRITE REPORTRECORD FROM TITLE-LINE.
       
           MOVE HEADER-LINE TO REPORTRECORD.
           WRITE REPORTRECORD FROM REPORTRECORD.
       
           MOVE COLUMN-HEADER TO REPORTRECORD.
           WRITE REPORTRECORD FROM REPORTRECORD.
       
           MOVE HEADER-LINE TO REPORTRECORD.
           WRITE REPORTRECORD FROM REPORTRECORD.
       
       3100-GENERATE-STUDENT-REPORT.
           MOVE SPACES TO STUDENT-REPORT-LINE
           STRING LASTNAME DELIMITED BY SIZE
                  "       "
                  FIRSTNAME DELIMITED BY SIZE
                  "       "
                  TOTAL-GRADE DELIMITED BY SIZE
                  "       "
                  COURSES-LINE DELIMITED BY SIZE
                  INTO STUDENT-REPORT-LINE
           WRITE REPORTRECORD FROM STUDENT-REPORT-LINE.
       
       4000-GENERATE-CLASS-AVERAGE.
           MOVE SPACES TO STUDENT-REPORT-LINE
           STRING "CLASSE" DELIMITED BY SIZE
                  "       "
                  WS-CLASS-AVERAGE DELIMITED BY SIZE
                  "       "
                  COURSE-LIST(1)-COURSE-AVERAGE DELIMITED BY SIZE
                  "       "
                  COURSE-LIST(2)-COURSE-AVERAGE DELIMITED BY SIZE
                  "       "
                  COURSE-LIST(3)-COURSE-AVERAGE DELIMITED BY SIZE
                  "       "
                  COURSE-LIST(4)-COURSE-AVERAGE DELIMITED BY SIZE
                  "       "
                  COURSE-LIST(5)-COURSE-AVERAGE DELIMITED BY SIZE
                  "       "
                  COURSE-LIST(6)-COURSE-AVERAGE DELIMITED BY SIZE
                  INTO STUDENT-REPORT-LINE
           WRITE REPORTRECORD FROM STUDENT-REPORT-LINE.
       
       5000-GENERATE-COURSE-DETAILS.
           PERFORM VARYING COURSE-ID FROM 1 BY 1 UNTIL COURSE-ID > 6
               MOVE SPACES TO STUDENT-REPORT-LINE
               STRING "C" COURSE-ID " => COEF: " 
                   COURSE-LIST(COURSE-ID)-COEF-COURSE
                      " LABEL: " COURSE-LIST(COURSE-ID)-COURSE-LABEL
                      INTO STUDENT-REPORT-LINE
               WRITE REPORTRECORD FROM STUDENT-REPORT-LINE
           END-PERFORM
       
           MOVE SPACES TO STUDENT-REPORT-LINE
           STRING "NOMBRE D'ELEVES => " WS-STUDENT-COUNTER
                  " NOMBRE DE COURS => 6"
                  " NOMBRE DE NOTES => " WS-STUDENT-COUNTER * 6
                  INTO STUDENT-REPORT-LINE
           WRITE REPORTRECORD FROM STUDENT-REPORT-LINE.
       
       9000-CLOSE-DB.
           EXEC SQL
               DISCONNECT ALL
           END-EXEC.
