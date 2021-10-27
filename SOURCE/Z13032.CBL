       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. Z13032P3.                                            00020001
      * DISPLAY STUDENT DETAILS. ACCUMULATE NUMBER OF STUDENT RECORDS.  00030001
      * ACCUMULATE STUDENT SCORES. CALCULATE AVERAGE SCORES AND DISPLAY 00040001
      * ON SUMMARY PAGE AT END OF REPORT.                               00050001
       ENVIRONMENT DIVISION.                                            00060002
       INPUT-OUTPUT SECTION.                                            00070002
       FILE-CONTROL.                                                    00080002
           SELECT UI-ALL-STUDENTS ASSIGN TO DDSTUDTL.                   00090002
           SELECT RP-ALL-STUDENTS ASSIGN TO DDSTURPT.                   00100002
       DATA DIVISION.                                                   00110002
       FILE SECTION.                                                    00120002
       FD  UI-ALL-STUDENTS                                              00130002
               RECORDING MODE IS F                                      00140002
               DATA RECORD IS UI-STUDENT-DETAILS.                       00150002
       01  UI-STUDENT-DETAILS.                                          00160002
           05  UI-USERID               PIC X(6).                        00170002
           05  UI-FIRST-NAME           PIC X(20).                       00180002
           05  UI-CLASS1.                                               00190002
               10  UI-CLASS1-NAME      PIC X(20).                       00200002
               10  UI-CLASS1-GR-DATE   PIC X(8).                        00210002
               10  UI-CLASS1-GR-PERC   PIC 9(3).                        00220002
           05  UI-CLASS2.                                               00230002
               10  UI-CLASS2-NAME      PIC X(20).                       00240002
               10  UI-CLASS2-GR-DATE   PIC X(8).                        00250002
               10  UI-CLASS2-GR-PERC   PIC 9(3).                        00260002
           05  UI-CLASS3.                                               00270002
               10  UI-CLASS3-NAME      PIC X(20).                       00280002
               10  UI-CLASS3-GR-DATE   PIC X(8).                        00290002
               10  UI-CLASS3-GR-PERC   PIC 9(3).                        00300002
           05  FILLER                  PIC X.                           00310002
       FD RP-ALL-STUDENTS                                               00320002
              RECORDING MODE IS F                                       00330002
              DATA RECORD IS REPORT-DETAILS.                            00340002
       01 REPORT-DETAILS               PIC X(133).                      00350002
       WORKING-STORAGE SECTION.                                         00360002
       01 MISC.                                                         00370002
          05 W1-CURRENT-DATE.                                           00380002
             10 W1-CURR-YEAR.                                           00390002
                15 W1-CURR-CC          PIC XX.                          00400002
                15 W1-CURR-YY          PIC XX.                          00410002
             10 W1-CURR-MONTH          PIC XX.                          00420002
             10 W1-CURR-DAY            PIC XX.                          00430002
          05 COUNTERS-SUBSCRIPTS.                                       00440002
             10 WS-REC-EOF             PIC X       VALUE SPACES.        00450002
             10 WS-PAGE-CNT            PIC 99.                          00460002
             10 WS-PRINT-LINE-CNT      PIC 99.                          00461007
             10 WS-LINE-CNT            PIC 99.                          00470002
             10 WS-STUDENT-CNT         PIC 99.                          00480002
             10 WS-TSO-CNT             PIC 99.                          00481004
             10 WS-JCL-CNT             PIC 99.                          00482004
             10 WS-COBOL-CNT           PIC 99.                          00483004
          05 DATE-CONVERSION-1.                                         00490002
             10 W1-CLASS-YEAR.                                          00500002
                15 W1-CLASS-CC          PIC XX.                         00510002
                15 W1-CLASS-YY          PIC XX.                         00520002
             10 W1-CLASS-MONTH          PIC XX.                         00530002
             10 W1-CLASS-DAY            PIC XX.                         00540002
          05 DATE-CONVERSION-2.                                         00550002
             10 W2-CLASS-YEAR.                                          00560002
                15 W2-CLASS-CC          PIC XX.                         00570002
                15 W2-CLASS-YY          PIC XX.                         00580002
             10 W2-CLASS-MONTH          PIC XX.                         00590002
             10 W2-CLASS-DAY            PIC XX.                         00600002
          05 DATE-CONVERSION-3.                                         00610002
             10 W3-CLASS-YEAR.                                          00620002
                15 W3-CLASS-CC          PIC XX.                         00630002
                15 W3-CLASS-YY          PIC XX.                         00640002
             10 W3-CLASS-MONTH          PIC XX.                         00650002
             10 W3-CLASS-DAY            PIC XX.                         00660002
          05 CLASS-AVERAGES.                                            00670002
             10 TSO-TOT                 PIC 9(04).                      00680004
             10 JCL-TOT                 PIC 9(04).                      00690004
             10 COBOL-TOT               PIC 9(04).                      00700004
             10 TSO-AVG                 PIC 9(04).                      00710004
             10 JCL-AVG                 PIC 9(04).                      00720004
             10 COBOL-AVG               PIC 9(04).                      00730004
       01 H1-HEADING.                                                   00740002
          05 FILLER                    PIC X       VALUE SPACES.        00750002
          05 FILLER                    PIC X(8)    VALUE                00760002
              'MY NAME:'.                                               00770002
          05 FILLER                    PIC X(11)   VALUE                00780002
              ' PATRICK'.                                               00790002
          05 FILLER                    PIC X(6)    VALUE SPACES.        00800002
          05 FILLER                    PIC X(10)   VALUE                00810002
              'PERSCHOLAS'.                                             00820002
          05 FILLER                    PIC X(13)   VALUE SPACES.        00830002
          05 FILLER                    PIC X(9)    VALUE                00840002
              'RUN DATE:'.                                              00850002
          05 H1-CURR-MM                PIC XX.                          00860002
          05 FILLER                    PIC X       VALUE                00870002
              '/'.                                                      00880002
          05 H1-CURR-DD                PIC XX.                          00890002
          05 FILLER                    PIC X       VALUE                00900002
              '/'.                                                      00910002
          05 H1-CURR-YY                PIC XX.                          00920002
       01 H2-HEADING.                                                   00930002
          05 FILLER                    PIC X(53)   VALUE SPACES.        00940002
          05 FILLER                    PIC X(5)    VALUE                00950002
              'PAGE:'.                                                  00960002
          05 H2-PAGE-NUM               PIC 99.                          00970002
       01 H3-HEADING.                                                   00980002
          05 FILLER                    PIC X(4)    VALUE SPACES.        00990002
          05 FILLER                    PIC X(7)    VALUE                01000002
                'STUDENT'.                                              01010002
          05 FILLER                    PIC X(18)   VALUE SPACES.        01020002
          05 FILLER                    PIC X(5)    VALUE                01030002
                'CLASS'.                                                01040002
          05 FILLER                    PIC X(18)   VALUE SPACES.        01050002
          05 FILLER                    PIC X(5)    VALUE                01060002
                'CLASS'.                                                01070002
          05 FILLER                    PIC X(3)    VALUE SPACES.        01080002
          05 FILLER                    PIC X(5)    VALUE                01090002
                'CLASS'.                                                01100002
       01 H4-HEADING.                                                   01110002
           05 FILLER                   PIC X(3)    VALUE SPACES.        01120002
           05 FILLER                   PIC X(2)    VALUE                01130002
                 'ID'.                                                  01140002
           05 FILLER                   PIC X(5)    VALUE SPACES.        01150002
           05 FILLER                   PIC X(4)    VALUE                01160002
                 'NAME'.                                                01170002
           05 FILLER                   PIC X(16)   VALUE SPACES.        01180002
           05 FILLER                   PIC X(4)    VALUE                01190002
                 'NAME'.                                                01200002
           05 FILLER                   PIC X(19)   VALUE SPACES.        01210002
           05 FILLER                   PIC X(4)    VALUE                01220002
                 'DATE'.                                                01230002
           05 FILLER                   PIC X(4)    VALUE SPACES.        01240002
           05 FILLER                   PIC X(4)    VALUE                01250002
                 'PERC'.                                                01260002
       01 B1-BODY.                                                      01270002
           05 FILLER                   PIC XX      VALUE SPACES.        01280002
           05 STU-ID                   PIC X(6).                        01290002
           05 FILLER                   PIC X(1)    VALUE SPACES.        01300002
           05 STU-NAME                 PIC X(20).                       01310002
       01 B2-BODY.                                                      01320002
           05 FILLER                   PIC X(29)   VALUE SPACES.        01330002
           05 B2-CLASS                 PIC X(20).                       01340002
           05 FILLER                   PIC X(1)    VALUE SPACES.        01350002
           05 B2-CLASS-MM              PIC XX.                          01360002
           05 FILLER                   PIC X       VALUE                01370002
               '/'.                                                     01380002
           05 B2-CLASS-DD              PIC XX.                          01390002
           05 FILLER                   PIC X       VALUE                01400002
               '/'.                                                     01410002
           05 B2-CLASS-YY              PIC XX.                          01420002
           05 FILLER                   PIC X(1)    VALUE SPACES.        01430002
           05 B2-PERC                  PIC ZZZ.                         01440002
           05 FILLER                   PIC X       VALUE                01450002
               '%'.                                                     01460002
       01 B3-BODY.                                                      01470002
           05 FILLER                   PIC X(29)   VALUE SPACES.        01480002
           05 B3-CLASS                 PIC X(20).                       01490002
           05 FILLER                   PIC X(1)    VALUE SPACES.        01500002
           05 B3-CLASS-MM              PIC XX.                          01510002
           05 FILLER                   PIC X       VALUE                01520002
               '/'.                                                     01530002
           05 B3-CLASS-DD              PIC XX.                          01540002
           05 FILLER                   PIC X       VALUE                01550002
               '/'.                                                     01560002
           05 B3-CLASS-YY              PIC XX.                          01570002
           05 FILLER                   PIC X(1)    VALUE SPACES.        01580002
           05 B3-PERC                  PIC ZZZ.                         01590002
           05 FILLER                   PIC X       VALUE                01600002
               '%'.                                                     01610002
       01 B4-BODY.                                                      01620002
           05 FILLER                   PIC X(29)   VALUE SPACES.        01630002
           05 B4-CLASS                 PIC X(20).                       01640002
           05 FILLER                   PIC X(1)    VALUE SPACES.        01650002
           05 B4-CLASS-MM              PIC XX.                          01660002
           05 FILLER                   PIC X       VALUE                01670002
               '/'.                                                     01680002
           05 B4-CLASS-DD              PIC XX.                          01690002
           05 FILLER                   PIC X       VALUE                01700002
               '/'.                                                     01710002
           05 B4-CLASS-YY              PIC XX.                          01720002
           05 FILLER                   PIC X(1)    VALUE SPACES.        01730002
           05 B4-PERC                  PIC ZZZ.                         01740002
           05 FILLER                   PIC X       VALUE                01750002
               '%'.                                                     01760002
       01 R-1-REPORT-SUMMARY.                                           01770002
           05 FILLER                    PIC X(2)    VALUE SPACES.       01780002
           05 FILLER                    PIC X(5)    VALUE               01790002
                'CLASS'.                                                01800002
           05 FILLER                    PIC X       VALUE SPACES.       01810002
           05 FILLER                    PIC X(9)    VALUE               01820002
                'AVERAGES:'.                                            01830002
       01 R-2-REPORT-SUMMARY-A.                                         01840004
           05 FILLER                    PIC X(6)    VALUE SPACES.       01850002
           05 FILLER                    PIC X(5)    VALUE               01860002
                'CLASS'.                                                01870002
           05 FILLER                    PIC X(13)   VALUE SPACES.       01880002
           05 FILLER                    PIC X       VALUE               01890002
                ':'.                                                    01900002
           05 FILLER                    PIC X       VALUE SPACES.       01910002
           05 FILLER                    PIC X(03)   VALUE               01911002
                'TSO'.                                                  01912002
       01 R-2-REPORT-SUMMARY-B.                                         01913004
           05 FILLER                    PIC X(6)    VALUE SPACES.       01914004
           05 FILLER                    PIC X(19)   VALUE               01915004
                'NUMBER OF STUDENTS:'.                                  01916004
           05 FILLER                    PIC X       VALUE SPACES.       01917004
           05 R-2-STUDENT-CNT           PIC ZZ.                         01918004
       01 R-2-REPORT-SUMMARY-C.                                         01923004
           05 FILLER                    PIC X(6)    VALUE SPACES.       01924004
           05 FILLER                    PIC X(19)   VALUE               01925004
                'AVERAGE PERCENTAGE:'.                                  01926004
           05 FILLER                    PIC X       VALUE SPACES.       01927004
           05 R-2-STUDENT-AVG           PIC ZZZ.                        01928004
           05 FILLER                    PIC X       VALUE               01929004
                '%'.                                                    01929104
      ****************************************************************  01929204
       01 R-3-REPORT-SUMMARY-A.                                         01990004
           05 FILLER                    PIC X(6)    VALUE SPACES.       02000004
           05 FILLER                    PIC X(5)    VALUE               02010004
                'CLASS'.                                                02020004
           05 FILLER                    PIC X(13)   VALUE SPACES.       02030004
           05 FILLER                    PIC X       VALUE               02040004
                ':'.                                                    02050004
           05 FILLER                    PIC X       VALUE SPACES.       02060004
           05 FILLER                    PIC X(03)   VALUE               02070004
                'JCL'.                                                  02080004
       01 R-3-REPORT-SUMMARY-B.                                         02090004
           05 FILLER                    PIC X(6)    VALUE SPACES.       02100004
           05 FILLER                    PIC X(19)   VALUE               02110004
                'NUMBER OF STUDENTS:'.                                  02120004
           05 FILLER                    PIC X       VALUE SPACES.       02130004
           05 R-3-STUDENT-CNT           PIC ZZ.                         02140004
       01 R-3-REPORT-SUMMARY-C.                                         02141004
           05 FILLER                    PIC X(6)    VALUE SPACES.       02142004
           05 FILLER                    PIC X(19)   VALUE               02143004
                'AVERAGE PERCENTAGE:'.                                  02144004
           05 FILLER                    PIC X       VALUE SPACES.       02145004
           05 R-3-STUDENT-AVG           PIC ZZZ.                        02146004
           05 FILLER                    PIC X       VALUE               02147004
                '%'.                                                    02148004
      **************************************************************    02149004
       01 R-4-REPORT-SUMMARY-A.                                         02150004
           05 FILLER                    PIC X(6)    VALUE SPACES.       02160004
           05 FILLER                    PIC X(5)    VALUE               02170004
                'CLASS'.                                                02180004
           05 FILLER                    PIC X(13)   VALUE SPACES.       02190004
           05 FILLER                    PIC X       VALUE               02200004
                ':'.                                                    02210004
           05 FILLER                    PIC X       VALUE SPACES.       02220004
           05 FILLER                    PIC X(05)   VALUE               02230004
                'COBOL'.                                                02240004
       01 R-4-REPORT-SUMMARY-B.                                         02330004
           05 FILLER                    PIC X(6)    VALUE SPACES.       02340004
           05 FILLER                    PIC X(19)   VALUE               02350004
                'NUMBER OF STUDENTS:'.                                  02360004
           05 FILLER                    PIC X       VALUE SPACES.       02370004
           05 R-4-STUDENT-CNT           PIC ZZ.                         02380004
       01 R-4-REPORT-SUMMARY-C.                                         02490004
           05 FILLER                    PIC X(6)    VALUE SPACES.       02500004
           05 FILLER                    PIC X(19)   VALUE               02510004
                'AVERAGE PERCENTAGE:'.                                  02520004
           05 FILLER                    PIC X       VALUE SPACES.       02521004
           05 R-4-STUDENT-AVG           PIC ZZZ.                        02522004
           05 FILLER                    PIC X       VALUE               02523004
                '%'.                                                    02524004
       01 PAGE-BREAK.                                                   02525005
           05 PAGE-MARKER-1           PIC X(40)  VALUE                  02525105
                '****************************************'.             02526005
           05 PAGE-MARKER-2           PIC X(40)  VALUE                  02527005
                '****************************************'.             02528005
       01 PAGE-BREAK-SUM.                                               02529006
           05 PAGE-MARKER-1           PIC X(40)  VALUE                  02529106
                '************************************SUMM'.             02529206
           05 PAGE-MARKER-2           PIC X(40)  VALUE                  02529306
                'ARY*************************************'.             02529406
       PROCEDURE DIVISION.                                              02530002
       MAIN-CONTROL.                                                    02540002
           PERFORM A-INITIAL.                                           02550002
           PERFORM B-CONTROL.                                           02560002
           PERFORM C-END.                                               02570002
       A-INITIAL.                                                       02580002
           MOVE FUNCTION CURRENT-DATE TO W1-CURRENT-DATE.               02590002
           MOVE W1-CURR-MONTH         TO H1-CURR-MM.                    02600002
           MOVE W1-CURR-DAY           TO H1-CURR-DD.                    02610002
           MOVE W1-CURR-YY            TO H1-CURR-YY.                    02620002
           MOVE 0                     TO WS-STUDENT-CNT.                02630004
           MOVE 0                     TO WS-PAGE-CNT.                   02640002
           MOVE 0                     TO TSO-TOT.                       02641004
           MOVE 0                     TO JCL-TOT.                       02642004
           MOVE 0                     TO COBOL-TOT.                     02643004
           MOVE 0                     TO WS-TSO-CNT.                    02644004
           MOVE 0                     TO WS-JCL-CNT.                    02645004
           MOVE 0                     TO WS-COBOL-CNT.                  02646004
           MOVE 70                    TO WS-LINE-CNT.                   02650002
           MOVE 6                     TO WS-PRINT-LINE-CNT.             02651007
           MOVE WS-PAGE-CNT           TO H2-PAGE-NUM.                   02660002
           OPEN INPUT UI-ALL-STUDENTS.                                  02670002
           OPEN OUTPUT RP-ALL-STUDENTS.                                 02680002
           READ UI-ALL-STUDENTS                                         02681007
               AT END                                                   02682007
                   MOVE "Y" TO WS-REC-EOF                               02683007
           END-READ.                                                    02684007
       B-CONTROL.                                                       02690002
           PERFORM B-READ UNTIL WS-REC-EOF = "Y".                       02700002
       B-READ.                                                          02710002
           PERFORM B-CLASS-DATE-CONVERTER.                              02720007
           PERFORM B-DATA-MIGRATION.                                    02760002
           PERFORM B-WRITE.                                             02790002
           READ UI-ALL-STUDENTS                                         02791007
               AT END                                                   02792007
                   MOVE "Y" TO WS-REC-EOF                               02793007
           END-READ.                                                    02794007
       B-CLASS-DATE-CONVERTER.                                          02800002
           MOVE UI-CLASS1-GR-DATE TO DATE-CONVERSION-1.                 02810002
           MOVE UI-CLASS2-GR-DATE TO DATE-CONVERSION-2.                 02820002
           MOVE UI-CLASS3-GR-DATE TO DATE-CONVERSION-3.                 02830002
       B-DATA-MIGRATION.                                                02840002
           MOVE UI-USERID         TO STU-ID.                            02850002
           MOVE UI-FIRST-NAME     TO STU-NAME.                          02860002
      *                                                                 02870002
           MOVE UI-CLASS1         TO B2-CLASS.                          02880002
           MOVE W1-CLASS-MONTH    TO B2-CLASS-MM.                       02890002
           MOVE W1-CLASS-DAY      TO B2-CLASS-DD.                       02900002
           MOVE W1-CLASS-YY       TO B2-CLASS-YY.                       02910002
           MOVE UI-CLASS1-GR-PERC TO B2-PERC.                           02920002
      *                                                                 02930002
           MOVE UI-CLASS2         TO B3-CLASS.                          02940002
           MOVE W2-CLASS-MONTH    TO B3-CLASS-MM.                       02950002
           MOVE W2-CLASS-DAY      TO B3-CLASS-DD.                       02960002
           MOVE W2-CLASS-YY       TO B3-CLASS-YY.                       02970002
           MOVE UI-CLASS2-GR-PERC TO B3-PERC.                           02980002
      *                                                                 02990002
           MOVE UI-CLASS3         TO B4-CLASS.                          03000002
           MOVE W3-CLASS-MONTH    TO B4-CLASS-MM.                       03010002
           MOVE W3-CLASS-DAY      TO B4-CLASS-DD.                       03020002
           MOVE W3-CLASS-YY       TO B4-CLASS-YY.                       03030002
           MOVE UI-CLASS3-GR-PERC TO B4-PERC.                           03040002
       B-AVG-DATA-MIGRATION.                                            03050002
           MOVE WS-TSO-CNT        TO R-2-STUDENT-CNT.                   03051004
           MOVE WS-JCL-CNT        TO R-3-STUDENT-CNT.                   03052005
           MOVE WS-COBOL-CNT      TO R-4-STUDENT-CNT.                   03053005
           MOVE TSO-AVG           TO R-2-STUDENT-AVG                    03060004
           MOVE JCL-AVG           TO R-3-STUDENT-AVG                    03070004
           MOVE COBOL-AVG         TO R-4-STUDENT-AVG.                   03080004
       B-WRITE.                                                         03090002
           EVALUATE TRUE                                                03130002
               WHEN WS-LINE-CNT > 65                                    03140002
                   ADD 1 TO WS-PAGE-CNT.                                03150002
           EVALUATE TRUE                                                03160002
               WHEN WS-LINE-CNT > 65                                    03170002
                   MOVE WS-PAGE-CNT TO H2-PAGE-NUM.                     03180002
           EVALUATE TRUE                                                03190002
               WHEN WS-LINE-CNT > 65                                    03200002
                   PERFORM Z-HEADINGS.                                  03210002
           EVALUATE TRUE                                                03220002
               WHEN WS-LINE-CNT > 65                                    03230002
                  MOVE 6 TO WS-LINE-CNT.                                03240002
           PERFORM B-CLASS-PERC-SORTER.                                 03240104
           ADD 1 TO WS-STUDENT-CNT.                                     03241004
           PERFORM Z-CLASS-RECORD.                                      03242006
       B-LINE-CNTR.                                                     03260002
           ADD 1 TO WS-LINE-CNT.                                        03270002
       B-CLASS-PERC-SORTER.                                             03330002
           EVALUATE TRUE                                                03340002
               WHEN UI-CLASS1-NAME = "TSO"                              03350004
                 ADD 1 TO WS-TSO-CNT                                    03351004
                 COMPUTE TSO-TOT = UI-CLASS1-GR-PERC + TSO-TOT          03360004
               WHEN UI-CLASS1-NAME = "JCL"                              03370004
                 COMPUTE JCL-TOT = UI-CLASS1-GR-PERC + JCL-TOT          03380004
                 ADD 1 TO WS-JCL-CNT                                    03381004
               WHEN UI-CLASS1-NAME = "COBOL"                            03390004
                 COMPUTE COBOL-TOT = UI-CLASS1-GR-PERC + COBOL-TOT      03400004
                 ADD 1 TO WS-COBOL-CNT                                  03401004
           END-EVALUATE.                                                03410002
           EVALUATE TRUE                                                03411004
               WHEN UI-CLASS2-NAME = "TSO"                              03412004
                 COMPUTE TSO-TOT = UI-CLASS2-GR-PERC + TSO-TOT          03413004
                 ADD 1 TO WS-TSO-CNT                                    03413104
               WHEN UI-CLASS2-NAME = "JCL"                              03414004
                 COMPUTE JCL-TOT = UI-CLASS2-GR-PERC + JCL-TOT          03415004
                 ADD 1 TO WS-JCL-CNT                                    03415104
               WHEN UI-CLASS2-NAME = "COBOL"                            03416004
                 COMPUTE COBOL-TOT = UI-CLASS2-GR-PERC + COBOL-TOT      03417004
                 ADD 1 TO WS-COBOL-CNT                                  03417104
           END-EVALUATE.                                                03418004
           EVALUATE TRUE                                                03419004
               WHEN UI-CLASS3-NAME = "TSO"                              03419104
                 ADD 1 TO WS-TSO-CNT                                    03419204
                 COMPUTE TSO-TOT = UI-CLASS3-GR-PERC + TSO-TOT          03419304
               WHEN UI-CLASS3-NAME = "JCL"                              03419404
                 COMPUTE JCL-TOT = UI-CLASS3-GR-PERC + JCL-TOT          03419504
                 ADD 1 TO WS-JCL-CNT                                    03419604
               WHEN UI-CLASS3-NAME = "COBOL"                            03419704
                 COMPUTE COBOL-TOT = UI-CLASS3-GR-PERC + COBOL-TOT      03419804
                 ADD 1 TO WS-COBOL-CNT                                  03419904
           END-EVALUATE.                                                03420004
       B-CLASS-AVG-CALC.                                                03420602
           COMPUTE TSO-AVG ROUNDED = TSO-TOT / WS-TSO-CNT.              03421004
           COMPUTE JCL-AVG ROUNDED = JCL-TOT / WS-JCL-CNT.              03430104
           COMPUTE COBOL-AVG ROUNDED = COBOL-TOT / WS-COBOL-CNT.        03431004
           PERFORM B-AVG-DATA-MIGRATION.                                03460002
       B-CLASS-SUMMARY.                                                 03461005
           PERFORM B-CLASS-AVG-CALC.                                    03462005
           ADD 1 TO WS-PAGE-CNT.                                        03462106
           MOVE WS-PAGE-CNT TO H2-PAGE-NUM.                             03462206
           PERFORM Z-HEADINGS.                                          03462306
           PERFORM Z-AVERAGE-SCORES.                                    03463005
       C-END.                                                           03470002
           PERFORM B-CLASS-SUMMARY.                                     03480006
           CLOSE UI-ALL-STUDENTS                                        03520002
                 RP-ALL-STUDENTS.                                       03530002
           STOP RUN.                                                    03540002
       Z-HEADINGS.                                                      03550002
           WRITE REPORT-DETAILS FROM PAGE-BREAK                         03550107
           WRITE REPORT-DETAILS FROM H1-HEADING                         03550207
               AFTER ADVANCING PAGE.                                    03551005
           WRITE REPORT-DETAILS FROM H2-HEADING.                        03580005
           EVALUATE TRUE                                                03580107
               WHEN WS-REC-EOF = "Y"                                    03580207
                   PERFORM Z-HEADINGS-3                                 03580307
               WHEN OTHER                                               03580407
                   PERFORM Z-HEADINGS-2                                 03580507
           END-EVALUATE.                                                03580607
       Z-HEADINGS-2.                                                    03580707
           WRITE REPORT-DETAILS FROM H3-HEADING                         03590007
           AFTER ADVANCING 4 LINES.                                     03591007
           WRITE REPORT-DETAILS FROM H4-HEADING.                        03600002
           PERFORM B-LINE-CNTR 6 TIMES.                                 03610007
       Z-HEADINGS-3.                                                    03611007
           WRITE REPORT-DETAILS FROM  PAGE-BREAK-SUM                    03612007
           AFTER ADVANCING 4 LINES.                                     03613007
       Z-CLASS-RECORD.                                                  03620002
           WRITE REPORT-DETAILS FROM  B1-BODY                           03640007
           AFTER ADVANCING 6 LINES.                                     03650007
           WRITE REPORT-DETAILS FROM  B2-BODY.                          03660002
           WRITE REPORT-DETAILS FROM  B3-BODY.                          03670002
           WRITE REPORT-DETAILS FROM  B4-BODY.                          03680002
           ADD 5 TO WS-PRINT-LINE-CNT.                                  03681007
           PERFORM B-LINE-CNTR 5 TIMES.                                 03690002
       Z-AVERAGE-SCORES.                                                03700002
           WRITE REPORT-DETAILS FROM  R-1-REPORT-SUMMARY                03710002
           WRITE REPORT-DETAILS FROM  R-2-REPORT-SUMMARY-A              03720002
           WRITE REPORT-DETAILS FROM  R-2-REPORT-SUMMARY-B              03730002
           WRITE REPORT-DETAILS FROM  R-2-REPORT-SUMMARY-C              03740002
           WRITE REPORT-DETAILS FROM  R-3-REPORT-SUMMARY-A              03750002
           WRITE REPORT-DETAILS FROM  R-3-REPORT-SUMMARY-B              03760002
           WRITE REPORT-DETAILS FROM  R-3-REPORT-SUMMARY-C              03770002
           WRITE REPORT-DETAILS FROM  R-4-REPORT-SUMMARY-A              03780002
           WRITE REPORT-DETAILS FROM  R-4-REPORT-SUMMARY-B              03790002
           WRITE REPORT-DETAILS FROM  R-4-REPORT-SUMMARY-C.             03800002
