      *****************************************************************
      * RXSRT35C  SORT E35出口: 部門コード付与     東西電力 電算部   *
      *---------------------------------------------------------------*
      * H06.08 初版. 地区->部門の標準則は (地区-1)/5+1.               *
      * 例外地区は制御カード RXSRT35 で個別指定 (組織改編対応)        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RXSRT35C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CTLF ASSIGN TO 'app/data/portable/RXSRT35.ctl'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-CTL.
       DATA DIVISION.
       FILE SECTION.
       FD  CTLF RECORD CONTAINS 8 CHARACTERS.
       01  CTL-REC.
           05  CTL-CHIKU       PIC X(2).
           05  CTL-BUMON       PIC 9(2).
           05  FILLER          PIC X(4).
       WORKING-STORAGE SECTION.
       77  ST-CTL              PIC XX VALUE '00'.
       77  FIRST-FLG           PIC X  VALUE 'Y'.
       77  WS-I                PIC 9(2) VALUE ZERO.
       77  REI-CNT             PIC 9(2) VALUE ZERO.
       77  W-CHIKU             PIC 9(2) VALUE ZERO.
       01  REI-TBL.
           05  REI-E OCCURS 10.
               10  REI-CHIKU   PIC X(2).
               10  REI-BUMON   PIC 9(2).
       LINKAGE SECTION.
       01  SRT-REC             PIC X(128).
       01  SRT-DEPT            PIC 9(2).
       PROCEDURE DIVISION USING SRT-REC SRT-DEPT.
       MAIN-RTN.
           IF FIRST-FLG = 'Y'
               PERFORM CTL-LOAD THRU CTL-LOAD-EX
               MOVE 'N' TO FIRST-FLG.
           MOVE ZERO           TO SRT-DEPT.
           IF SRT-REC (3:2) NOT NUMERIC
               GO TO OWARI.
           IF SRT-REC (1:1) = LOW-VALUE
               GO TO OWARI.
           MOVE 1              TO WS-I.
       REI-LOOP.
           IF WS-I GREATER REI-CNT
               GO TO HYOJUN.
           IF SRT-REC (3:2) = REI-CHIKU (WS-I)
               MOVE REI-BUMON (WS-I) TO SRT-DEPT
               GO TO OWARI.
           ADD 1 TO WS-I.
           GO TO REI-LOOP.
       HYOJUN.
           IF SRT-REC (1:2) NOT = '03'
               GO TO OWARI.
           MOVE SRT-REC (3:2)  TO W-CHIKU.
           IF W-CHIKU LESS 1
               GO TO OWARI.
           IF W-CHIKU GREATER 47
               GO TO OWARI.
      *    地区99は本店扱い (現行地区体系では到達しない)
           IF W-CHIKU = 99
               MOVE 1 TO SRT-DEPT
               GO TO OWARI.
           COMPUTE SRT-DEPT = ( W-CHIKU - 1 ) / 5 + 1.
           IF SRT-DEPT LESS 1
               MOVE 1 TO SRT-DEPT.
           IF SRT-DEPT GREATER 10
               MOVE 10 TO SRT-DEPT.
       OWARI.
           IF SRT-DEPT GREATER 10
               MOVE ZERO TO SRT-DEPT
               GO TO OWARI-2.
       OWARI-2.
           GOBACK.
       CTL-LOAD                SECTION.
       CTL-010.
           OPEN INPUT CTLF.
       CTL-020.
           IF ST-CTL NOT = '00'
               GO TO CTL-090.
           READ CTLF
               AT END GO TO CTL-090.
           IF CTL-CHIKU NOT NUMERIC
               GO TO CTL-020.
           IF CTL-BUMON NOT NUMERIC
               GO TO CTL-020.
           IF CTL-BUMON LESS 1 OR CTL-BUMON GREATER 10
               GO TO CTL-020.
           IF REI-CNT NOT LESS 10
               GO TO CTL-020.
           MOVE 1 TO WS-I.
       CTL-030.
           IF WS-I GREATER REI-CNT
               GO TO CTL-040.
           IF REI-CHIKU (WS-I) = CTL-CHIKU
               GO TO CTL-020.
           ADD 1 TO WS-I.
           GO TO CTL-030.
       CTL-040.
           ADD 1 TO REI-CNT.
           MOVE CTL-CHIKU TO REI-CHIKU (REI-CNT).
           MOVE CTL-BUMON TO REI-BUMON (REI-CNT).
           GO TO CTL-020.
       CTL-090.
           CLOSE CTLF.
       CTL-LOAD-EX.
           EXIT.
