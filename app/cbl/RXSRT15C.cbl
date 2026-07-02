      *****************************************************************
      * RXSRT15C  SORT E15出口: 検針レコード選別   東西電力 電算部   *
      *---------------------------------------------------------------*
      * H06.08 初版. 除外条件は制御カード RXSRT15 で指定              *
      * 復帰: 0=採用 4=削除                                           *
      * 注意: 除外条件は業務仕様 (テスト局・撤去地区の除外).          *
      *       JCL の MODS からしか呼ばれないため設計書に記載なし      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RXSRT15C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CTLF ASSIGN TO 'app/data/portable/RXSRT15.ctl'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-CTL.
       DATA DIVISION.
       FILE SECTION.
       FD  CTLF RECORD CONTAINS 8 CHARACTERS.
       01  CTL-REC.
           05  CTL-TYPE        PIC X(3).
           05  CTL-VAL         PIC X(5).
       WORKING-STORAGE SECTION.
       77  ST-CTL              PIC XX VALUE '00'.
       77  FIRST-FLG           PIC X  VALUE 'Y'.
       77  WS-I                PIC 9(2) VALUE ZERO.
       77  KBN-CNT             PIC 9(2) VALUE ZERO.
       77  CHI-CNT             PIC 9(2) VALUE ZERO.
       01  KBN-TBL.
           05  KBN-V           PIC X OCCURS 10.
       01  CHI-TBL.
           05  CHI-V           PIC X(2) OCCURS 10.
       LINKAGE SECTION.
       01  SRT-REC             PIC X(128).
       01  SRT-REC-R REDEFINES SRT-REC.
           05  SR-SPT          PIC X(22).
           05  SR-NENGETU      PIC 9(6).
           05  SR-KENSHIN-BI   PIC 9(6).
           05  FILLER          PIC X(21).
           05  SR-KBN          PIC X(1).
           05  FILLER          PIC X(72).
       01  SRT-RC              PIC 9.
       PROCEDURE DIVISION USING SRT-REC SRT-RC.
       MAIN-RTN.
           IF FIRST-FLG = 'Y'
               PERFORM CTL-LOAD THRU CTL-LOAD-EX
               MOVE 'N' TO FIRST-FLG.
           MOVE 0              TO SRT-RC.
           IF SR-SPT (1:2) NOT = '03'
               GO TO SAKUJO.
           IF SR-NENGETU NOT NUMERIC
               GO TO SAKUJO.
           MOVE 1              TO WS-I.
       KBN-LOOP.
           IF WS-I GREATER KBN-CNT
               GO TO CHI-CHK.
           IF SR-KBN = KBN-V (WS-I)
               GO TO SAKUJO.
           ADD 1 TO WS-I.
           GO TO KBN-LOOP.
       CHI-CHK.
           MOVE 1              TO WS-I.
       CHI-LOOP.
           IF WS-I GREATER CHI-CNT
               GO TO OWARI.
           IF SR-SPT (3:2) = CHI-V (WS-I)
               GO TO SAKUJO.
           ADD 1 TO WS-I.
           GO TO CHI-LOOP.
       SAKUJO.
           MOVE 4              TO SRT-RC.
           GO TO OWARI.
       OWARI.
           IF SRT-RC NOT = 0 AND SRT-RC NOT = 4
               MOVE 0 TO SRT-RC.
           GOBACK.
       CTL-LOAD                SECTION.
       CTL-010.
           OPEN INPUT CTLF.
       CTL-020.
           IF ST-CTL NOT = '00'
               GO TO CTL-090.
           READ CTLF
               AT END GO TO CTL-090.
           IF CTL-TYPE = 'KBN'
               IF KBN-CNT LESS 10
                   ADD 1 TO KBN-CNT
                   MOVE CTL-VAL (1:1) TO KBN-V (KBN-CNT)
               END-IF
           END-IF
           IF CTL-TYPE = 'CHI'
               IF CHI-CNT LESS 10
                   ADD 1 TO CHI-CNT
                   MOVE CTL-VAL (1:2) TO CHI-V (CHI-CNT)
               END-IF
           END-IF
           GO TO CTL-020.
       CTL-090.
           CLOSE CTLF.
       CTL-LOAD-EX.
           EXIT.
