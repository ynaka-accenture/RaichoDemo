       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MKKENIN.
      * 検針受信VBファイル生成 (テスト基盤: KENFILE 202406 先頭300件)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KENIN  ASSIGN TO 'app/data/portable/KENFILE.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KEN.
           SELECT VBOUT  ASSIGN TO 'app/data/portable/KENVB.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-VB.
       DATA DIVISION.
       FILE SECTION.
       FD  KENIN RECORD CONTAINS 128 CHARACTERS.
       COPY RCKENREC.
       FD  VBOUT RECORD IS VARYING IN SIZE FROM 9 TO 283
           CHARACTERS DEPENDING ON WS-RLEN.
       COPY RCKENIN.
       WORKING-STORAGE SECTION.
       77  ST-KEN          PIC XX VALUE '00'.
       77  ST-VB           PIC XX VALUE '00'.
       77  WS-RLEN         PIC 9(4) VALUE ZERO.
       77  WK-D-CNT        PIC 9(7) VALUE ZERO.
       77  WK-K-CNT        PIC 9(7) VALUE ZERO.
       77  WK-SUM          PIC 9(10) VALUE ZERO.
       77  WS-I            PIC 9(3) VALUE ZERO.
       77  WS-AMARI        PIC 9(5) VALUE ZERO.
       77  WS-KO           PIC 9(5) VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN INPUT KENIN.
           OPEN OUTPUT VBOUT.
           MOVE SPACES TO KIN-REC.
           MOVE '1' TO KIN-KBN.
           MOVE 'SMT01' TO KIN-H-SOSIN-CD.
           MOVE 20240620 TO KIN-H-SAKUSEI-BI.
           MOVE 300 TO KIN-H-KENSU.
           MOVE 21 TO WS-RLEN.
           WRITE KIN-REC.
       READ-LOOP.
           READ KENIN AT END GO TO OWARI.
           IF KEN-NENGETU NOT = 202406
               GO TO READ-LOOP.
           IF WK-D-CNT NOT LESS 300
               GO TO OWARI.
           MOVE SPACES TO KIN-REC.
           MOVE '2' TO KIN-KBN.
           MOVE KEN-SPT-NO TO KIN-D-SPT-NO.
           MOVE KEN-KENSHIN-BI TO KIN-D-KENSHIN-BI.
           MOVE KEN-KON-SIJISU TO KIN-D-SIJISU.
           MOVE KEN-KENSHININ TO KIN-D-KENSHININ.
           MOVE KEN-KENSHIN-KBN TO KIN-D-KBN.
           IF FUNCTION MOD (WK-D-CNT 2) = 0
               MOVE 48 TO KIN-D-SU
           ELSE
               MOVE 24 TO KIN-D-SU.
      *    30分値: 使用量を等分し端数は先頭コマへ
           COMPUTE WS-KO = KEN-SIYORYO * 10 / KIN-D-SU.
           COMPUTE WS-AMARI = KEN-SIYORYO * 10
               - WS-KO * KIN-D-SU.
           MOVE 1 TO WS-I.
       VAL-LOOP.
           IF WS-I GREATER KIN-D-SU
               GO TO VAL-END.
           MOVE WS-KO TO KIN-D-VAL (WS-I).
           IF WS-I = 1
               ADD WS-AMARI TO KIN-D-VAL (1).
           ADD 1 TO WS-I.
           GO TO VAL-LOOP.
       VAL-END.
           COMPUTE WS-RLEN = 43 + KIN-D-SU * 5.
           WRITE KIN-REC.
           ADD 1 TO WK-D-CNT.
           ADD KEN-KON-SIJISU TO WK-SUM.
      *    40件毎に計器交換レコードを挟む
           IF FUNCTION MOD (WK-D-CNT 40) = 0
               MOVE SPACES TO KIN-REC
               MOVE '3' TO KIN-KBN
               MOVE KEN-SPT-NO TO KIN-K-SPT-NO
               MOVE 'MTROLD0001' TO KIN-K-KYU-MTR
               MOVE 'MTRNEW0001' TO KIN-K-SHIN-MTR
               MOVE 20240610 TO KIN-K-KOKAN-BI
               MOVE 999900 TO KIN-K-KYU-SIJI
               MOVE 000012 TO KIN-K-SHIN-SIJI
               MOVE 63 TO WS-RLEN
               WRITE KIN-REC
               ADD 1 TO WK-K-CNT.
           GO TO READ-LOOP.
       OWARI.
           MOVE SPACES TO KIN-REC.
           MOVE '9' TO KIN-KBN.
           MOVE WK-D-CNT TO KIN-T-KENSU.
           MOVE WK-SUM TO KIN-T-SIJI-GOKEI.
           MOVE 18 TO WS-RLEN.
           WRITE KIN-REC.
           CLOSE KENIN VBOUT.
           DISPLAY 'MKKENIN D=' WK-D-CNT ' K=' WK-K-CNT
                   ' SUM=' WK-SUM.
           STOP RUN.
