      *****************************************************************
      * RBBKU01C  契約マスタ退避 (日次D090)    東西電力 電算部        *
      *---------------------------------------------------------------*
      * S63.03 初版. 出力は世代データセット RACS.KYKBKUP(+1) (D-04)   *
      * 入出力エラーは宣言部で記録のみ (夜間無人運転のため)           *
      * 制御レコード (件数+検査和) を別ファイルへ書き 復元時に照合    *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBBKU01C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KYKIN   ASSIGN TO 'app/data/portable/KYKMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-IN.
           SELECT BKOUT   ASSIGN TO 'app/data/portable/KYKBKUP.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-BK.
           SELECT CTLOUT  ASSIGN TO 'app/data/portable/KYKBKCTL.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-CT.
       DATA DIVISION.
       FILE SECTION.
       FD  KYKIN RECORD CONTAINS 320 CHARACTERS.
       01  IN-REC              PIC X(320).
       01  IN-REC-R REDEFINES IN-REC.
           05  IN-SPT          PIC X(22).
           05  IN-JYU          PIC 9(10).
           05  IN-SYU          PIC X(2).
           05  FILLER          PIC X(8).
           05  IN-KAISI        PIC 9(8).
           05  IN-SYURYO       PIC 9(8).
           05  IN-TEISI        PIC X(1).
           05  FILLER          PIC X(261).
       FD  BKOUT RECORD CONTAINS 320 CHARACTERS.
       01  BK-REC              PIC X(320).
       FD  CTLOUT RECORD CONTAINS 40 CHARACTERS.
       01  CT-REC.
           05  CT-KENSU        PIC 9(7).
           05  CT-KENSAWA      PIC 9(13).
           05  CT-NG           PIC 9(7).
           05  FILLER          PIC X(13).
       WORKING-STORAGE SECTION.
       77  ST-IN               PIC XX VALUE '00'.
       77  ST-BK               PIC XX VALUE '00'.
       77  ST-CT               PIC XX VALUE '00'.
       77  WK-IN-CNT           PIC 9(7)  VALUE ZERO.
       77  WK-OUT-CNT          PIC 9(7)  VALUE ZERO.
       77  WK-NG-CNT           PIC 9(7)  VALUE ZERO.
       77  WK-IOERR            PIC 9(7)  VALUE ZERO.
       77  WK-LOWV             PIC 9(7)  VALUE ZERO.
       77  KENSAWA             PIC 9(13) VALUE ZERO.
       77  SETU-CNT            PIC 9(5)  VALUE ZERO.
       77  WS-I                PIC 9(3)  VALUE ZERO.
       77  WS-CHIKU            PIC 9(2)  VALUE ZERO.
       01  CHIKU-BK-TBL.
           05  CHIKU-BK        PIC 9(5) OCCURS 47 VALUE ZERO.
       01  SYU-CNT-TBL.
           05  SYU-CNT         PIC 9(7) OCCURS 4 VALUE ZERO.
       PROCEDURE DIVISION.
       DECLARATIVES.
       IN-ERR-SEC              SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON KYKIN.
       IN-ERR-P.
      *    夜間無人のため記録のみで続行 (S63 運用判断)
           ADD 1               TO WK-IOERR.
           DISPLAY 'RBBKU01C W201 KYKIN ST=' ST-IN.
       END DECLARATIVES.
       MAIN-SEC                SECTION.
       MAIN-000.
           OPEN INPUT KYKIN.
           OPEN OUTPUT BKOUT.
           OPEN OUTPUT CTLOUT.
       MAIN-LOOP.
           READ KYKIN
               AT END GO TO SYUKEI-RTN.
           ADD 1               TO WK-IN-CNT.
           PERFORM IN-VALID    THRU IN-VALID-EX.
           PERFORM KENSAWA-KAS THRU KENSAWA-KAS-EX.
           PERFORM SYU-KEISU   THRU SYU-KEISU-EX.
           PERFORM CHIKU-KEISU THRU CHIKU-KEISU-EX.
           IF IN-KAISI NOT LESS 19930101
               PERFORM SHIN-KENSA THRU SHIN-KENSA-EX.
           MOVE IN-REC         TO BK-REC.
           WRITE BK-REC.
           ADD 1               TO WK-OUT-CNT.
           ADD 1               TO SETU-CNT.
           IF SETU-CNT NOT LESS 1000
               MOVE ZERO       TO SETU-CNT
               GO TO MAIN-LOOP.
           GO TO MAIN-LOOP.
       SYUKEI-RTN.
           IF WK-IN-CNT NOT = WK-OUT-CNT
               DISPLAY 'RBBKU01C E301 KENSU FUICCHI'
               GO TO SYUKEI-2.
           IF WK-IOERR GREATER ZERO
               DISPLAY 'RBBKU01C W302 IOERR ARI'
               GO TO SYUKEI-2.
       SYUKEI-2.
           MOVE WK-OUT-CNT     TO CT-KENSU.
           MOVE KENSAWA        TO CT-KENSAWA.
           MOVE WK-NG-CNT      TO CT-NG.
           WRITE CT-REC.
           CLOSE KYKIN BKOUT CTLOUT.
           DISPLAY 'RBBKU01C IN=' WK-IN-CNT ' OUT=' WK-OUT-CNT
                   ' NG=' WK-NG-CNT ' IOERR=' WK-IOERR.
           DISPLAY 'KENSAWA=' KENSAWA ' LOWV=' WK-LOWV
                   ' SYU=' SYU-CNT (1) '/' SYU-CNT (2) '/'
                   SYU-CNT (3) '/' SYU-CNT (4).
           MOVE 0              TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
      * 検査和: 需要家番号の総和 (復元時照合用. S63 方式)
      *   桁あふれ時は 10^13 を引いて桁繰り (剰余方式)
      *****************************************************************
       KENSAWA-KAS             SECTION.
       KWK-010.
           IF IN-SPT (1:1) = LOW-VALUE
               ADD 1 TO WK-LOWV
               GO TO KENSAWA-KAS-EX.
           IF IN-JYU NOT NUMERIC
               GO TO KENSAWA-KAS-EX.
           ADD IN-JYU          TO KENSAWA.
       KWK-020.
           IF KENSAWA LESS 9000000000000
               GO TO KENSAWA-KAS-EX.
           SUBTRACT 9000000000000 FROM KENSAWA.
           GO TO KWK-020.
       KENSAWA-KAS-EX.
           EXIT.
      *****************************************************************
       SYU-KEISU               SECTION.
       SYK-010.
           IF IN-SYU = '10'
               ADD 1 TO SYU-CNT (1)
               GO TO SYU-KEISU-EX.
           IF IN-SYU = '11'
               ADD 1 TO SYU-CNT (2)
               GO TO SYU-KEISU-EX.
           IF IN-SYU = '20'
               ADD 1 TO SYU-CNT (3)
               GO TO SYU-KEISU-EX.
           ADD 1               TO SYU-CNT (4).
       SYU-KEISU-EX.
           EXIT.
      *****************************************************************
      * 地区別退避計数
      *****************************************************************
       CHIKU-KEISU             SECTION.
       CHK-010.
           IF IN-SPT (1:1) = LOW-VALUE
               GO TO CHIKU-KEISU-EX.
           IF IN-SPT (3:2) NOT NUMERIC
               GO TO CHIKU-KEISU-EX.
           MOVE IN-SPT (3:2)   TO WS-CHIKU.
           IF WS-CHIKU LESS 1 OR WS-CHIKU GREATER 47
               GO TO CHIKU-KEISU-EX.
           ADD 1               TO CHIKU-BK (WS-CHIKU).
       CHIKU-KEISU-EX.
           EXIT.
      *****************************************************************
      * 新レイアウト部の粗検査 (増改築フラグ域)
      *****************************************************************
       SHIN-KENSA              SECTION.
       SNK-010.
           IF IN-REC (122:1) NOT = 'Y' AND IN-REC (122:1) NOT = 'N'
               GO TO SNK-NG.
           IF IN-REC (123:1) NOT = 'Y' AND IN-REC (123:1) NOT = 'N'
               GO TO SNK-NG.
           IF IN-REC (124:1) NOT = '1' AND IN-REC (124:1) NOT = '0'
               GO TO SNK-NG.
           IF IN-REC (125:1) NOT = '1' AND IN-REC (125:1) NOT = '0'
               GO TO SNK-NG.
           IF IN-REC (139:1) NOT = '0' AND IN-REC (139:1) NOT = '1'
               GO TO SNK-NG.
           IF IN-REC (143:4) NOT = 'EL01' AND
              IN-REC (143:4) NOT = SPACE
               GO TO SNK-NG.
           IF IN-REC (128:8) NOT NUMERIC
               GO TO SNK-NG.
           IF IN-REC (153:8) NOT NUMERIC
               GO TO SNK-NG.
           IF IN-REC (161:8) = SPACES
               GO TO SNK-NG.
           GO TO SHIN-KENSA-EX.
       SNK-NG.
           ADD 1               TO WK-NG-CNT.
       SHIN-KENSA-EX.
           EXIT.
      *****************************************************************
      * 退避前検査 (NG でも退避はする. 件数のみ制御へ)
      *****************************************************************
       IN-VALID                SECTION.
       IVA-010.
           IF IN-SPT (1:1) = LOW-VALUE
               GO TO IN-VALID-EX.
           IF IN-SPT (1:2) NOT = '03'
               GO TO IVA-NG.
           IF IN-SPT (3:2) LESS '01' OR IN-SPT (3:2) GREATER '47'
               GO TO IVA-NG.
           IF IN-SPT (21:2) NOT NUMERIC
               GO TO IVA-NG.
           IF IN-JYU NOT NUMERIC
               GO TO IVA-NG.
           IF IN-SYU NOT = '10' AND IN-SYU NOT = '11' AND
              IN-SYU NOT = '20' AND IN-SYU NOT = '99'
               GO TO IVA-NG.
           IF IN-KAISI NOT NUMERIC
               GO TO IVA-NG.
           IF IN-KAISI (5:2) LESS '01' OR
              IN-KAISI (5:2) GREATER '12'
               GO TO IVA-NG.
           IF IN-SYURYO NOT = 99991231 AND
              IN-SYURYO NOT = 99999999 AND
              IN-SYURYO NOT = 00000000
               IF IN-SYURYO LESS IN-KAISI
                   GO TO IVA-NG
               END-IF
           END-IF
           IF IN-TEISI NOT = '0' AND IN-TEISI NOT = '1'
               GO TO IVA-NG.
           GO TO IN-VALID-EX.
       IVA-NG.
           ADD 1               TO WK-NG-CNT.
       IN-VALID-EX.
           EXIT.
