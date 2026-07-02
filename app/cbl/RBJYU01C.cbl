      *****************************************************************
      * RBJYU01C  需要家一括異動 (日次D080)     東西電力 電算部       *
      *---------------------------------------------------------------*
      * H03.06 初版  H07.11 対応表転記化 (障害 No.212 参照)           *
      * 異動項目の転記は MOVE CORRESPONDING. 異動レコードと更新域の   *
      * 項目名一致で転記対象が決まるため 項目追加時は両側同時に行う   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBJYU01C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JYUIN   ASSIGN TO 'app/data/portable/JYUMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-JYU.
           SELECT IDOIN   ASSIGN TO 'app/data/portable/IDOIN.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-IDO.
           SELECT JYUOUT  ASSIGN TO 'app/data/portable/JYUMST2.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-OUT.
       DATA DIVISION.
       FILE SECTION.
       FD  JYUIN RECORD CONTAINS 256 CHARACTERS.
       COPY RCJYUREC.
       FD  IDOIN RECORD CONTAINS 40 CHARACTERS.
       01  IDO-REC.
           05  IDO-JNO         PIC 9(10).
           05  IDO-TEL         PIC X(11).
           05  IDO-MAIL        PIC X(1).
           05  IDO-KAIYAKU     PIC 9(8).
           05  FILLER          PIC X(10).
       FD  JYUOUT RECORD CONTAINS 256 CHARACTERS.
       01  OUT-REC             PIC X(256).
       WORKING-STORAGE SECTION.
       77  ST-JYU              PIC XX VALUE '00'.
       77  ST-IDO              PIC XX VALUE '00'.
       77  ST-OUT              PIC XX VALUE '00'.
       77  WK-IN-CNT           PIC 9(7)  VALUE ZERO.
       77  WK-TEKIYO           PIC 9(7)  VALUE ZERO.
       77  WK-FUNOU            PIC 9(7)  VALUE ZERO.
       77  WK-NG-CNT           PIC 9(7)  VALUE ZERO.
       77  WS-I                PIC 9(3)  VALUE ZERO.
       77  WS-J                PIC 9(3)  VALUE ZERO.
       77  WS-LO               PIC S9(3) VALUE ZERO.
       77  WS-HI               PIC S9(3) VALUE ZERO.
       77  WS-MD               PIC S9(3) VALUE ZERO.
       77  WS-HIT              PIC S9(3) VALUE ZERO.
       77  WK-SOSI-NG          PIC 9(7)  VALUE ZERO.
       77  WK-DUP-NG           PIC 9(3)  VALUE ZERO.
      *----------------------------------------------------------------
      *    異動転記域: 同名項目のみ CORRESPONDING で写る (H07 方式)
      *----------------------------------------------------------------
       01  W-IDO-KOSHIN.
           05  TEL             PIC X(11).
           05  MAIL-FLG        PIC X(1).
           05  KAIYAKU-BI      PIC 9(8).
       01  W-JYU-KOSHIN.
           05  TEL             PIC X(11).
           05  MAIL-FLG        PIC X(1).
           05  KAIYAKU-BI      PIC 9(8).
       01  IDT-AREA.
           05  IDT-CNT         PIC S9(3) COMP-3 VALUE ZERO.
           05  IDT-E OCCURS 100.
               10  IDT-JNO     PIC 9(10).
               10  IDT-TEL     PIC X(11).
               10  IDT-MAIL    PIC X(1).
               10  IDT-KAIYAKU PIC 9(8).
       01  IDT-SWAP.
           05  SW-JNO          PIC 9(10).
           05  SW-TEL          PIC X(11).
           05  SW-MAIL         PIC X(1).
           05  SW-KAIYAKU     PIC 9(8).
       PROCEDURE DIVISION.
       MAIN-SEC                SECTION.
       MAIN-000.
           PERFORM IDT-LOAD    THRU IDT-LOAD-EX.
           PERFORM IDT-SORT    THRU IDT-SORT-EX.
           OPEN INPUT JYUIN.
           OPEN OUTPUT JYUOUT.
       MAIN-LOOP.
           READ JYUIN
               AT END GO TO SYUKEI-RTN.
           ADD 1               TO WK-IN-CNT.
           PERFORM JYU-VALID   THRU JYU-VALID-EX.
           PERFORM IDT-SRCH    THRU IDT-SRCH-EX.
           IF WS-HIT GREATER ZERO
               PERFORM IDO-TEKIYO THRU IDO-TEKIYO-EX.
           MOVE JYU-REC        TO OUT-REC.
           WRITE OUT-REC.
           GO TO MAIN-LOOP.
       SYUKEI-RTN.
           CLOSE JYUIN JYUOUT.
           DISPLAY 'RBJYU01C IN=' WK-IN-CNT ' IDO=' IDT-CNT
                   ' TEKIYO=' WK-TEKIYO.
           DISPLAY 'FUNOU=' WK-FUNOU ' NG=' WK-NG-CNT
                   ' SOSI=' WK-SOSI-NG ' DUP=' WK-DUP-NG.
           MOVE 0              TO RETURN-CODE.
           STOP RUN.
       ABEND-RTN.
           DISPLAY 'RBJYU01C ABEND'.
           MOVE 16             TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
      * 異動適用: 対応表転記 -> マスタ各項目へ
      *****************************************************************
       IDO-TEKIYO              SECTION.
       IDT-010.
           IF JYU-JOTAI-KBN NOT = '1'
               ADD 1           TO WK-FUNOU
               GO TO IDO-TEKIYO-EX.
           MOVE IDT-TEL  (WS-HIT) TO TEL OF W-IDO-KOSHIN.
           MOVE IDT-MAIL (WS-HIT) TO MAIL-FLG OF W-IDO-KOSHIN.
           MOVE IDT-KAIYAKU (WS-HIT)
                                  TO KAIYAKU-BI OF W-IDO-KOSHIN.
      *    対応表転記 (名前一致. 項目追加時は両側同時に)
           MOVE CORRESPONDING W-IDO-KOSHIN TO W-JYU-KOSHIN.
           IF TEL OF W-JYU-KOSHIN NOT = SPACES
               MOVE TEL OF W-JYU-KOSHIN TO JYU-TEL.
           IF MAIL-FLG OF W-JYU-KOSHIN = 'Y' OR
              MAIL-FLG OF W-JYU-KOSHIN = 'N'
               MOVE MAIL-FLG OF W-JYU-KOSHIN TO JYU-MAIL-FLG.
           IF KAIYAKU-BI OF W-JYU-KOSHIN NOT = ZERO
               MOVE KAIYAKU-BI OF W-JYU-KOSHIN TO JYU-KAIYAKU-BI.
           MOVE 20260701       TO JYU-KOSHIN-BI.
           MOVE 'RBJYU01C'     TO JYU-KOSHIN-ID.
           ADD 1               TO WK-TEKIYO.
           IF WK-TEKIYO GREATER IDT-CNT
               DISPLAY 'RBJYU01C W401 TEKIYO KAJO'
               GO TO IDO-TEKIYO-EX.
           IF JYU-TEL (1:2) NOT = '07' AND
              JYU-TEL (1:2) NOT = '03'
               ADD 1 TO WK-SOSI-NG
               GO TO IDO-TEKIYO-EX.
       IDO-TEKIYO-EX.
           EXIT.
      *****************************************************************
      * マスタ検証 (H09 一括点検で追加)
      *****************************************************************
       JYU-VALID               SECTION.
       JVA-010.
           IF JYU-NO NOT NUMERIC
               GO TO JVA-NG.
           IF JYU-NO LESS 1000000000
               GO TO JVA-NG.
           IF JYU-SIMEI (1:1) = X'0E'
               IF JYU-SIMEI (40:1) NOT = X'0F' AND
                  JYU-SIMEI (40:1) NOT = SPACE
                   ADD 1 TO WK-SOSI-NG
               END-IF
           END-IF.
           IF JYU-KAISI-BI NOT NUMERIC
               GO TO JVA-NG.
           IF JYU-KAISI-BI (5:2) LESS '01' OR
              JYU-KAISI-BI (5:2) GREATER '12'
               GO TO JVA-NG.
           IF JYU-KAIYAKU-BI NOT NUMERIC
               GO TO JVA-NG.
           IF JYU-JOTAI-KBN NOT = '1' AND JYU-JOTAI-KBN NOT = '2'
               GO TO JVA-NG.
           IF JYU-SEIKYU-HOHO NOT = '0' AND
              JYU-SEIKYU-HOHO NOT = '1'
               GO TO JVA-NG.
           IF JYU-MAIL-FLG NOT = 'Y' AND JYU-MAIL-FLG NOT = 'N'
               GO TO JVA-NG.
           IF JYU-KOFURI-KBN NOT = '0' AND JYU-KOFURI-KBN NOT = '1'
               GO TO JVA-NG.
           IF JYU-KOSHIN-BI NOT NUMERIC
               GO TO JVA-NG.
           IF JYU-KEIYAKU-SU LESS ZERO
               GO TO JVA-NG.
           IF JYU-TEL = SPACES
               GO TO JVA-NG.
           GO TO JYU-VALID-EX.
       JVA-NG.
           ADD 1               TO WK-NG-CNT.
       JYU-VALID-EX.
           EXIT.
      *****************************************************************
      * 異動テーブル (検証つき読込)
      *****************************************************************
       IDT-LOAD                SECTION.
       IDL-010.
           OPEN INPUT IDOIN.
       IDL-020.
           IF ST-IDO NOT = '00'
               GO TO IDL-090.
           READ IDOIN
               AT END GO TO IDL-090.
           IF IDT-CNT NOT LESS 100
               GO TO IDL-090.
           IF IDO-JNO NOT NUMERIC
               ADD 1 TO WK-NG-CNT
               GO TO IDL-020.
           IF IDO-JNO LESS 1000000000
               ADD 1 TO WK-NG-CNT
               GO TO IDL-020.
           IF IDO-TEL = SPACES AND IDO-MAIL = SPACE AND
              IDO-KAIYAKU = ZERO
               ADD 1 TO WK-NG-CNT
               GO TO IDL-020.
           IF IDO-MAIL NOT = 'Y' AND IDO-MAIL NOT = 'N' AND
              IDO-MAIL NOT = SPACE
               ADD 1 TO WK-NG-CNT
               GO TO IDL-020.
           IF IDO-KAIYAKU NOT NUMERIC
               ADD 1 TO WK-NG-CNT
               GO TO IDL-020.
           MOVE 1              TO WS-J.
       IDL-030.
           IF WS-J GREATER IDT-CNT
               GO TO IDL-040.
           IF IDT-JNO (WS-J) = IDO-JNO
               ADD 1 TO WK-DUP-NG
               GO TO IDL-020.
           ADD 1 TO WS-J.
           GO TO IDL-030.
       IDL-040.
           ADD 1               TO IDT-CNT.
           MOVE IDO-JNO        TO IDT-JNO (IDT-CNT).
           MOVE IDO-TEL        TO IDT-TEL (IDT-CNT).
           MOVE IDO-MAIL       TO IDT-MAIL (IDT-CNT).
           MOVE IDO-KAIYAKU    TO IDT-KAIYAKU (IDT-CNT).
           GO TO IDL-020.
       IDL-090.
           CLOSE IDOIN.
       IDT-LOAD-EX.
           EXIT.
       IDT-SORT                SECTION.
       IDS-010.
           MOVE 2              TO WS-I.
       IDS-020.
           IF WS-I GREATER IDT-CNT
               GO TO IDT-SORT-EX.
           MOVE IDT-E (WS-I)   TO IDT-SWAP.
           COMPUTE WS-J = WS-I - 1.
       IDS-030.
           IF WS-J LESS 1
               GO TO IDS-040.
           IF IDT-JNO (WS-J) NOT GREATER SW-JNO
               GO TO IDS-040.
           MOVE IDT-E (WS-J)   TO IDT-E (WS-J + 1).
           SUBTRACT 1        FROM WS-J.
           GO TO IDS-030.
       IDS-040.
           MOVE IDT-SWAP       TO IDT-E (WS-J + 1).
           ADD 1               TO WS-I.
           GO TO IDS-020.
       IDT-SORT-EX.
           EXIT.
       IDT-SRCH                SECTION.
       IDR-010.
           MOVE ZERO           TO WS-HIT.
           MOVE 1              TO WS-LO.
           MOVE IDT-CNT        TO WS-HI.
       IDR-020.
           IF WS-LO GREATER WS-HI
               GO TO IDT-SRCH-EX.
           COMPUTE WS-MD = ( WS-LO + WS-HI ) / 2.
           IF IDT-JNO (WS-MD) = JYU-NO
               MOVE WS-MD      TO WS-HIT
               GO TO IDT-SRCH-EX.
           IF IDT-JNO (WS-MD) LESS JYU-NO
               COMPUTE WS-LO = WS-MD + 1
           ELSE
               COMPUTE WS-HI = WS-MD - 1.
           GO TO IDR-020.
       IDT-SRCH-EX.
           EXIT.
