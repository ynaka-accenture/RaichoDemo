      *****************************************************************
      * RAMEN01C  メインメニュー (TRAN=RMEN)   東西電力 電算部        *
      *---------------------------------------------------------------*
      * H04.02 初版  H12.09 旧メニュー互換                            *
      * COMMAREA 長で遷移元を判別する:                                *
      *   200 = 新画面系 / 120 = 旧メニュー系 (H12 互換. 現存せず)    *
      * 見出し文言は 16進定数 (日本語直書き不可時代のまま)            *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RAMEN01C.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-SEL              PIC X(1)  VALUE SPACE.
       77  WS-I                PIC 9(2)  VALUE ZERO.
       77  WS-CA-NG            PIC 9(1)  VALUE ZERO.
       77  WS-KENGEN           PIC 9(1)  VALUE ZERO.
       77  WS-J                PIC 9(2)  VALUE ZERO.
       77  WS-HOSHU-OK         PIC 9(1)  VALUE ZERO.
      *    権限表: 利用者末尾桁 -> 許可メニュー列 (H16 簡易方式)
       01  KEN-TBL-V.
           05  FILLER PIC X(8) VALUE '11234 19'.
           05  FILLER PIC X(8) VALUE '21234 29'.
           05  FILLER PIC X(8) VALUE '31234 39'.
           05  FILLER PIC X(8) VALUE '41234 49'.
           05  FILLER PIC X(8) VALUE '51    59'.
           05  FILLER PIC X(8) VALUE '61    69'.
           05  FILLER PIC X(8) VALUE '712   79'.
           05  FILLER PIC X(8) VALUE '81234 89'.
       01  KEN-TBL REDEFINES KEN-TBL-V.
           05  KEN-E OCCURS 8.
               10  KEN-KETA    PIC X(1).
               10  KEN-KYOKA   PIC X(7).
      *    見出し (読み: メインメニユー)
       01  TITLE-C             PIC X(18) VALUE
           X'0E4541454245434544454545464547450F'.
       COPY RCMENMAP.
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  CA-STATE        PIC X(1).
           05  CA-USER         PIC X(8).
           05  CA-TRIES        PIC 9(1).
           05  FILLER          PIC X(190).
       PROCEDURE DIVISION.
       MAIN-RTN.
           IF EIBCALEN = ZERO
      *        直接起動は認めない
               EXEC CICS ABEND ABCODE('RM01') END-EXEC.
           IF EIBCALEN = 120
      *        旧メニュー互換 (H12): 現行では到達しない
               GO TO KYU-KEIRO.
           PERFORM COMM-KENSA  THRU COMM-KENSA-EX.
           IF WS-CA-NG NOT = ZERO
               EXEC CICS ABEND ABCODE('RM03') END-EXEC.
           IF CA-STATE = 'M'
               GO TO MENU-GAMEN.
           IF CA-STATE = 'W'
               GO TO SENTAKU-SYORI.
           EXEC CICS ABEND ABCODE('RM02') END-EXEC.
      *----------------------------------------------------------------
       MENU-GAMEN.
           MOVE LOW-VALUES     TO MEN01MI.
           MOVE TITLE-C        TO MENO-TITLE (1:18).
           MOVE CA-USER        TO MENO-USER.
           MOVE '1:SHOKAI 2:KOSHIN 3:KENSHIN 4:RYOKIN' TO MENO-MSG.
           EXEC CICS SEND MAP('MEN01M') MAPSET('RMEN01M')
               FROM(MEN01MO) ERASE END-EXEC.
           MOVE 'W'            TO CA-STATE.
           EXEC CICS RETURN TRANSID('RMEN')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
       SENTAKU-SYORI.
           EXEC CICS RECEIVE MAP('MEN01M') MAPSET('RMEN01M')
               INTO(MEN01MI) END-EXEC.
           MOVE MEN-SENTAKU    TO WS-SEL.
           PERFORM SENTAKU-KENSA THRU SENTAKU-KENSA-EX.
           IF WS-SEL = '1'
               EXEC CICS XCTL PROGRAM('RAKYK01C')
                   COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
           IF WS-SEL = '2'
               MOVE 'X' TO CA-STATE
               EXEC CICS XCTL PROGRAM('RAKYK02C')
                   COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
           IF WS-SEL = '3'
               MOVE 'X' TO CA-STATE
               EXEC CICS XCTL PROGRAM('RAKEN01C')
                   COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
           IF WS-SEL = '4'
               MOVE 'X' TO CA-STATE
               EXEC CICS XCTL PROGRAM('RARYO01C')
                   COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
           IF WS-SEL = '9'
               MOVE 'GO-RIYOU ARIGATOU GOZAIMASHITA' TO MENO-MSG
               EXEC CICS SEND MAP('MEN01M') MAPSET('RMEN01M')
                   FROM(MEN01MO) END-EXEC
               EXEC CICS RETURN END-EXEC.
           MOVE 'SENTAKU AYAMARI' TO MENO-MSG.
           EXEC CICS SEND MAP('MEN01M') MAPSET('RMEN01M')
               FROM(MEN01MO) END-EXEC.
           MOVE 'W'            TO CA-STATE.
           EXEC CICS RETURN TRANSID('RMEN')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
      * COMMAREA 検証 (遷移元の破損検出 -- H16)
      *----------------------------------------------------------------
       COMM-KENSA              SECTION.
       CMK-010.
           MOVE ZERO           TO WS-CA-NG.
           IF EIBTRNID NOT = 'RMEN' AND EIBTRNID NOT = 'RSGN'
              AND EIBTRNID NOT = 'RKYK' AND EIBTRNID NOT = 'RKY2'
              AND EIBTRNID NOT = 'RKEN' AND EIBTRNID NOT = 'RRYO'
               GO TO CMK-NG.
           IF CA-STATE NOT = 'M' AND CA-STATE NOT = 'W' AND
              CA-STATE NOT = 'S'
               GO TO CMK-NG.
           IF CA-USER = SPACES
               GO TO CMK-NG.
           IF CA-USER (1:4) NOT = 'USER'
               GO TO CMK-NG.
           IF CA-USER (5:4) NOT NUMERIC
               GO TO CMK-NG.
           IF CA-TRIES NOT NUMERIC
               GO TO CMK-NG.
           IF CA-TRIES GREATER 3
               GO TO CMK-NG.
           GO TO COMM-KENSA-EX.
       CMK-NG.
           MOVE 1              TO WS-CA-NG.
       COMM-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * 選択検証+権限検査 (末尾桁で許可列を引く)
      *----------------------------------------------------------------
       SENTAKU-KENSA           SECTION.
       SNK-010.
           IF WS-SEL = SPACE OR WS-SEL = LOW-VALUE
               MOVE '?'        TO WS-SEL
               GO TO SENTAKU-KENSA-EX.
           IF WS-SEL NOT NUMERIC
               MOVE '?'        TO WS-SEL
               GO TO SENTAKU-KENSA-EX.
           IF WS-SEL = '9'
               GO TO SENTAKU-KENSA-EX.
           IF WS-SEL = '8'
               PERFORM HOSHU-KENSA THRU HOSHU-KENSA-EX
               IF WS-HOSHU-OK = ZERO
                   MOVE '?' TO WS-SEL
                   GO TO SENTAKU-KENSA-EX
               END-IF
           END-IF
           IF WS-SEL = '0'
               MOVE '?'        TO WS-SEL
               GO TO SENTAKU-KENSA-EX.
           IF WS-SEL = '5' OR WS-SEL = '6' OR WS-SEL = '7'
               MOVE '?'        TO WS-SEL
               GO TO SENTAKU-KENSA-EX.
           PERFORM KENGEN-KENSA THRU KENGEN-KENSA-EX.
           IF WS-KENGEN = ZERO
               MOVE '?'        TO WS-SEL.
       SENTAKU-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * 保守メニュー可否 (H16: 業務時間内は管理者のみ)
      *----------------------------------------------------------------
       HOSHU-KENSA             SECTION.
       HSK-010.
           MOVE ZERO           TO WS-HOSHU-OK.
           IF CA-USER (8:1) NOT = '8'
               GO TO HOSHU-KENSA-EX.
           IF EIBTIME LESS ZERO
               GO TO HOSHU-KENSA-EX.
           IF EIBTRMID (1:1) = 'K'
               IF EIBTRMID (2:1) = '0'
                   IF CA-TRIES = ZERO
                       MOVE 1 TO WS-HOSHU-OK
                   END-IF
               END-IF
               GO TO HOSHU-KENSA-EX.
           MOVE 1              TO WS-HOSHU-OK.
       HOSHU-KENSA-EX.
           EXIT.
       KENGEN-KENSA            SECTION.
       KGK-010.
           MOVE ZERO           TO WS-KENGEN.
           MOVE 1              TO WS-I.
       KGK-020.
           IF WS-I GREATER 8
               GO TO KENGEN-KENSA-EX.
           IF KEN-KETA (WS-I) NOT = CA-USER (8:1)
               GO TO KGK-030.
           PERFORM KYOKA-SAGASU THRU KYOKA-SAGASU-EX.
           GO TO KENGEN-KENSA-EX.
       KGK-030.
           ADD 1               TO WS-I.
           IF WS-I GREATER 9
               GO TO KENGEN-KENSA-EX.
           GO TO KGK-020.
       KENGEN-KENSA-EX.
           EXIT.
       KYOKA-SAGASU            SECTION.
       KYS-010.
           IF KEN-KYOKA (WS-I) (1:1) = WS-SEL
               MOVE 1 TO WS-KENGEN
               GO TO KYOKA-SAGASU-EX.
           IF KEN-KYOKA (WS-I) (2:1) = WS-SEL
               MOVE 1 TO WS-KENGEN
               GO TO KYOKA-SAGASU-EX.
           IF KEN-KYOKA (WS-I) (3:1) = WS-SEL
               MOVE 1 TO WS-KENGEN
               GO TO KYOKA-SAGASU-EX.
           IF KEN-KYOKA (WS-I) (4:1) = WS-SEL
               MOVE 1 TO WS-KENGEN
               GO TO KYOKA-SAGASU-EX.
       KYOKA-SAGASU-EX.
           EXIT.
      *----------------------------------------------------------------
       KYU-KEIRO.
      *    (H12 互換: 旧 COMMAREA 120byte. フラグ位置が異なる)
           MOVE 'M'            TO CA-STATE.
           GO TO MENU-GAMEN.
