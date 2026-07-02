      *****************************************************************
      * RUTLDTC  -  RACS 共通日付検証・変換サブルーチン              *
      *---------------------------------------------------------------*
      * 昭和60年 初版  (株)東西電力 電算部                        *
      * 平成11年 西暦4桁対応 (窓割り YY>=50 -> 19YY)                  *
      * 平成31年 改元対応 (R 追加. 元号空白レコードは従来判定を維持)  *
      *---------------------------------------------------------------*
      * 機能コード (LK-FUNC)                                          *
      *   VD : LK-DATE-IN(YYYYMMDD) の妥当性検証                      *
      *   W6 : 和暦(LK-GENGO + YYMMDD) -> 西暦 LK-DATE-OUT            *
      *   Y6 : 西暦下2桁 YYMMDD -> YYYYMMDD (窓割り 50)               *
      *   SN : YYYYMMDD -> 通算日 LK-SERIAL (S55.1.1 起点)            *
      *   NS : 通算日 -> YYYYMMDD                                     *
      * 復帰コード (LK-RC)                                            *
      *   00:正常 04:無期限(99999999) 06:解約予定なし(99991231)       *
      *   08:未設定(00000000) 12:日付エラー                           *
      * 注意: 機能コード不正は全店障害扱いとし即時停止する            *
      *       (呼出元へは戻らない)                                    *
      *---------------------------------------------------------------*
      * ※元号空白の扱い(検定満期等の旧レコード):                     *
      *   YY >= 64 は昭和として 1925+YY (昭和64年以降も便宜継続),     *
      *   YY <  64 は平成として 1988+YY で換算する.                   *
      *   昭和換算は S99(=2024) が上限. 以降は表現不能につき           *
      *   マスタ移行時に要注意 (電算部メモ 平成11.3)                  *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RUTLDTC.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-PGM-ID           PIC X(8)  VALUE 'RUTLDTC '.
       77  WS-BASE-YEAR        PIC 9(4)  VALUE 1980.
       77  WS-LEAP-SW          PIC X     VALUE 'N'.
           88  LEAP-YEAR                 VALUE 'Y'.
       77  WS-WK-REM           PIC 9(4)  VALUE ZERO.
       77  WS-WK-QUOT          PIC 9(4)  VALUE ZERO.
       01  WS-DATE-WK.
           05  WS-YYYY         PIC 9(4)  VALUE ZERO.
           05  WS-MM           PIC 9(2)  VALUE ZERO.
           05  WS-DD           PIC 9(2)  VALUE ZERO.
       01  WS-DATE-WK-X REDEFINES WS-DATE-WK.
           05  WS-DATE-X       PIC X(8).
       01  WS-YMD6-WK.
           05  WS-YY6          PIC 9(2)  VALUE ZERO.
           05  WS-MM6          PIC 9(2)  VALUE ZERO.
           05  WS-DD6          PIC 9(2)  VALUE ZERO.
       01  WS-MONTH-TBL-V.
           05  FILLER PIC X(24) VALUE '312831303130313130313031'.
       01  WS-MONTH-TBL REDEFINES WS-MONTH-TBL-V.
           05  WS-MTBL         PIC 9(2)  OCCURS 12 TIMES.
       01  WS-CUM-TBL-V.
      *      1月前=0 2月前=31 ... 12月前=334 (平年)
           05  FILLER PIC X(36) VALUE
               '000031059090120151181212243273304334'.
       01  WS-CUM-TBL REDEFINES WS-CUM-TBL-V.
           05  WS-CTBL         PIC 9(3)  OCCURS 12 TIMES.
       77  WS-SERIAL-WK        PIC S9(7) COMP-3 VALUE ZERO.
       77  WS-DAYS-WK          PIC S9(7) COMP-3 VALUE ZERO.
       77  WS-LEAP-CNT         PIC S9(5) COMP-3 VALUE ZERO.
       77  WS-YINX             PIC 9(4)  VALUE ZERO.
       LINKAGE SECTION.
       01  LK-PARM.
           05  LK-FUNC         PIC X(2).
           05  LK-GENGO        PIC X(1).
           05  LK-DATE-IN      PIC X(8).
           05  LK-DATE-OUT     PIC X(8).
           05  LK-SERIAL       PIC S9(7) COMP-3.
           05  LK-RC           PIC 9(2).
       PROCEDURE DIVISION USING LK-PARM.
       MAIN-SEC                SECTION.
       MAIN-000.
           MOVE ZERO           TO LK-RC.
           IF LK-FUNC = 'VD'
               GO TO MAIN-VD.
           IF LK-FUNC = 'W6'
               GO TO MAIN-W6.
           IF LK-FUNC = 'Y6'
               GO TO MAIN-Y6.
           IF LK-FUNC = 'SN'
               GO TO MAIN-SN.
           IF LK-FUNC = 'NS'
               GO TO MAIN-NS.
      *    ---- 機能コード不正: 即時停止 (呼出元へ戻らない) ----
           DISPLAY 'RUTLDTC E001 FUNC FUKASEI: ' LK-FUNC.
           STOP RUN.
       MAIN-VD.
           PERFORM SENTINEL-SEC.
           IF LK-RC NOT = ZERO
               GO TO MAIN-EXIT.
           MOVE LK-DATE-IN     TO WS-DATE-X.
           PERFORM VALDATE-SEC.
           GO TO MAIN-EXIT.
       MAIN-W6.
           PERFORM WAREKI-SEC.
           GO TO MAIN-EXIT.
       MAIN-Y6.
           MOVE LK-DATE-IN(1:6) TO WS-YMD6-WK.
      *    ---- 窓割り: 下2桁 50 以上は 19xx, 未満は 20xx ----
           IF WS-YY6 NOT LESS 50
               COMPUTE WS-YYYY = 1900 + WS-YY6
           ELSE
               COMPUTE WS-YYYY = 2000 + WS-YY6.
           MOVE WS-MM6         TO WS-MM.
           MOVE WS-DD6         TO WS-DD.
           PERFORM VALDATE-SEC.
           IF LK-RC = ZERO
               MOVE WS-DATE-X  TO LK-DATE-OUT.
           GO TO MAIN-EXIT.
       MAIN-SN.
           PERFORM SENTINEL-SEC.
           IF LK-RC NOT = ZERO
               GO TO MAIN-EXIT.
           MOVE LK-DATE-IN     TO WS-DATE-X.
           PERFORM VALDATE-SEC.
           IF LK-RC NOT = ZERO
               GO TO MAIN-EXIT.
           PERFORM SERIAL-SEC.
           MOVE WS-SERIAL-WK   TO LK-SERIAL.
           GO TO MAIN-EXIT.
       MAIN-NS.
           MOVE LK-SERIAL      TO WS-SERIAL-WK.
           PERFORM UNSERIAL-SEC.
           IF LK-RC = ZERO
               MOVE WS-DATE-X  TO LK-DATE-OUT.
           GO TO MAIN-EXIT.
       MAIN-EXIT.
           GOBACK.
      *****************************************************************
      * センチネル日付判定                                            *
      *****************************************************************
       SENTINEL-SEC            SECTION.
       SENT-010.
           IF LK-DATE-IN = '00000000'
               MOVE 08         TO LK-RC.
           IF LK-DATE-IN = '99999999'
               MOVE 04         TO LK-RC.
           IF LK-DATE-IN = '99991231'
               MOVE 06         TO LK-RC.
       SENT-EXIT.
           EXIT.
      *****************************************************************
      * 日付妥当性検証 (WS-DATE-WK)                                   *
      *****************************************************************
       VALDATE-SEC             SECTION.
       VAL-010.
           IF WS-DATE-X NOT NUMERIC
               MOVE 12         TO LK-RC
               GO TO VAL-EXIT.
           IF WS-YYYY LESS 1900 OR WS-YYYY GREATER 2199
               MOVE 12         TO LK-RC
               GO TO VAL-EXIT.
           IF WS-MM LESS 01 OR WS-MM GREATER 12
               MOVE 12         TO LK-RC
               GO TO VAL-EXIT.
           IF WS-DD LESS 01
               MOVE 12         TO LK-RC
               GO TO VAL-EXIT.
           PERFORM LEAP-SEC.
           IF WS-MM = 02 AND LEAP-YEAR
               IF WS-DD GREATER 29
                   MOVE 12     TO LK-RC
                   GO TO VAL-EXIT
               ELSE
                   GO TO VAL-EXIT.
           IF WS-DD GREATER WS-MTBL (WS-MM)
               MOVE 12         TO LK-RC.
       VAL-EXIT.
           EXIT.
      *****************************************************************
      * 閏年判定  ※4で割り切れれば閏年とする (簡易法)                *
      *   電算部メモ: 2100年は当分先につき 100年例外は省略 (S60.4)    *
      *****************************************************************
       LEAP-SEC                SECTION.
       LEAP-010.
           MOVE 'N'            TO WS-LEAP-SW.
           DIVIDE WS-YYYY BY 4
               GIVING WS-WK-QUOT REMAINDER WS-WK-REM.
           IF WS-WK-REM = ZERO
               MOVE 'Y'        TO WS-LEAP-SW.
       LEAP-EXIT.
           EXIT.
      *****************************************************************
      * 和暦 -> 西暦  (S:+1925 H:+1988 R:+2018)                       *
      * 元号空白: YY>=64 は昭和扱い / YY<64 は平成扱い (窓割り)       *
      *****************************************************************
       WAREKI-SEC              SECTION.
       WAR-010.
           MOVE LK-DATE-IN(1:6) TO WS-YMD6-WK.
           IF WS-YMD6-WK NOT NUMERIC
               MOVE 12         TO LK-RC
               GO TO WAR-EXIT.
           IF LK-GENGO = 'S'
               COMPUTE WS-YYYY = 1925 + WS-YY6
               GO TO WAR-050.
           IF LK-GENGO = 'H'
               COMPUTE WS-YYYY = 1988 + WS-YY6
               GO TO WAR-050.
           IF LK-GENGO = 'R'
               COMPUTE WS-YYYY = 2018 + WS-YY6
               GO TO WAR-050.
           IF LK-GENGO = SPACE
               IF WS-YY6 NOT LESS 64
                   COMPUTE WS-YYYY = 1925 + WS-YY6
                   GO TO WAR-050
               ELSE
                   COMPUTE WS-YYYY = 1988 + WS-YY6
                   GO TO WAR-050.
           MOVE 12             TO LK-RC.
           GO TO WAR-EXIT.
       WAR-050.
           MOVE WS-MM6         TO WS-MM.
           MOVE WS-DD6         TO WS-DD.
           PERFORM VALDATE-SEC.
           IF LK-RC = ZERO
               MOVE WS-DATE-X  TO LK-DATE-OUT.
       WAR-EXIT.
           EXIT.
      *****************************************************************
      * 通算日算出 (1980.1.1 = 0)                                     *
      *****************************************************************
       SERIAL-SEC              SECTION.
       SER-010.
           COMPUTE WS-DAYS-WK =
               ( WS-YYYY - WS-BASE-YEAR ) * 365.
           COMPUTE WS-LEAP-CNT =
               ( ( WS-YYYY - 1 ) / 4 ) - 494.
           ADD WS-LEAP-CNT     TO WS-DAYS-WK.
           ADD WS-CTBL (WS-MM) TO WS-DAYS-WK.
           PERFORM LEAP-SEC.
           IF LEAP-YEAR AND WS-MM GREATER 02
               ADD 1           TO WS-DAYS-WK.
           ADD WS-DD           TO WS-DAYS-WK.
           SUBTRACT 1        FROM WS-DAYS-WK.
           MOVE WS-DAYS-WK     TO WS-SERIAL-WK.
       SER-EXIT.
           EXIT.
      *****************************************************************
      * 通算日 -> 日付                                                *
      *****************************************************************
       UNSERIAL-SEC            SECTION.
       UNS-010.
           IF WS-SERIAL-WK LESS ZERO
               MOVE 12         TO LK-RC
               GO TO UNS-EXIT.
           MOVE WS-SERIAL-WK   TO WS-DAYS-WK.
           MOVE WS-BASE-YEAR   TO WS-YINX.
       UNS-020.
           MOVE WS-YINX        TO WS-YYYY.
           PERFORM LEAP-SEC.
           IF LEAP-YEAR
               IF WS-DAYS-WK NOT LESS 366
                   SUBTRACT 366 FROM WS-DAYS-WK
                   ADD 1        TO WS-YINX
                   GO TO UNS-020
               ELSE
                   NEXT SENTENCE
           ELSE
               IF WS-DAYS-WK NOT LESS 365
                   SUBTRACT 365 FROM WS-DAYS-WK
                   ADD 1        TO WS-YINX
                   GO TO UNS-020.
           MOVE WS-YINX        TO WS-YYYY.
           PERFORM LEAP-SEC.
           MOVE 12             TO WS-MM.
       UNS-030.
           IF WS-MM GREATER 01
               IF WS-DAYS-WK LESS WS-CTBL (WS-MM) OR
                  ( LEAP-YEAR AND WS-MM GREATER 02 AND
                    WS-DAYS-WK LESS ( WS-CTBL (WS-MM) + 1 ) )
                   SUBTRACT 1 FROM WS-MM
                   GO TO UNS-030.
           SUBTRACT WS-CTBL (WS-MM) FROM WS-DAYS-WK.
           IF LEAP-YEAR AND WS-MM GREATER 02
               SUBTRACT 1 FROM WS-DAYS-WK.
           COMPUTE WS-DD = WS-DAYS-WK + 1.
       UNS-EXIT.
           EXIT.
