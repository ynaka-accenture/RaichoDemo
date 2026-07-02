      *****************************************************************
      * RAKYK02C  契約更新 (TRAN=RKY2)         東西電力 電算部        *
      *---------------------------------------------------------------*
      * H07.06 初版  H26.04 検証強化 (新検証を追加. 旧検証も並走)     *
      * 画面遷移: 1=キー入力 -> 2=変更入力 -> 3=確認 -> 書換え        *
      * 注意: 本体は H07 様式 (ピリオド終端) と H26 様式 (END-IF)     *
      *       が節単位で混在する. 改修時は節の年代に合わせること      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RAKYK02C.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-RESP             PIC S9(8) COMP VALUE ZERO.
       77  WS-I                PIC 9(2)  VALUE ZERO.
       77  CD-GOKEI            PIC 9(5)  VALUE ZERO.
       77  CD-AMARI            PIC 9(2)  VALUE ZERO.
       77  CD-SYO              PIC 9(5)  VALUE ZERO.
       77  KYU-NG              PIC 9(1)  VALUE ZERO.
       77  SHIN-NG             PIC 9(1)  VALUE ZERO.
       77  W-SYURYO            PIC 9(8)  VALUE ZERO.
       77  WS-J                PIC 9(2)  VALUE ZERO.
       77  WS-KENGEN           PIC 9(1)  VALUE ZERO.
       77  WS-NIJU             PIC 9(1)  VALUE ZERO.
       77  WS-KISOKU-NG        PIC 9(1)  VALUE ZERO.
       77  WS-MOJI-NG          PIC 9(1)  VALUE ZERO.
       77  W-KAIYAKU           PIC 9(8)  VALUE ZERO.
       77  W-SAISYU8           PIC 9(8)  VALUE ZERO.
       77  W-SAI-YY            PIC 9(2)  VALUE ZERO.
       77  WS-SAIKAKU-NG       PIC 9(1)  VALUE ZERO.
      *    更新権限表 (末尾桁 -> 可否. H26)
       01  KKEN-TBL-V.
           05  FILLER PIC X(2) VALUE '1Y'.
           05  FILLER PIC X(2) VALUE '2Y'.
           05  FILLER PIC X(2) VALUE '3Y'.
           05  FILLER PIC X(2) VALUE '4Y'.
           05  FILLER PIC X(2) VALUE '5N'.
           05  FILLER PIC X(2) VALUE '6N'.
           05  FILLER PIC X(2) VALUE '7N'.
           05  FILLER PIC X(2) VALUE '8Y'.
       01  KKEN-TBL REDEFINES KKEN-TBL-V.
           05  KKEN-E OCCURS 8.
               10  KKEN-KETA   PIC X(1).
               10  KKEN-KAHI   PIC X(1).
      *    更新通番: 初期値なし (疑似会話間で保持される前提 --
      *    実機では TRANSACTION 毎に初期化されるが動作していた)
       77  KOSHIN-TUBAN        PIC 9(4).
       COPY RCKY2MAP.
       01  W-KYK-REC           PIC X(320).
       01  W-KYK-R REDEFINES W-KYK-REC.
           05  WK-SPT          PIC X(22).
           05  WK-JYU          PIC 9(10).
           05  WK-SYU          PIC X(2).
           05  FILLER          PIC X(8).
           05  WK-KAISI        PIC 9(8).
           05  WK-SYURYO       PIC 9(8).
           05  WK-TEISI        PIC X(1).
           05  WK-KIHON-P      PIC S9(9) COMP-3.
           05  FILLER          PIC X(88).
           05  WK-KOSHIN-BI    PIC 9(8).
           05  WK-KOSHIN-PGM   PIC X(8).
           05  FILLER          PIC X(152).
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  CA-STATE        PIC X(1).
           05  CA-USER         PIC X(8).
           05  CA-TRIES        PIC 9(1).
           05  CA-KYK-KEY      PIC X(22).
           05  CA-NEW-SYURYO   PIC X(8).
           05  CA-NEW-TEISI    PIC X(1).
           05  CA-ZEN-KEY      PIC X(22).
           05  CA-TUBAN        PIC 9(4).
           05  FILLER          PIC X(133).
       PROCEDURE DIVISION.
       MAIN-RTN.
           IF EIBCALEN = ZERO
               EXEC CICS ABEND ABCODE('K201') END-EXEC.
           IF EIBCALEN = 120
      *        旧メニュー系 (H12 互換): 現行では到達しない
               GO TO KYU-IRIGUCHI.
           IF EIBCALEN NOT = 200
               EXEC CICS ABEND ABCODE('K202') END-EXEC.
           PERFORM JOTAI-BOUEI THRU JOTAI-BOUEI-EX.
           IF CA-STATE = '1'
               GO TO KEY-NYURYOKU.
           IF CA-STATE = '2'
               GO TO HENKO-NYURYOKU.
           IF CA-STATE = '3'
               GO TO KAKUNIN-SYORI.
           GO TO GAMEN-SYOKI.
      *----------------------------------------------------------------
      * 画面初期表示 (H07 様式)
      *----------------------------------------------------------------
       GAMEN-SYOKI.
           MOVE LOW-VALUES     TO KY201MI.
           PERFORM SYOKI-BUNGEN THRU SYOKI-BUNGEN-EX.
           EXEC CICS SEND MAP('KY201M') MAPSET('RKY201M')
               FROM(KY201MO) ERASE END-EXEC.
           MOVE '1'            TO CA-STATE.
           EXEC CICS RETURN TRANSID('RKY2')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
      * 状態1: キー受付 (H07 様式)
      *----------------------------------------------------------------
       KEY-NYURYOKU.
           EXEC CICS RECEIVE MAP('KY201M') MAPSET('RKY201M')
               INTO(KY201MI) RESP(WS-RESP) END-EXEC.
           IF WS-RESP NOT = ZERO
               EXEC CICS RETURN END-EXEC.
           IF KY2-SPT (1:1) = 'C'
               IF KY2-SPT (2:21) = SPACES
                   MOVE SPACES TO CA-KYK-KEY CA-ZEN-KEY
                   MOVE ZERO   TO CA-TUBAN
                   MOVE SPACES TO KY201MO
                   MOVE 'CLEAR SHIMASHITA                        '
                       TO KYO2-MSG
                   GO TO GAMEN-OKURI-1
               END-IF
           END-IF.
           IF KY2-SPT (1:1) = 'M'
               MOVE 'M' TO CA-STATE
               EXEC CICS XCTL PROGRAM('RAMEN01C')
                   COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
           PERFORM SPT-KENSA   THRU SPT-KENSA-EX.
           PERFORM KENGEN-KENSA THRU KENGEN-KENSA-EX.
           IF WS-KENGEN = ZERO
               MOVE SPACES TO KY201MO
               MOVE 'KOSHIN KENGEN NASHI                     '
                   TO KYO2-MSG
               GO TO GAMEN-OKURI-1.
           PERFORM NIJU-BOSHI  THRU NIJU-BOSHI-EX.
           IF WS-NIJU NOT = ZERO
               MOVE SPACES TO KY201MO
               MOVE 'DOU-KEY RENZOKU: NIJU KOSHIN BOSHI      '
                   TO KYO2-MSG
               GO TO GAMEN-OKURI-1.
           EXEC CICS READ DATASET('KYKMAST')
               INTO(W-KYK-REC) RIDFLD(KY2-SPT)
               RESP(WS-RESP) END-EXEC.
           IF WS-RESP = 13
               MOVE SPACES TO KY201MO
               MOVE 'KEIYAKU MIAJTORI                        '
                   TO KYO2-MSG
               GO TO GAMEN-OKURI-1.
           IF WS-RESP NOT = ZERO
               EXEC CICS ABEND ABCODE('K203') END-EXEC.
           IF WK-TEISI = '1'
               MOVE SPACES TO KY201MO
               MOVE KY2-SPT TO KYO2-SPT
               MOVE 'TEISHI-CHU: KOSHIN FUKA                 '
                   TO KYO2-MSG
               GO TO GAMEN-OKURI-1.
           PERFORM KYU-LAYOUT-KENSA THRU KYU-LAYOUT-KENSA-EX.
           IF WS-KISOKU-NG NOT = ZERO
               MOVE SPACES TO KY201MO
               MOVE KY2-SPT TO KYO2-SPT
               MOVE 'KYU-LAYOUT: GAMEN KOSHIN FUKA (BATCH)   '
                   TO KYO2-MSG
               GO TO GAMEN-OKURI-1.
      *    現況を表示し 変更入力へ
           MOVE SPACES         TO KY201MO.
           MOVE WK-SPT         TO KYO2-SPT.
           MOVE WK-SYURYO      TO KYO2-KYU-SYUR.
           MOVE WK-TEISI       TO KYO2-KYU-TEI.
           MOVE 'SYURYO-BI / TEISI WO IRETE KUDASAI      '
                               TO KYO2-MSG.
           MOVE WK-SPT         TO CA-KYK-KEY.
           MOVE '2'            TO CA-STATE.
           EXEC CICS SEND MAP('KY201M') MAPSET('RKY201M')
               FROM(KY201MO) END-EXEC.
           EXEC CICS RETURN TRANSID('RKY2')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
       GAMEN-OKURI-1.
           MOVE '1'            TO CA-STATE.
           EXEC CICS SEND MAP('KY201M') MAPSET('RKY201M')
               FROM(KY201MO) END-EXEC.
           EXEC CICS RETURN TRANSID('RKY2')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
      * 状態2: 変更値受付+二重検証 (H26 様式: END-IF/EVALUATE)
      *----------------------------------------------------------------
       HENKO-NYURYOKU.
           EXEC CICS RECEIVE MAP('KY201M') MAPSET('RKY201M')
               INTO(KY201MI) RESP(WS-RESP) END-EXEC.
           IF WS-RESP NOT = ZERO
               EXEC CICS RETURN END-EXEC.
           PERFORM MOJI-KENSA  THRU MOJI-KENSA-EX.
           IF WS-MOJI-NG NOT = ZERO
               MOVE 'SHIYO FUKA MOJI                         '
                   TO KYO2-MSG
               MOVE '2' TO CA-STATE
               GO TO GAMEN-OKURI-2
           END-IF
           PERFORM KYU-KENSHO  THRU KYU-KENSHO-EX.
           PERFORM SHIN-KENSHO THRU SHIN-KENSHO-EX.
           PERFORM SYUBETU-KISOKU THRU SYUBETU-KISOKU-EX.
           IF WS-KISOKU-NG NOT = ZERO
               MOVE 'SYUBETU KISOKU IHAN                     '
                   TO KYO2-MSG
               MOVE '2' TO CA-STATE
               GO TO GAMEN-OKURI-2
           END-IF
           IF KYU-NG NOT = SHIN-NG
      *        新旧検証の不一致は運用連絡のうえ旧を優先 (H26 取決め)
               MOVE 'KENSHO FUICCHI: KYU YUSEN               '
                   TO KYO2-MSG
               MOVE '2' TO CA-STATE
               GO TO GAMEN-OKURI-2
           END-IF
           IF KYU-NG = 1
               MOVE 'NYURYOKU AYAMARI                        '
                   TO KYO2-MSG
               MOVE '2' TO CA-STATE
               GO TO GAMEN-OKURI-2
           END-IF
           MOVE KY2-SYURYO     TO CA-NEW-SYURYO.
           MOVE KY2-TEISI      TO CA-NEW-TEISI.
           MOVE SPACES         TO KY201MO.
           MOVE CA-KYK-KEY     TO KYO2-SPT.
           MOVE CA-NEW-SYURYO  TO KYO2-SYURYO.
           MOVE 'YOROSHIKEREBA Y WO IRETE KUDASAI        '
                               TO KYO2-MSG.
           MOVE '3'            TO CA-STATE.
       GAMEN-OKURI-2.
           EXEC CICS SEND MAP('KY201M') MAPSET('RKY201M')
               FROM(KY201MO) END-EXEC.
           EXEC CICS RETURN TRANSID('RKY2')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
      * 状態3: 確認と書換え (H07 様式)
      *----------------------------------------------------------------
       KAKUNIN-SYORI.
           EXEC CICS RECEIVE MAP('KY201M') MAPSET('RKY201M')
               INTO(KY201MI) RESP(WS-RESP) END-EXEC.
           IF WS-RESP NOT = ZERO
               EXEC CICS RETURN END-EXEC.
           IF KY2-KAKUNIN NOT = 'Y'
               MOVE SPACES TO KY201MO
               MOVE 'TORIKESHI SHIMASHITA                    '
                   TO KYO2-MSG
               GO TO GAMEN-OKURI-3.
           EXEC CICS READ DATASET('KYKMAST')
               INTO(W-KYK-REC) RIDFLD(CA-KYK-KEY)
               RESP(WS-RESP) END-EXEC.
           IF WS-RESP NOT = ZERO
               EXEC CICS ABEND ABCODE('K204') END-EXEC.
           PERFORM SAIKAKUNIN  THRU SAIKAKUNIN-EX.
           IF WS-SAIKAKU-NG NOT = ZERO
               MOVE SPACES TO KY201MO
               MOVE 'TA-TANMATSU KOSHIN KANCHI: YARINAOSI    '
                   TO KYO2-MSG
               GO TO GAMEN-OKURI-3.
           PERFORM RIREKI-HENSHU THRU RIREKI-HENSHU-EX.
           MOVE CA-NEW-SYURYO  TO WK-SYURYO.
           IF CA-NEW-TEISI = '1'
               MOVE '1'        TO WK-TEISI.
           MOVE 20260701       TO WK-KOSHIN-BI.
           MOVE 'RAKYK02C'     TO WK-KOSHIN-PGM.
           ADD 1               TO KOSHIN-TUBAN.
           MOVE CA-KYK-KEY     TO CA-ZEN-KEY.
           MOVE KOSHIN-TUBAN   TO CA-TUBAN.
           EXEC CICS REWRITE DATASET('KYKMAST')
               FROM(W-KYK-REC) RESP(WS-RESP) END-EXEC.
           IF WS-RESP NOT = ZERO
               EXEC CICS ABEND ABCODE('K205') END-EXEC.
           MOVE SPACES         TO KY201MO.
           MOVE CA-KYK-KEY     TO KYO2-SPT.
           MOVE 'KOSHIN KANRYO                           '
                               TO KYO2-MSG.
       GAMEN-OKURI-3.
           MOVE '1'            TO CA-STATE.
           EXEC CICS SEND MAP('KY201M') MAPSET('RKY201M')
               FROM(KY201MO) END-EXEC.
           EXEC CICS RETURN TRANSID('RKY2')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
       KYU-IRIGUCHI.
           MOVE ' '            TO CA-STATE.
           GO TO GAMEN-SYOKI.
      *----------------------------------------------------------------
      * 初期文言の端末別分岐 (事務所/検針局/管理で案内を変える)
      *----------------------------------------------------------------
       SYOKI-BUNGEN            SECTION.
       SYB-010.
           IF EIBTRMID (1:1) = 'K'
               MOVE 'KANRI: KOSHIN KEY WO IRETE KUDASAI     '
                   TO KYO2-MSG
               GO TO SYOKI-BUNGEN-EX.
           IF EIBTRMID (1:1) = 'J'
               MOVE 'JIMU: KOSHIN KEY WO IRETE KUDASAI      '
                   TO KYO2-MSG
               GO TO SYOKI-BUNGEN-EX.
           IF EIBTRMID (1:1) = 'T'
               MOVE 'KOSHIN KEY WO IRETE KUDASAI             '
                   TO KYO2-MSG
               GO TO SYOKI-BUNGEN-EX.
           MOVE 'KOSHIN KEY WO IRETE KUDASAI (SONOTA)   '
                               TO KYO2-MSG.
       SYOKI-BUNGEN-EX.
           EXIT.
      *----------------------------------------------------------------
      * 状態値の防衛 (破損 COMMAREA からの回復 -- H26)
      *----------------------------------------------------------------
       JOTAI-BOUEI             SECTION.
       JTB-010.
           IF CA-STATE = '1'
               GO TO JOTAI-BOUEI-EX.
           IF CA-STATE = '2'
               IF CA-KYK-KEY = SPACES
                   MOVE ' ' TO CA-STATE
                   GO TO JOTAI-BOUEI-EX
               END-IF
               GO TO JOTAI-BOUEI-EX.
           IF CA-STATE = '3'
               IF CA-NEW-SYURYO = SPACES
                   MOVE ' ' TO CA-STATE
                   GO TO JOTAI-BOUEI-EX
               END-IF
               IF CA-KYK-KEY = SPACES
                   MOVE ' ' TO CA-STATE
                   GO TO JOTAI-BOUEI-EX
               END-IF
               GO TO JOTAI-BOUEI-EX.
           MOVE ' '            TO CA-STATE.
       JOTAI-BOUEI-EX.
           EXIT.
      *----------------------------------------------------------------
      * 書換え前 再確認 (H26: 楽観方式. 表示時点との相違を検出)
      *----------------------------------------------------------------
       SAIKAKUNIN              SECTION.
       SKN-010.
           MOVE ZERO           TO WS-SAIKAKU-NG.
           IF WK-SPT NOT = CA-KYK-KEY
               MOVE 1 TO WS-SAIKAKU-NG
               GO TO SAIKAKUNIN-EX.
           IF WK-TEISI = '1'
               MOVE 1 TO WS-SAIKAKU-NG
               GO TO SAIKAKUNIN-EX.
           IF WK-SYU = '99'
               MOVE 1 TO WS-SAIKAKU-NG
               GO TO SAIKAKUNIN-EX.
           IF WK-KOSHIN-BI NOT NUMERIC
               MOVE 1 TO WS-SAIKAKU-NG
               GO TO SAIKAKUNIN-EX.
           IF WK-KOSHIN-BI = 20260701
               IF WK-KOSHIN-PGM = 'RAKYK02C'
                   IF CA-ZEN-KEY = WK-SPT
                       IF CA-TUBAN = ZERO
                           MOVE 1 TO WS-SAIKAKU-NG
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF WK-KAISI LESS 19930101
               MOVE 1 TO WS-SAIKAKU-NG.
       SAIKAKUNIN-EX.
           EXIT.
      *----------------------------------------------------------------
      * レコード内履歴退避: 旧値を末尾予備域へ (S63 からの慣行)
      *----------------------------------------------------------------
       RIREKI-HENSHU           SECTION.
       RRK-010.
           IF W-KYK-REC (301:20) NOT = SPACES
               IF W-KYK-REC (301:1) NOT = 'H'
                   GO TO RRK-020
               END-IF
           END-IF
           GO TO RRK-030.
       RRK-020.
           MOVE SPACES         TO W-KYK-REC (301:20).
       RRK-030.
           MOVE 'H'            TO W-KYK-REC (301:1).
           MOVE WK-SYURYO      TO W-KYK-REC (302:8).
           MOVE WK-TEISI       TO W-KYK-REC (310:1).
           IF WK-KOSHIN-PGM = SPACES
               MOVE 'SHOKI   '  TO W-KYK-REC (311:8)
               GO TO RIREKI-HENSHU-EX.
           MOVE WK-KOSHIN-PGM  TO W-KYK-REC (311:8).
       RIREKI-HENSHU-EX.
           EXIT.
      *----------------------------------------------------------------
      * 更新権限 (末尾桁表引き -- H26 様式)
      *----------------------------------------------------------------
       KENGEN-KENSA            SECTION.
       KGN-010.
           MOVE ZERO           TO WS-KENGEN.
           IF CA-USER (1:4) NOT = 'USER'
               GO TO KENGEN-KENSA-EX.
           MOVE 1              TO WS-J.
       KGN-020.
           IF WS-J GREATER 8
               GO TO KENGEN-KENSA-EX.
           IF KKEN-KETA (WS-J) NOT = CA-USER (8:1)
               GO TO KGN-030.
           IF KKEN-KAHI (WS-J) = 'Y'
               MOVE 1 TO WS-KENGEN.
           GO TO KENGEN-KENSA-EX.
       KGN-030.
           ADD 1               TO WS-J.
           GO TO KGN-020.
       KENGEN-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * 二重更新防止 (直前と同一キーの連続更新を抑止 -- H26)
      *----------------------------------------------------------------
       NIJU-BOSHI              SECTION.
       NJB-010.
           MOVE ZERO           TO WS-NIJU.
           IF CA-ZEN-KEY = SPACES
               GO TO NIJU-BOSHI-EX.
           IF CA-ZEN-KEY NOT = KY2-SPT
               GO TO NIJU-BOSHI-EX.
           IF CA-TUBAN NOT NUMERIC
               GO TO NIJU-BOSHI-EX.
           IF CA-TUBAN GREATER ZERO
               MOVE 1 TO WS-NIJU.
       NIJU-BOSHI-EX.
           EXIT.
      *----------------------------------------------------------------
      * 旧レイアウト検査 (H18: 型圧縮前の契約は画面更新不可.
      *   後続項目が +4 ずれており 画面 REWRITE では壊すため)
      *----------------------------------------------------------------
       KYU-LAYOUT-KENSA        SECTION.
       KLK-010.
           MOVE ZERO           TO WS-KISOKU-NG.
           IF WK-KAISI NOT NUMERIC
               MOVE 1 TO WS-KISOKU-NG
               GO TO KYU-LAYOUT-KENSA-EX.
           IF WK-KAISI LESS 19930101
               MOVE 1 TO WS-KISOKU-NG.
       KYU-LAYOUT-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * 入力文字走査 (画面ゴミの検出 -- H26)
      *----------------------------------------------------------------
       MOJI-KENSA              SECTION.
       MJK-010.
           MOVE ZERO           TO WS-MOJI-NG.
           MOVE 1              TO WS-J.
       MJK-020.
           IF WS-J GREATER 32
               GO TO MOJI-KENSA-EX.
           IF KY201MI (WS-J:1) LESS SPACE
               MOVE 1 TO WS-MOJI-NG
               GO TO MOJI-KENSA-EX.
           ADD 1               TO WS-J.
           GO TO MJK-020.
       MOJI-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * 種別別更新規則 (H26): 種別ごとの制約へ振り分け
      *----------------------------------------------------------------
       SYUBETU-KISOKU          SECTION.
       SYK-010.
           MOVE ZERO           TO WS-KISOKU-NG.
           IF WK-SYU = '10'
               PERFORM KISOKU-10 THRU KISOKU-10-EX
               GO TO SYUBETU-KISOKU-EX.
           IF WK-SYU = '11'
               PERFORM KISOKU-11 THRU KISOKU-11-EX
               GO TO SYUBETU-KISOKU-EX.
           IF WK-SYU = '20'
               PERFORM KISOKU-20 THRU KISOKU-20-EX
               GO TO SYUBETU-KISOKU-EX.
           IF WK-SYU = '99'
               PERFORM KISOKU-99 THRU KISOKU-99-EX
               GO TO SYUBETU-KISOKU-EX.
           MOVE 1              TO WS-KISOKU-NG.
       SYUBETU-KISOKU-EX.
           EXIT.
      *    種別10 (従量電灯): 制約なし. 整合のみ
       KISOKU-10               SECTION.
       K10-010.
           PERFORM SYURYO-SEIGO THRU SYURYO-SEIGO-EX.
           PERFORM TEISI-KISOKU THRU TEISI-KISOKU-EX.
       KISOKU-10-EX.
           EXIT.
      *    種別11 (時間帯別): 停止入力は不可 (別画面)
       KISOKU-11               SECTION.
       K11-010.
           IF KY2-TEISI = '1'
               MOVE 1 TO WS-KISOKU-NG
               GO TO KISOKU-11-EX.
           PERFORM SYURYO-SEIGO THRU SYURYO-SEIGO-EX.
       KISOKU-11-EX.
           EXIT.
      *    種別20 (低圧電力): 大口は事務所K端末のみ
       KISOKU-20               SECTION.
       K20-010.
           PERFORM OGUCHI-KENSA THRU OGUCHI-KENSA-EX.
           IF WS-KISOKU-NG NOT = ZERO
               GO TO KISOKU-20-EX.
           PERFORM SYURYO-SEIGO THRU SYURYO-SEIGO-EX.
           PERFORM TEISI-KISOKU THRU TEISI-KISOKU-EX.
       KISOKU-20-EX.
           EXIT.
      *    種別99 (テスト): 画面からの更新一切不可
       KISOKU-99               SECTION.
       K99-010.
           MOVE 1              TO WS-KISOKU-NG.
       KISOKU-99-EX.
           EXIT.
      *----------------------------------------------------------------
      * 終了日整合: 解約予定・最終検針との前後関係
      *----------------------------------------------------------------
       SYURYO-SEIGO            SECTION.
       SSG-010.
           IF KY2-SYURYO NOT NUMERIC
               GO TO SYURYO-SEIGO-EX.
           MOVE KY2-SYURYO     TO W-SYURYO.
           IF W-SYURYO = 99991231
               GO TO SYURYO-SEIGO-EX.
           MOVE W-KYK-REC (128:8) TO W-KAIYAKU.
           IF W-KAIYAKU NOT NUMERIC
               MOVE 1 TO WS-KISOKU-NG
               GO TO SYURYO-SEIGO-EX.
           IF W-KAIYAKU = ZERO
               GO TO SSG-020.
           IF W-SYURYO GREATER W-KAIYAKU
               MOVE 1 TO WS-KISOKU-NG
               GO TO SYURYO-SEIGO-EX.
       SSG-020.
      *    最終検針は YYMMDD 6桁. 50 窓で西暦化して比較 (H26)
           IF W-KYK-REC (110:6) NOT NUMERIC
               MOVE 1 TO WS-KISOKU-NG
               GO TO SYURYO-SEIGO-EX.
           MOVE W-KYK-REC (110:2) TO W-SAI-YY.
           IF W-SAI-YY NOT LESS 50
               COMPUTE W-SAISYU8 = 19000000
                   + FUNCTION NUMVAL (W-KYK-REC (110:6))
           ELSE
               COMPUTE W-SAISYU8 = 20000000
                   + FUNCTION NUMVAL (W-KYK-REC (110:6)).
           IF W-SYURYO LESS W-SAISYU8
               MOVE 1 TO WS-KISOKU-NG.
       SYURYO-SEIGO-EX.
           EXIT.
      *----------------------------------------------------------------
      * 停止/再開規則: 停止には終了日 再開には停止中が前提
      *----------------------------------------------------------------
       TEISI-KISOKU            SECTION.
       TSK-010.
           IF KY2-TEISI = ' '
               GO TO TEISI-KISOKU-EX.
           IF KY2-TEISI = '1'
               IF KY2-SYURYO = '99991231'
                   MOVE 1 TO WS-KISOKU-NG
               END-IF
               GO TO TEISI-KISOKU-EX.
           IF KY2-TEISI = '0'
               IF WK-TEISI = '0'
                   GO TO TEISI-KISOKU-EX
               END-IF
               IF WK-SYURYO NOT = 99991231
                   IF WK-SYURYO NOT = ZERO
                       MOVE 1 TO WS-KISOKU-NG
                   END-IF
               END-IF
           END-IF.
       TEISI-KISOKU-EX.
           EXIT.
      *----------------------------------------------------------------
      * 大口検査: 基本料金が閾値超は K系端末のみ (5段の防衛)
      *----------------------------------------------------------------
       OGUCHI-KENSA            SECTION.
       OGK-010.
           IF WK-KAISI LESS 19930101
               GO TO OGUCHI-KENSA-EX.
           IF W-KYK-REC (60:1) = LOW-VALUE
               GO TO OGUCHI-KENSA-EX.
      *    大口 = 基本料金 100,000 円超のみ対象
           IF WK-KIHON-P LESS 100001
               GO TO OGUCHI-KENSA-EX.
           IF WK-SYU = '20'
               IF EIBTRMID (1:1) NOT = 'K'
                   IF EIBCALEN = 200
                       IF CA-USER (8:1) NOT = '8'
                           IF KY2-SYURYO NOT = '99991231'
                               MOVE 1 TO WS-KISOKU-NG
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.
       OGUCHI-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * 旧検証 (H07): 終了日は数値で月が 01-12 なら可
      *----------------------------------------------------------------
       KYU-KENSHO              SECTION.
       KYK-010.
           MOVE ZERO           TO KYU-NG.
           IF KY2-SYURYO = SPACES
               MOVE 1 TO KYU-NG
               GO TO KYU-KENSHO-EX.
           IF KY2-SYURYO (1:1) = '0'
               MOVE 1 TO KYU-NG
               GO TO KYU-KENSHO-EX.
           IF KY2-SYURYO NOT NUMERIC
               MOVE 1 TO KYU-NG
               GO TO KYU-KENSHO-EX.
           IF KY2-SYURYO (5:2) LESS '01'
               MOVE 1 TO KYU-NG
               GO TO KYU-KENSHO-EX.
           IF KY2-SYURYO (5:2) GREATER '12'
               MOVE 1 TO KYU-NG
               GO TO KYU-KENSHO-EX.
           IF KY2-TEISI NOT = '0' AND KY2-TEISI NOT = '1'
              AND KY2-TEISI NOT = ' '
               MOVE 1 TO KYU-NG.
       KYU-KENSHO-EX.
           EXIT.
      *----------------------------------------------------------------
      * 新検証 (H26): 過去日・遠未来・日の範囲まで見る
      *----------------------------------------------------------------
       SHIN-KENSHO             SECTION.
       SHK-010.
           MOVE ZERO           TO SHIN-NG.
           IF KY2-SYURYO NOT NUMERIC
               MOVE 1 TO SHIN-NG
           ELSE
               MOVE KY2-SYURYO TO W-SYURYO
               EVALUATE TRUE
                   WHEN W-SYURYO = 99991231
                       CONTINUE
                   WHEN W-SYURYO LESS 20260701
                       MOVE 1 TO SHIN-NG
                   WHEN W-SYURYO GREATER 20991231
                       MOVE 1 TO SHIN-NG
                   WHEN W-SYURYO (5:2) LESS '01'
                       MOVE 1 TO SHIN-NG
                   WHEN W-SYURYO (5:2) GREATER '12'
                       MOVE 1 TO SHIN-NG
                   WHEN W-SYURYO (7:2) LESS '01'
                       MOVE 1 TO SHIN-NG
                   WHEN W-SYURYO (7:2) GREATER '31'
                       MOVE 1 TO SHIN-NG
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF
           IF KY2-TEISI NOT = '0' AND KY2-TEISI NOT = '1'
              AND KY2-TEISI NOT = ' '
               MOVE 1 TO SHIN-NG
           END-IF.
       SHIN-KENSHO-EX.
           EXIT.
      *----------------------------------------------------------------
      * 供給地点検査 (検査数字複製: 第5箇所)
      *----------------------------------------------------------------
       SPT-KENSA               SECTION.
       SPK-010.
           IF KY2-SPT (1:1) = LOW-VALUE
               GO TO SPK-NG.
           IF KY2-SPT (1:2) NOT = '03'
               GO TO SPK-NG.
           IF KY2-SPT (3:2) LESS '01' OR KY2-SPT (3:2) GREATER '47'
               GO TO SPK-NG.
           IF KY2-SPT (3:2) NOT NUMERIC
               GO TO SPK-NG.
           IF KY2-SPT (5:16) NOT NUMERIC
               GO TO SPK-NG.
           IF KY2-SPT (21:2) NOT NUMERIC
               GO TO SPK-NG.
           MOVE ZERO           TO CD-GOKEI.
           MOVE 1              TO WS-I.
       SPK-020.
           IF WS-I GREATER 20
               GO TO SPK-030.
           COMPUTE CD-GOKEI = CD-GOKEI
               + FUNCTION NUMVAL (KY2-SPT (WS-I:1)).
           ADD 1               TO WS-I.
           GO TO SPK-020.
       SPK-030.
           DIVIDE CD-GOKEI BY 97 GIVING CD-SYO
               REMAINDER CD-AMARI.
           IF CD-AMARI NOT = FUNCTION NUMVAL (KY2-SPT (21:2))
               GO TO SPK-NG.
           GO TO SPT-KENSA-EX.
       SPK-NG.
           MOVE SPACES         TO KY201MO.
           MOVE 'KENSA SUJI AYAMARI                      '
                               TO KYO2-MSG.
           GO TO GAMEN-OKURI-1.
       SPT-KENSA-EX.
           EXIT.
