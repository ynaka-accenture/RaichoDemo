      *****************************************************************
      * RAKYK01C  契約照会 (TRAN=RKYK)         東西電力 電算部        *
      *---------------------------------------------------------------*
      * H05.04 初版  H18.04 型圧縮対応 (旧レイアウト表示分岐)         *
      * 適用開始 <1993 の契約は基本料金がゾーン9桁・以降 +4 バイト    *
      * 後方のため 表示編集を分ける (判別を忘れると化ける. 注意)      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RAKYK01C.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-RESP             PIC S9(8) COMP VALUE ZERO.
       77  WS-I                PIC 9(2)  VALUE ZERO.
       77  CD-GOKEI            PIC 9(5)  VALUE ZERO.
       77  CD-AMARI            PIC 9(2)  VALUE ZERO.
       77  CD-SYO              PIC 9(5)  VALUE ZERO.
       77  W-KIHON9            PIC 9(9)  VALUE ZERO.
       77  ED-KIHON            PIC ZZZ,ZZ9 VALUE ZERO.
       77  WS-J                PIC 9(2)  VALUE ZERO.
       77  WS-KEIKOKU          PIC 9(1)  VALUE ZERO.
       77  WS-CHIKU            PIC 9(2)  VALUE ZERO.
       01  SHOKAI-TOKEI.
           05  TOKEI-CNT       PIC 9(5) OCCURS 5 VALUE ZERO.
       COPY RCKYKMAP.
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
           05  FILLER          PIC X(256).
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  CA-STATE        PIC X(1).
           05  CA-USER         PIC X(8).
           05  CA-TRIES        PIC 9(1).
           05  FILLER          PIC X(190).
       PROCEDURE DIVISION.
       MAIN-RTN.
           IF EIBCALEN = ZERO
               EXEC CICS ABEND ABCODE('RK01') END-EXEC.
           IF CA-STATE = 'K'
               GO TO SHOKAI-SYORI.
           GO TO SYOKI-GAMEN.
      *----------------------------------------------------------------
       SYOKI-GAMEN.
           MOVE LOW-VALUES     TO KYK01MI.
           MOVE 'KYOKYU-TEN WO IRETE KUDASAI ' TO KYO-MSG.
           EXEC CICS SEND MAP('KYK01M') MAPSET('RKYK01M')
               FROM(KYK01MO) ERASE END-EXEC.
           MOVE 'K'            TO CA-STATE.
           EXEC CICS RETURN TRANSID('RKYK')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
       SHOKAI-SYORI.
           EXEC CICS RECEIVE MAP('KYK01M') MAPSET('RKYK01M')
               INTO(KYK01MI) RESP(WS-RESP) END-EXEC.
           IF WS-RESP NOT = ZERO
               EXEC CICS RETURN END-EXEC.
           IF KYI-SPT (1:1) = 'M'
               IF KYI-SPT (2:21) = SPACES
                   MOVE 'M' TO CA-STATE
                   EXEC CICS XCTL PROGRAM('RAMEN01C')
                       COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC
               END-IF
           END-IF.
           PERFORM SPT-KENSA   THRU SPT-KENSA-EX.
           EXEC CICS READ DATASET('KYKMAST')
               INTO(W-KYK-REC) RIDFLD(KYI-SPT)
               RESP(WS-RESP) END-EXEC.
           IF WS-RESP = 13
               MOVE SPACES     TO KYK01MO
               MOVE KYI-SPT    TO KYO-SPT
               MOVE 'KEIYAKU MIAJTORI (NOTFND)  ' TO KYO-MSG
               GO TO OUTO-HENSHIN.
           IF WS-RESP NOT = ZERO
               EXEC CICS ABEND ABCODE('RK02') END-EXEC.
           PERFORM TOKEI-KASAN  THRU TOKEI-KASAN-EX.
           PERFORM KEKKA-KENSA  THRU KEKKA-KENSA-EX.
           PERFORM HYOJI-HENSYU THRU HYOJI-HENSYU-EX.
       OUTO-HENSHIN.
           EXEC CICS SEND MAP('KYK01M') MAPSET('RKYK01M')
               FROM(KYK01MO) END-EXEC.
           MOVE 'K'            TO CA-STATE.
           EXEC CICS RETURN TRANSID('RKYK')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
      * 照会統計 (地区帯別. 端末セッション内のみ)
      *----------------------------------------------------------------
       TOKEI-KASAN             SECTION.
       TKS-010.
           IF KYI-SPT (3:2) NOT NUMERIC
               GO TO TOKEI-KASAN-EX.
           MOVE KYI-SPT (3:2)  TO WS-CHIKU.
           MOVE 1              TO WS-J.
       TKS-020.
           IF WS-J GREATER 4
               GO TO TKS-030.
           IF WS-CHIKU NOT GREATER WS-J * 12
               ADD 1 TO TOKEI-CNT (WS-J)
               GO TO TOKEI-KASAN-EX.
           ADD 1               TO WS-J.
           GO TO TKS-020.
       TKS-030.
           ADD 1               TO TOKEI-CNT (5).
       TOKEI-KASAN-EX.
           EXIT.
      *----------------------------------------------------------------
      * 取得結果の整合検査 (画面表示前の防衛 -- H18)
      *----------------------------------------------------------------
       KEKKA-KENSA             SECTION.
       KKK-010.
           MOVE ZERO           TO WS-KEIKOKU.
           IF WK-SPT NOT = KYI-SPT
               MOVE 9 TO WS-KEIKOKU
               GO TO KEKKA-KENSA-EX.
           IF WK-JYU NOT NUMERIC
               MOVE 9 TO WS-KEIKOKU
               GO TO KEKKA-KENSA-EX.
           IF WK-SYU NOT = '10' AND WK-SYU NOT = '11' AND
              WK-SYU NOT = '20' AND WK-SYU NOT = '99'
               MOVE 9 TO WS-KEIKOKU
               GO TO KEKKA-KENSA-EX.
           IF WK-KAISI NOT NUMERIC
               MOVE 9 TO WS-KEIKOKU
               GO TO KEKKA-KENSA-EX.
           IF WK-SYU = '99'
               MOVE 1 TO WS-KEIKOKU
               GO TO KEKKA-KENSA-EX.
           IF WK-TEISI = '1'
               MOVE 2 TO WS-KEIKOKU
               GO TO KEKKA-KENSA-EX.
           IF WK-SYURYO NOT = 99991231 AND
              WK-SYURYO NOT = 99999999 AND
              WK-SYURYO NOT = ZERO
               IF WK-SYURYO NOT NUMERIC
                   MOVE 9 TO WS-KEIKOKU
               ELSE
                   IF WK-SYURYO LESS 20300101
                       MOVE 3 TO WS-KEIKOKU
                   END-IF
               END-IF
           END-IF.
       KEKKA-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * 表示編集: 旧レイアウトは基本料金がゾーン9桁 (+4 ずれ)
      *----------------------------------------------------------------
       HYOJI-HENSYU            SECTION.
       HYH-010.
           MOVE SPACES         TO KYK01MO.
           MOVE WK-SPT         TO KYO-SPT.
           MOVE WK-JYU         TO KYO-JYU.
           MOVE WK-SYU         TO KYO-SYU.
           MOVE WK-KAISI       TO KYO-KAISI.
           MOVE WK-TEISI       TO KYO-FLG.
           IF WK-KAISI LESS 19930101
               GO TO HYH-KYU.
      *    新: PACKED 5バイト (H18 型圧縮後の現行形式)
           MOVE WK-KIHON-P     TO W-KIHON9.
           MOVE W-KIHON9       TO ED-KIHON.
           MOVE ED-KIHON       TO KYO-KIHON (1:7).
           PERFORM MSG-HENSYU THRU MSG-HENSYU-EX.
           GO TO HYOJI-HENSYU-EX.
       HYH-KYU.
      *    旧: ゾーン9桁を直接編集. 旧様式である旨を表示
           MOVE W-KYK-REC (60:9) TO W-KIHON9.
           MOVE W-KIHON9       TO ED-KIHON.
           MOVE ED-KIHON       TO KYO-KIHON (1:7).
           MOVE 'KY'           TO KYO-KIHON (8:2).
           MOVE 'SHOKAI KANRYO (KYU-LAYOUT)  ' TO KYO-MSG.
           IF WS-KEIKOKU = 2
               MOVE 'TEISHI-CHU (KYU-LAYOUT)     ' TO KYO-MSG.
       HYOJI-HENSYU-EX.
           EXIT.
      *----------------------------------------------------------------
       MSG-HENSYU              SECTION.
       MSH-010.
           IF WS-KEIKOKU = ZERO
               MOVE 'SHOKAI KANRYO               ' TO KYO-MSG
               GO TO MSG-HENSYU-EX.
           IF WS-KEIKOKU = 1
               MOVE 'SHOKAI KANRYO (TEST KEIYAKU)' TO KYO-MSG
               GO TO MSG-HENSYU-EX.
           IF WS-KEIKOKU = 2
               MOVE 'SHOKAI KANRYO (TEISHI-CHU)  ' TO KYO-MSG
               GO TO MSG-HENSYU-EX.
           IF WS-KEIKOKU = 3
               MOVE 'SHOKAI KANRYO (SYURYO YOTEI)' TO KYO-MSG
               GO TO MSG-HENSYU-EX.
           MOVE 'DATA IJO: KANRI RENRAKU     ' TO KYO-MSG.
       MSG-HENSYU-EX.
           EXIT.
      *----------------------------------------------------------------
      * 供給地点検査 (検査数字は共通化前の複製がここにもある)
      *----------------------------------------------------------------
       SPT-KENSA               SECTION.
       SPK-010.
           IF KYI-SPT = SPACES OR KYI-SPT = LOW-VALUES
               GO TO SPK-NG.
           IF KYI-SPT (1:2) NOT = '03'
               GO TO SPK-NG.
           IF KYI-SPT (1:1) = LOW-VALUE
               GO TO SPK-NG.
           IF KYI-SPT (3:2) NOT NUMERIC
               GO TO SPK-NG.
           IF KYI-SPT (3:2) LESS '01' OR KYI-SPT (3:2) GREATER '47'
               GO TO SPK-NG.
           IF KYI-SPT (5:16) NOT NUMERIC
               GO TO SPK-NG.
           IF KYI-SPT (21:2) NOT NUMERIC
               GO TO SPK-NG.
           MOVE ZERO           TO CD-GOKEI.
           MOVE 1              TO WS-I.
       SPK-020.
           IF WS-I GREATER 20
               GO TO SPK-030.
           IF WS-I GREATER 22
               GO TO SPK-NG.
           IF KYI-SPT (WS-I:1) NOT NUMERIC
               GO TO SPK-NG.
           COMPUTE CD-GOKEI = CD-GOKEI
               + FUNCTION NUMVAL (KYI-SPT (WS-I:1)).
           ADD 1               TO WS-I.
           GO TO SPK-020.
       SPK-030.
           DIVIDE CD-GOKEI BY 97 GIVING CD-SYO
               REMAINDER CD-AMARI.
           IF CD-AMARI NOT = FUNCTION NUMVAL (KYI-SPT (21:2))
               GO TO SPK-NG.
           GO TO SPT-KENSA-EX.
       SPK-NG.
           MOVE SPACES         TO KYK01MO.
           MOVE KYI-SPT        TO KYO-SPT.
           MOVE 'KENSA SUJI AYAMARI          ' TO KYO-MSG.
           GO TO OUTO-HENSHIN.
       SPT-KENSA-EX.
           EXIT.
