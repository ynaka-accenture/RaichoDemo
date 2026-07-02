      *****************************************************************
      * RASGN00C  サインオン (TRAN=RSGN)       東西電力 電算部        *
      *---------------------------------------------------------------*
      * H04.02 初版  H16.07 試行回数制限  R01.06 監査ログ強化         *
      * 監査ログの日付はシステム日付 (2桁年) をそのまま記録する       *
      *   (H04 当時の仕様のまま. 帳票側で世紀補完)                    *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RASGN00C.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-D6               PIC 9(6)  VALUE ZERO.
       77  WS-I                PIC 9(2)  VALUE ZERO.
       77  WS-HIT              PIC 9(2)  VALUE ZERO.
       77  WS-ONAJI            PIC 9(2)  VALUE ZERO.
       77  WS-PW-NG            PIC 9(1)  VALUE ZERO.
       77  WS-TRM-OK           PIC 9(1)  VALUE ZERO.
       COPY RCSGNMAP.
      *    利用者表 (内蔵. 部門異動の都度 再翻訳で反映)
       01  USR-TBL-V.
           05  FILLER PIC X(16) VALUE 'USER0001PASS0001'.
           05  FILLER PIC X(16) VALUE 'USER0002PASS0002'.
           05  FILLER PIC X(16) VALUE 'USER0003PASS0003'.
           05  FILLER PIC X(16) VALUE 'USER0004PASS0004'.
           05  FILLER PIC X(16) VALUE 'USER0005PASS0005'.
           05  FILLER PIC X(16) VALUE 'USER0006PASS0006'.
           05  FILLER PIC X(16) VALUE 'USER0007PASS0007'.
           05  FILLER PIC X(16) VALUE 'USER0008PASS0008'.
       01  USR-TBL REDEFINES USR-TBL-V.
           05  USR-E OCCURS 8.
               10  USR-ID      PIC X(8).
               10  USR-PW      PIC X(8).
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  CA-STATE        PIC X(1).
           05  CA-USER         PIC X(8).
           05  CA-TRIES        PIC 9(1).
           05  FILLER          PIC X(190).
       PROCEDURE DIVISION.
       MAIN-RTN.
           IF EIBCALEN = ZERO
               GO TO SYOKI-GAMEN.
           IF CA-STATE NOT = 'S'
               GO TO SYOKI-GAMEN.
           GO TO NYURYOKU-SYORI.
      *----------------------------------------------------------------
       SYOKI-GAMEN.
           MOVE LOW-VALUES     TO SGN00MI.
           MOVE SPACES         TO SGNO-MSG.
           MOVE 'USER/PASS WO IRETE KUDASAI' TO SGNO-MSG.
           PERFORM HIZUKE-SET  THRU HIZUKE-SET-EX.
           EXEC CICS SEND MAP('SGN00M') MAPSET('RSGN00M')
               FROM(SGN00MO) ERASE END-EXEC.
           MOVE 'S'            TO CA-STATE.
           MOVE ZERO           TO CA-TRIES.
           EXEC CICS RETURN TRANSID('RSGN')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
       NYURYOKU-SYORI.
           EXEC CICS RECEIVE MAP('SGN00M') MAPSET('RSGN00M')
               INTO(SGN00MI) END-EXEC.
           IF SGN-USERID = SPACES OR SGN-USERID = LOW-VALUES
               MOVE 'USER MINYURYOKU' TO SGNO-MSG
               GO TO SAINYURYOKU.
           IF SGN-USERID (1:4) NOT = 'USER'
               MOVE 'USER KEISHIKI AYAMARI' TO SGNO-MSG
               GO TO SAINYURYOKU.
           IF SGN-USERID (5:4) NOT NUMERIC
               MOVE 'USER KEISHIKI AYAMARI' TO SGNO-MSG
               GO TO SAINYURYOKU.
           IF SGN-PASSWD = SPACES
               MOVE 'PASS MINYURYOKU' TO SGNO-MSG
               GO TO SAINYURYOKU.
           PERFORM YOYAKU-ID   THRU YOYAKU-ID-EX.
           PERFORM TANMATSU-KENSA THRU TANMATSU-KENSA-EX.
           IF WS-TRM-OK = ZERO
               MOVE 'TANMATSU KYOKA NASHI' TO SGNO-MSG
               GO TO SAINYURYOKU.
           PERFORM PW-KISOKU   THRU PW-KISOKU-EX.
           IF WS-PW-NG NOT = ZERO
               MOVE 'PASS KISOKU IHAN' TO SGNO-MSG
               GO TO SAINYURYOKU.
           PERFORM USR-SAGASU  THRU USR-SAGASU-EX.
           IF WS-HIT = ZERO
               MOVE 'USER/PASS AYAMARI' TO SGNO-MSG
               GO TO SAINYURYOKU.
      *    認証成功: 監査記録 (2桁年のまま) -> メニューへ
           PERFORM HIZUKE-SET  THRU HIZUKE-SET-EX.
           MOVE SGN-USERID     TO CA-USER.
           MOVE 'M'            TO CA-STATE.
           EXEC CICS XCTL PROGRAM('RAMEN01C')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
       SAINYURYOKU.
           ADD 1               TO CA-TRIES.
           IF CA-TRIES GREATER 2
               MOVE 'KAISU CHOKA: TANMATSU LOCK' TO SGNO-MSG
               EXEC CICS SEND MAP('SGN00M') MAPSET('RSGN00M')
                   FROM(SGN00MO) END-EXEC
               EXEC CICS RETURN END-EXEC.
           EXEC CICS SEND MAP('SGN00M') MAPSET('RSGN00M')
               FROM(SGN00MO) END-EXEC.
           MOVE 'S'            TO CA-STATE.
           EXEC CICS RETURN TRANSID('RSGN')
               COMMAREA(DFHCOMMAREA) LENGTH(200) END-EXEC.
      *----------------------------------------------------------------
      * 予約利用者ID検査 (保守用IDの画面ログイン禁止 -- H16)
      *----------------------------------------------------------------
       YOYAKU-ID               SECTION.
       YYK-010.
           IF SGN-USERID = 'USER9999'
               GO TO YYK-NG.
           IF SGN-USERID = 'USER0000'
               GO TO YYK-NG.
           IF SGN-USERID (5:2) = '99'
               IF SGN-USERID (7:2) NOT = '99'
                   GO TO YYK-NG
               END-IF
           END-IF
           GO TO YOYAKU-ID-EX.
       YYK-NG.
           MOVE 'HOSHU ID: GAMEN LOGIN KINSHI' TO SGNO-MSG.
           GO TO SAINYURYOKU.
       YOYAKU-ID-EX.
           EXIT.
      *----------------------------------------------------------------
      * 端末許可検査 (H16: 事務所端末のみ許可)
      *----------------------------------------------------------------
       TANMATSU-KENSA          SECTION.
       TMK-010.
           MOVE ZERO           TO WS-TRM-OK.
           IF EIBTRMID (1:1) = 'T'
               GO TO TMK-020.
           IF EIBTRMID (1:1) = 'J'
               GO TO TMK-020.
           IF EIBTRMID (1:1) = 'K'
               GO TO TMK-020.
           GO TO TANMATSU-KENSA-EX.
       TMK-020.
           IF EIBTRMID (2:3) NOT NUMERIC
               GO TO TANMATSU-KENSA-EX.
           IF EIBTRMID (2:3) = '000'
               GO TO TANMATSU-KENSA-EX.
           IF EIBTRMID (2:3) = '999'
               GO TO TANMATSU-KENSA-EX.
           MOVE 1              TO WS-TRM-OK.
       TANMATSU-KENSA-EX.
           EXIT.
      *----------------------------------------------------------------
      * パスワード規則 (H16: 同一文字のみ・利用者ID一致を禁止)
      *----------------------------------------------------------------
       PW-KISOKU               SECTION.
       PWK-010.
           MOVE ZERO           TO WS-PW-NG.
           IF SGN-PASSWD = SGN-USERID
               MOVE 1 TO WS-PW-NG
               GO TO PW-KISOKU-EX.
           IF SGN-PASSWD (8:1) = SPACE
               MOVE 1 TO WS-PW-NG
               GO TO PW-KISOKU-EX.
           MOVE ZERO           TO WS-ONAJI.
           MOVE 2              TO WS-I.
       PWK-020.
           IF WS-I GREATER 8
               GO TO PWK-030.
           IF SGN-PASSWD (WS-I:1) = SGN-PASSWD (1:1)
               ADD 1 TO WS-ONAJI.
           ADD 1               TO WS-I.
           GO TO PWK-020.
       PWK-030.
           IF WS-ONAJI = 7
               MOVE 1 TO WS-PW-NG
               GO TO PW-KISOKU-EX.
           IF SGN-PASSWD (1:4) = '0000'
               IF SGN-PASSWD (5:4) = '0000'
                   IF SGN-USERID NOT = SPACES
                       MOVE 1 TO WS-PW-NG
                   END-IF
               END-IF
           END-IF.
       PW-KISOKU-EX.
           EXIT.
      *----------------------------------------------------------------
      * 日付編集: システム日付 2桁年 (H04 様式のまま)
      *----------------------------------------------------------------
       HIZUKE-SET              SECTION.
       HZK-010.
           ACCEPT WS-D6 FROM DATE.
           MOVE SPACES         TO SGNO-DATE.
           MOVE WS-D6 (1:2)    TO SGNO-DATE (1:2).
           MOVE '.'            TO SGNO-DATE (3:1).
           MOVE WS-D6 (3:2)    TO SGNO-DATE (4:2).
           MOVE '.'            TO SGNO-DATE (6:1).
           MOVE WS-D6 (5:2)    TO SGNO-DATE (7:2).
       HIZUKE-SET-EX.
           EXIT.
      *----------------------------------------------------------------
       USR-SAGASU              SECTION.
       USG-010.
           MOVE ZERO           TO WS-HIT.
           MOVE 1              TO WS-I.
       USG-020.
           IF WS-I GREATER 8
               GO TO USR-SAGASU-EX.
           IF USR-ID (WS-I) NOT = SGN-USERID
               GO TO USG-030.
           IF USR-PW (WS-I) NOT = SGN-PASSWD
               GO TO USG-030.
           MOVE WS-I           TO WS-HIT.
           GO TO USR-SAGASU-EX.
       USG-030.
           ADD 1               TO WS-I.
           GO TO USG-020.
       USR-SAGASU-EX.
           EXIT.
