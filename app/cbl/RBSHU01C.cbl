      *****************************************************************
      * RBSHU01C  収納結果突合 (日次D060)     (株)東西電力 電算部     *
      *---------------------------------------------------------------*
      * H02.04 初版 (全銀フォーマット対応)  H27.02 訂正データ対応     *
      * 翻訳オプション ZWB 前提 (符号付きゾーンと文字の比較あり)      *
      * 突合キーは 銀行+支店+口座. 口座は理論上重複しうるが           *
      * 当社契約では発生していない (H02 検討会資料 4-2 参照)          *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBSHU01C.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SHUIN   ASSIGN TO 'app/data/portable/SHUIN.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-SHU.
           SELECT KYKMST  ASSIGN TO 'app/data/portable/KYKMAST.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KYK.
           SELECT SHUOUT  ASSIGN TO 'app/data/portable/SHUMEI.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-OUT.
       DATA DIVISION.
       FILE SECTION.
       FD  SHUIN RECORD CONTAINS 120 CHARACTERS.
       COPY RCZENGIN.
       FD  KYKMST RECORD CONTAINS 320 CHARACTERS.
       COPY RCKYKREC.
       FD  SHUOUT RECORD CONTAINS 100 CHARACTERS.
       COPY RCSHUREC.
       WORKING-STORAGE SECTION.
       77  ST-SHU              PIC XX VALUE '00'.
       77  ST-KYK              PIC XX VALUE '00'.
       77  ST-OUT              PIC XX VALUE '00'.
       77  WS-PRM              PIC X(20) VALUE SPACE.
       77  WS-PRM-YM           PIC 9(6)  VALUE ZERO.
       77  WK-IN-CNT           PIC 9(7)  VALUE ZERO.
       77  WK-MATCH            PIC 9(7)  VALUE ZERO.
       77  WK-UNMATCH          PIC 9(7)  VALUE ZERO.
       77  WK-MINUS            PIC 9(7)  VALUE ZERO.
       77  WK-FUNO             PIC 9(7)  VALUE ZERO.
       77  WK-KANA-NG          PIC 9(7)  VALUE ZERO.
       77  PLUS-SUM            PIC S9(13) COMP-3 VALUE ZERO.
       77  MINUS-SUM           PIC S9(13) COMP-3 VALUE ZERO.
       77  W-GAKU              PIC S9(10) VALUE ZERO.
       77  W-KETA10            PIC X      VALUE SPACE.
       77  W-FUGO              PIC S9     VALUE +1.
       77  W-SUJI              PIC 9      VALUE ZERO.
       77  WS-I                PIC 9(5)  VALUE ZERO.
       77  WS-J                PIC 9(5)  VALUE ZERO.
       77  WS-LO               PIC S9(5) VALUE ZERO.
       77  WS-HI               PIC S9(5) VALUE ZERO.
       77  WS-MD               PIC S9(5) VALUE ZERO.
       77  WS-HIT              PIC S9(5) VALUE ZERO.
       77  W-SRCH-KEY          PIC X(14) VALUE SPACE.
      *----------------------------------------------------------------
      *    口座->供給地点テーブル
      *----------------------------------------------------------------
       01  KZT-AREA.
           05  KZT-CNT         PIC S9(5) COMP-3 VALUE ZERO.
           05  KZT-E OCCURS 6000.
               10  KZT-KEY     PIC X(14).
               10  KZT-SPT     PIC X(22).
       01  KZT-SWAP.
           05  SW-KEY          PIC X(14).
           05  SW-SPT          PIC X(22).
       PROCEDURE DIVISION.
       MAIN-SEC                SECTION.
       MAIN-000.
           ACCEPT WS-PRM FROM COMMAND-LINE.
           IF WS-PRM (1:6) NOT NUMERIC
               DISPLAY 'RBSHU01C E001 PARM FUSEI'
               GO TO ABEND-RTN.
           MOVE WS-PRM (1:6)   TO WS-PRM-YM.
           PERFORM KZT-LOAD    THRU KZT-LOAD-EX.
           PERFORM KZT-SORT    THRU KZT-SORT-EX.
           OPEN INPUT SHUIN.
           OPEN OUTPUT SHUOUT.
       MAIN-LOOP.
           READ SHUIN
               AT END GO TO SYUKEI-RTN.
           ADD 1               TO WK-IN-CNT.
           IF ZG-DATA-KBN NOT = '2'
               GO TO MAIN-LOOP.
           PERFORM KANA-CHECK  THRU KANA-CHECK-EX.
           PERFORM GAKU-DECODE THRU GAKU-DECODE-EX.
           PERFORM KZT-SRCH    THRU KZT-SRCH-EX.
           IF WS-HIT = ZERO
               ADD 1           TO WK-UNMATCH
               GO TO MAIN-LOOP.
           PERFORM SHU-EDIT    THRU SHU-EDIT-EX.
           WRITE SHU-REC.
           ADD 1               TO WK-MATCH.
           GO TO MAIN-LOOP.
      *****************************************************************
       SYUKEI-RTN.
           CLOSE SHUIN SHUOUT.
           DISPLAY 'RBSHU01C YM=' WS-PRM-YM ' IN=' WK-IN-CNT
                   ' MATCH=' WK-MATCH ' UNMATCH=' WK-UNMATCH.
           DISPLAY 'KANANG=' WK-KANA-NG.
           DISPLAY 'MINUS=' WK-MINUS ' FUNO=' WK-FUNO
                   ' PLUS-SUM=' PLUS-SUM ' MINUS-SUM=' MINUS-SUM.
           IF WK-UNMATCH GREATER ZERO
               MOVE 8          TO RETURN-CODE
           ELSE
               MOVE 0          TO RETURN-CODE.
           STOP RUN.
       ABEND-RTN.
           DISPLAY 'RBSHU01C ABEND'.
           MOVE 16             TO RETURN-CODE.
           STOP RUN.
      *****************************************************************
      * 契約マスタから口座テーブル展開
      *  旧レイアウト(適用開始<1993)は口座域が4バイト後方 (型圧縮前)
      *****************************************************************
       KZT-LOAD                SECTION.
       KZL-010.
           OPEN INPUT KYKMST.
       KZL-020.
           READ KYKMST
               AT END GO TO KZL-090.
           IF KZT-CNT NOT LESS 6000
               GO TO KZL-090.
           ADD 1               TO KZT-CNT.
           MOVE KYK-SPT-NO     TO KZT-SPT (KZT-CNT).
           IF KYK-TEKIYO-KAISI LESS 19930101
               MOVE KYK-REC (69:4)  TO KZT-KEY (KZT-CNT) (1:4)
               MOVE KYK-REC (73:3)  TO KZT-KEY (KZT-CNT) (5:3)
               MOVE KYK-REC (77:7)  TO KZT-KEY (KZT-CNT) (8:7)
           ELSE
               MOVE KYK-GINKO       TO KZT-KEY (KZT-CNT) (1:4)
               MOVE KYK-SITEN       TO KZT-KEY (KZT-CNT) (5:3)
               MOVE KYK-KOZA        TO KZT-KEY (KZT-CNT) (8:7).
           GO TO KZL-020.
       KZL-090.
           CLOSE KYKMST.
       KZT-LOAD-EX.
           EXIT.
      *****************************************************************
      * 口座テーブル整列 (単純挿入法)
      *****************************************************************
       KZT-SORT                SECTION.
       KZS-010.
           MOVE 2              TO WS-I.
       KZS-020.
           IF WS-I GREATER KZT-CNT
               GO TO KZT-SORT-EX.
           MOVE KZT-E (WS-I)   TO KZT-SWAP.
           COMPUTE WS-J = WS-I - 1.
       KZS-030.
           IF WS-J LESS 1
               GO TO KZS-040.
           IF KZT-KEY (WS-J) NOT GREATER SW-KEY
               GO TO KZS-040.
           MOVE KZT-E (WS-J)   TO KZT-E (WS-J + 1).
           SUBTRACT 1        FROM WS-J.
           GO TO KZS-030.
       KZS-040.
           MOVE KZT-SWAP       TO KZT-E (WS-J + 1).
           ADD 1               TO WS-I.
           GO TO KZS-020.
       KZT-SORT-EX.
           EXIT.
      *****************************************************************
      * 口座二分探索
      *****************************************************************
       KZT-SRCH                SECTION.
       KZR-010.
           MOVE SPACES         TO W-SRCH-KEY.
           MOVE ZG-GINKO       TO W-SRCH-KEY (1:4).
           MOVE ZG-SITEN       TO W-SRCH-KEY (5:3).
           MOVE ZG-KOZA        TO W-SRCH-KEY (8:7).
           MOVE ZERO           TO WS-HIT.
           MOVE 1              TO WS-LO.
           MOVE KZT-CNT        TO WS-HI.
       KZR-020.
           IF WS-LO GREATER WS-HI
               GO TO KZT-SRCH-EX.
           COMPUTE WS-MD = ( WS-LO + WS-HI ) / 2.
           IF KZT-KEY (WS-MD) = W-SRCH-KEY
               MOVE WS-MD      TO WS-HIT
               GO TO KZT-SRCH-EX.
           IF KZT-KEY (WS-MD) LESS W-SRCH-KEY
               COMPUTE WS-LO = WS-MD + 1
           ELSE
               COMPUTE WS-HI = WS-MD - 1.
           GO TO KZR-020.
       KZT-SRCH-EX.
           EXIT.
      *****************************************************************
      * 受取人カナ名検査 (H07 追加: 先頭は カナ/英数/カ) のいずれか)
      *****************************************************************
       KANA-CHECK              SECTION.
       KAN-010.
           IF ZG-UKETORI-KANA = SPACES
               ADD 1 TO WK-KANA-NG
               GO TO KANA-CHECK-EX.
           IF ZG-UKETORI-KANA (1:1) NOT = SPACE
               IF ZG-UKETORI-KANA (1:1) NUMERIC
                   ADD 1 TO WK-KANA-NG
                   GO TO KANA-CHECK-EX
               ELSE
                   IF ZG-UKETORI-KANA (30:1) NOT = SPACE AND
                      ZG-UKETORI-KANA (30:1) NOT = ')'
                       CONTINUE
                   END-IF
               END-IF
           END-IF.
       KANA-CHECK-EX.
           EXIT.
      *****************************************************************
      * 金額復号: 末尾桁の符号オーバーパンチ (H27 訂正データ対応)
      *  { =+0 A-I=+1..+9  } =-0 J-R=-1..-9  それ以外は数字
      *****************************************************************
       GAKU-DECODE             SECTION.
       GKD-010.
           MOVE +1             TO W-FUGO.
           MOVE ZG-KINGAKU (10:1) TO W-KETA10.
           IF W-KETA10 NUMERIC
               MOVE W-KETA10   TO W-SUJI
               GO TO GKD-050.
           IF W-KETA10 = '{'
               MOVE 0 TO W-SUJI
               GO TO GKD-050.
           IF W-KETA10 = '}'
               MOVE 0 TO W-SUJI
               MOVE -1 TO W-FUGO
               GO TO GKD-050.
           IF W-KETA10 = 'A' MOVE 1 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'B' MOVE 2 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'C' MOVE 3 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'D' MOVE 4 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'E' MOVE 5 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'F' MOVE 6 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'G' MOVE 7 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'H' MOVE 8 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'I' MOVE 9 TO W-SUJI GO TO GKD-050.
           MOVE -1             TO W-FUGO.
           IF W-KETA10 = 'J' MOVE 1 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'K' MOVE 2 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'L' MOVE 3 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'M' MOVE 4 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'N' MOVE 5 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'O' MOVE 6 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'P' MOVE 7 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'Q' MOVE 8 TO W-SUJI GO TO GKD-050.
           IF W-KETA10 = 'R' MOVE 9 TO W-SUJI GO TO GKD-050.
           DISPLAY 'RBSHU01C E002 FUGO FUSEI: ' ZG-KINGAKU
           GO TO ABEND-RTN.
       GKD-050.
           IF ZG-KINGAKU (1:9) NOT NUMERIC
               DISPLAY 'RBSHU01C E003 GAKU FUSEI'
               GO TO ABEND-RTN.
           COMPUTE W-GAKU = FUNCTION NUMVAL (ZG-KINGAKU (1:9))
                          * 10 + W-SUJI.
           IF W-FUGO LESS ZERO
               COMPUTE W-GAKU = W-GAKU * -1
               ADD 1           TO WK-MINUS
               ADD W-GAKU      TO MINUS-SUM
           ELSE
               ADD W-GAKU      TO PLUS-SUM.
           IF ZG-FURIKAE-KEKKA NOT = '0'
               ADD 1           TO WK-FUNO.
       GAKU-DECODE-EX.
           EXIT.
      *****************************************************************
      * 収納明細編集
      *****************************************************************
       SHU-EDIT                SECTION.
       SHE-010.
           MOVE SPACES         TO SHU-REC.
           MOVE KZT-SPT (WS-HIT) TO SHU-SPT-NO.
           MOVE WS-PRM-YM      TO SHU-SEIKYU-YM.
           MOVE ZG-FURIKAE-BI  TO SHU-NYUKIN-BI.
           MOVE W-GAKU         TO SHU-NYUKIN-GAKU.
           MOVE '1'            TO SHU-HOHO.
           MOVE ZG-SHOKAI-NO   TO SHU-SHOKAI-NO.
           IF ZG-FURIKAE-KEKKA = '0'
               MOVE '1'        TO SHU-KESHIKOMI-FLG
           ELSE
               MOVE '0'        TO SHU-KESHIKOMI-FLG.
       SHU-EDIT-EX.
           EXIT.
