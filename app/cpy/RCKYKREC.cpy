      *----------------------------------------------------------------
      * RCKYKREC 需給契約マスタ (320 BYTE)  S60.04 初版
      * 増改築履歴: S60 予備30 -> H17スマート2 H21燃調1 H24再エネ1
      *   H26ポイント2 H28解約予定8 H30割引3 R02直販1 R04節電3 R06 4
      *   残予備 2バイト (これ以上の追加は型圧縮で捻出のこと)
      * H18.04 KIHON-KIN を PIC 9(9) -> S9(9) COMP-3 へ型変更(4byte捻出)
      *----------------------------------------------------------------
       01  KYK-REC.
      *      供給地点特定番号22桁: 桁数超過のため文字格納
      *      (3:2)=地区 (21:2)=検数 は部分参照で使用 (B-09)
           05  KYK-SPT-NO          PIC X(22).
           05  KYK-JYU-NO          PIC 9(10).
           05  KYK-SYUBETU         PIC X(2).
           05  KYK-YORYO           PIC S9(3)V9 COMP-3.
           05  KYK-DENRYOKU        PIC S9(5) COMP-3.
      *      単価世代: 旧システム由来レコードは符号F (B-03)
           05  KYK-TANKA-SEDAI     PIC S9(3) COMP-3.
           05  KYK-TEKIYO-KAISI    PIC 9(8).
      *      99991231=解約予定なし (E-02)
           05  KYK-TEKIYO-SYURYO   PIC 9(8).
           05  KYK-TEISI-FLG       PIC X(1).
      *      H18型圧縮: 旧 PIC 9(9) DISPLAY (B-12)
           05  KYK-KIHON-KIN       PIC S9(9) COMP-3.
           05  KYK-FURIKAE.
               10  KYK-GINKO       PIC 9(4).
               10  KYK-SITEN       PIC 9(3).
               10  KYK-YOKIN-SYU   PIC 9(1).
               10  KYK-KOZA        PIC 9(7).
               10  KYK-KOZA-KANA   PIC X(30).
           05  KYK-SAISYU-KENSHIN  PIC 9(6).
           05  KYK-ZEN-SIJISU      PIC 9(6).
      *----- S60当初 FILLER X(30) 侵食域 (B-12) ----------------------
           05  KYK-SMART-FLG       PIC X(1).
           05  KYK-DMD-TAISYO      PIC X(1).
           05  KYK-NENCHO-KBN      PIC X(1).
           05  KYK-SAIENE-KBN      PIC X(1).
           05  KYK-POINT-RITU      PIC S9(1)V99 COMP-3.
           05  KYK-KAIYAKU-YOTEI   PIC 9(8).
           05  KYK-WARIBIKI-KIN    PIC S9(5) COMP-3.
           05  KYK-CHOKU-KBN       PIC X(1).
           05  KYK-SETSUDEN-PT     PIC S9(5) COMP-3.
           05  KYK-DENKA-CD        PIC X(4).
           05  FILLER              PIC X(2).
      *---------------------------------------------------------------
      *      予備 (台帳上未使用)
           05  KYK-YOBI            PIC X(4).
           05  KYK-KOSHIN-BI       PIC 9(8).
           05  KYK-KOSHIN-PGM      PIC X(8).
           05  FILLER              PIC X(152).
