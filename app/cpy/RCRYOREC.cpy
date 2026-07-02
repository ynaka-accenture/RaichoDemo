      *----------------------------------------------------------------
      * RCRYOREC 料金・請求 (256 BYTE)
      *----------------------------------------------------------------
       01  RYO-REC.
           05  RYO-SPT-NO          PIC X(22).
           05  RYO-SEIKYU-YM       PIC 9(6).
           05  RYO-SIYORYO         PIC S9(6) COMP-3.
           05  RYO-KIHON           PIC S9(7) COMP-3.
           05  RYO-DAN1            PIC S9(7) COMP-3.
           05  RYO-DAN2            PIC S9(7) COMP-3.
           05  RYO-DAN3            PIC S9(7) COMP-3.
           05  RYO-NENCHO          PIC S9(7) COMP-3.
           05  RYO-SAIENE          PIC S9(7) COMP-3.
           05  RYO-WARIBIKI        PIC S9(5) COMP-3.
      *      税率世代: 旧外税レコード残存 (F-02)
           05  RYO-ZEI-SEDAI       PIC X(2).
           05  RYO-ZEIGAKU         PIC S9(7) COMP-3.
           05  RYO-GOKEI           PIC S9(9) COMP-3.
           05  RYO-SEIKYU-KBN      PIC X(1).
           05  RYO-NYUKIN-FLG      PIC X(1).
      *      予備域: 種別により金額/フラグ二重利用 (A-10)
           05  RYO-YOBI            PIC X(8).
           05  RYO-YOBI-P REDEFINES RYO-YOBI
                                   PIC S9(13)V99 COMP-3.
           05  RYO-KENSHIN-BI      PIC 9(6).
      *      日割日数: 除数 (J-05 台帳LB-006)
           05  RYO-NIWARI-NISSU    PIC 9(2).
           05  FILLER              PIC X(168).
