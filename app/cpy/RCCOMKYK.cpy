      *----------------------------------------------------------------
      * RCCOMKYK 契約系 COMMAREA
      *   旧画面120byte/新画面200byte: RAKYK02CはEIBCALEN値で
      *   遷移元判定 (G-01). 旧呼出時の120超参照はG-07台帳参照
      *----------------------------------------------------------------
       01  COM-KYK-AREA.
           05  COM-KYK-OLD-PART.
               10  COM-SPT-NO      PIC X(22).
               10  COM-JYU-NO      PIC 9(10).
               10  COM-GAMEN-ID    PIC X(8).
               10  COM-MODE        PIC X(1).
               10  COM-PAGE-NO     PIC 9(3).
               10  COM-SEL-LINE    PIC 9(2).
               10  COM-MSG-CD      PIC X(4).
               10  FILLER          PIC X(70).
           05  COM-KYK-NEW-PART.
               10  COM-DMD-TAISYO  PIC X(1).
               10  COM-SMART-FLG   PIC X(1).
               10  COM-DENKA-CD    PIC X(4).
               10  COM-KENGEN-LV   PIC X(1).
               10  FILLER          PIC X(73).
