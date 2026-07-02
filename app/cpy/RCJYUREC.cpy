      *----------------------------------------------------------------
      * RCJYUREC 需要家マスタ (256 BYTE)  S60.04 初版
      *   H11.10 西暦4桁化  H21.04 ポイント対応  H25.06 WEB会員
      *----------------------------------------------------------------
       01  JYU-REC.
           05  JYU-NO              PIC 9(10).
      *      氏名: SO/SI込みDBCS/SBCS混在 (A-01)
           05  JYU-SIMEI           PIC X(40).
      *      先頭12バイトを「姓」とみなす (A-06 固定バイト切出し)
           05  JYU-SIMEI-R REDEFINES JYU-SIMEI.
               10  JYU-SEI         PIC X(12).
               10  JYU-MEI         PIC X(28).
      *      DBCS純形式の再定義 (A-08 G/X二重定義)
           05  JYU-SIMEI-G REDEFINES JYU-SIMEI
                                   PIC G(20) DISPLAY-1.
           05  JYU-KANA            PIC X(20).
           05  JYU-JUSHO           PIC X(60).
           05  JYU-TEL             PIC X(11).
           05  JYU-KEIYAKU-SU      PIC S9(3) COMP-3.
           05  JYU-KAISI-BI        PIC 9(8).
      *      解約日: 00000000=未設定 99999999=無期限 (E-02)
           05  JYU-KAIYAKU-BI      PIC 9(8).
           05  JYU-JOTAI-KBN       PIC X(1).
           05  JYU-SEIKYU-HOHO     PIC X(1).
      *----- 以下 S60当初 FILLER X(30) を保守で侵食 (B-12) -----------
           05  JYU-MAIL-FLG        PIC X(1).
      *      H21.04 ポイント残高 (バイト捻出のためPACKED)
           05  JYU-POINT-ZAN       PIC S9(7) COMP-3.
           05  JYU-WEB-ID          PIC X(8).
           05  JYU-KOFURI-KBN      PIC X(1).
           05  FILLER              PIC X(16).
      *---------------------------------------------------------------
           05  JYU-KOSHIN-BI       PIC 9(8).
           05  JYU-KOSHIN-ID       PIC X(8).
           05  FILLER              PIC X(49).
