      *****************************************************************
      * RBRYO00C  電気料金計算 (旧版)         (株)東西電力 電算部     *
      *---------------------------------------------------------------*
      * S60.04 初版  S62.10 従量単価改定  H09.03 消費税5%->据置3%判断 *
      * ※本体は H11 に RBRYO01C へ移行済. 本プログラムは年度末の     *
      *   旧方式検算 (経理監査資料) にのみ使用する                    *
      * ※H15 以降 ソース修正なし. 翻訳記録簿の日付矛盾は調査中       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RBRYO00C.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370
           MEMORY SIZE 512000 CHARACTERS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KENIN   ASSIGN TO 'app/data/portable/KENFILE.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KEN.
           SELECT RYOOLD  ASSIGN TO 'app/data/portable/RYOOLD.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-OLD.
           SELECT KENSAN  ASSIGN TO 'app/data/portable/RYOKSN.dat'
               ORGANIZATION SEQUENTIAL FILE STATUS IS ST-KSN.
       DATA DIVISION.
       FILE SECTION.
       FD  KENIN
           LABEL RECORDS ARE STANDARD
           DATA RECORDS ARE KEN-REC
           RECORD CONTAINS 128 CHARACTERS.
       COPY RCKENREC.
       FD  RYOOLD
           LABEL RECORDS ARE STANDARD
           DATA RECORDS ARE OLD-REC
           RECORD CONTAINS 100 CHARACTERS.
       01  OLD-REC.
           05  OLD-SPT         PIC X(22).
           05  OLD-YM          PIC 9(6).
           05  OLD-SIYO        PIC 9(6).
           05  OLD-KIHON       PIC 9(7).
           05  OLD-JURYO       PIC 9(7).
           05  OLD-ZEI         PIC 9(7).
           05  OLD-GOKEI       PIC 9(9).
           05  FILLER          PIC X(36).
       FD  KENSAN
           LABEL RECORDS ARE STANDARD
           DATA RECORDS ARE KSN-REC
           RECORD CONTAINS 40 CHARACTERS.
       01  KSN-REC             PIC X(40).
       WORKING-STORAGE SECTION.
       77  ST-KEN              PIC XX    VALUE '00'.
       77  ST-OLD              PIC XX    VALUE '00'.
       77  ST-KSN              PIC XX    VALUE '00'.
       77  PRM-AREA            PIC X(20) VALUE SPACE.
       77  PRM-YM              PIC 9(6)  VALUE ZERO.
       77  YOMI-KENSU          PIC 9(7)  VALUE ZERO.
       77  TAISYO-KENSU        PIC 9(7)  VALUE ZERO.
       77  KAKIDASI-KENSU      PIC 9(7)  VALUE ZERO.
       77  HAJIKI-KENSU        PIC 9(7)  VALUE ZERO.
       77  SIYO-RYO            PIC 9(6)  VALUE ZERO.
       77  KIHON-KIN           PIC 9(7)  VALUE ZERO.
       77  JURYO-KIN           PIC 9(7)  VALUE ZERO.
       77  ZEI-KIN             PIC 9(7)  VALUE ZERO.
       77  GOKEI-KIN           PIC 9(9)  VALUE ZERO.
       77  SOU-GOKEI           PIC 9(11) VALUE ZERO.
      *    旧単価 (S62.10 改定のまま. 単価表ファイルは使わない)
       77  KIHON-TANKA         PIC 9(4)  VALUE 0600.
       77  JURYO-TANKA         PIC 9(4)V99 VALUE 0021.00.
       77  ZEI-RITU            PIC V99   VALUE .03.
       77  KENSAN-KIN          PIC 9(7)  VALUE ZERO.
       77  KENSAN-ZAN          PIC 9(6)  VALUE ZERO.
       77  KENSAN-NG           PIC 9(7)  VALUE ZERO.
       77  HYAKU-TANKA         PIC 9(6)  VALUE 002100.
       77  KAIKYU-1            PIC 9(7)  VALUE ZERO.
       77  KAIKYU-2            PIC 9(7)  VALUE ZERO.
       77  KAIKYU-3            PIC 9(7)  VALUE ZERO.
       77  KAIKYU-4            PIC 9(7)  VALUE ZERO.
       77  CD-GOKEI            PIC 9(5)  VALUE ZERO.
       77  CD-AMARI            PIC 9(2)  VALUE ZERO.
       77  CD-SYO              PIC 9(5)  VALUE ZERO.
       77  CD-KETA             PIC 9(2)  VALUE ZERO.
       77  CD-MOJI             PIC X     VALUE SPACE.
       77  CD-SUJI             PIC 9     VALUE ZERO.
       77  CD-FUICCHI          PIC 9(7)  VALUE ZERO.
       77  TUKI-KENSU          PIC 9(7)  VALUE ZERO.
       77  ZENKAI-YM           PIC 9(6)  VALUE ZERO.
       PROCEDURE DIVISION.
       SYORI-HONTAI SECTION 50.
       KAISI-P.
           ACCEPT PRM-AREA FROM COMMAND-LINE.
           IF PRM-AREA (1:6) NOT NUMERIC
               DISPLAY 'RBRYO00C E001 PARM AYAMARI'
               GO TO IJO-SYURYO.
           MOVE PRM-AREA (1:6) TO PRM-YM.
           OPEN INPUT KENIN.
           OPEN OUTPUT RYOOLD.
           OPEN OUTPUT KENSAN.
       YOMIKOMI-P.
           READ KENIN
               AT END GO TO SYUKEI-P.
           ADD 1               TO YOMI-KENSU.
           IF KEN-NENGETU NOT = PRM-YM
               GO TO YOMIKOMI-P.
           ADD 1               TO TAISYO-KENSU.
           IF KEN-SPT-NO (1:2) NOT = '03'
               GO TO HAJIKI-P.
           IF KEN-SPT-NO (3:2) LESS '01'
               GO TO HAJIKI-P.
           IF KEN-SPT-NO (3:2) GREATER '47'
               GO TO HAJIKI-P.
           IF KEN-SPT-NO (21:2) NOT NUMERIC
               GO TO HAJIKI-P.
           IF KEN-KENSHIN-BI NOT NUMERIC
               GO TO HAJIKI-P.
           IF KEN-KENSHIN-BI (3:2) LESS '01'
               GO TO HAJIKI-P.
           IF KEN-KENSHIN-BI (3:2) GREATER '12'
               GO TO HAJIKI-P.
           IF KEN-ZEN-SIJISU NOT NUMERIC
               GO TO HAJIKI-P.
           IF KEN-KON-SIJISU NOT NUMERIC
               GO TO HAJIKI-P.
           IF KEN-KENSHIN-KBN NOT = '1' AND
              KEN-KENSHIN-KBN NOT = '2' AND
              KEN-KENSHIN-KBN NOT = '3' AND
              KEN-KENSHIN-KBN NOT = '9'
               GO TO HAJIKI-P.
           MOVE KEN-SIYORYO    TO SIYO-RYO.
           IF SIYO-RYO GREATER 999999
               GO TO HAJIKI-P.
           IF KEN-KOKAN-FLG NOT = SPACE AND
              KEN-KOKAN-FLG NOT = 'K' AND
              KEN-KOKAN-FLG NOT = 'G'
               GO TO HAJIKI-P.
           IF KEN-NENGETU (5:2) LESS '01' OR
              KEN-NENGETU (5:2) GREATER '12'
               GO TO HAJIKI-P.
           IF KEN-KENSHIN-BI (5:2) GREATER '31'
               GO TO HAJIKI-P.
           GO TO KEISAN-P.
       HAJIKI-P.
           ADD 1               TO HAJIKI-KENSU.
           MOVE KEN-SPT-NO     TO KSN-REC.
           WRITE KSN-REC.
           GO TO YOMIKOMI-P.
      *----------------------------------------------------------------
      *    旧方式: 基本定額 + 従量単一段. 消費税は外税で加算する
      *----------------------------------------------------------------
       KEISAN-P.
           MOVE KIHON-TANKA    TO KIHON-KIN.
           COMPUTE JURYO-KIN = JURYO-TANKA * SIYO-RYO.
           COMPUTE ZEI-KIN ROUNDED =
               ( KIHON-KIN + JURYO-KIN ) * ZEI-RITU.
           COMPUTE GOKEI-KIN = KIHON-KIN + JURYO-KIN + ZEI-KIN.
           IF GOKEI-KIN GREATER 99999999
               GO TO HAJIKI-P.
           PERFORM KENSA-SUJI   THRU KENSA-SUJI-OWARI.
           PERFORM TUKI-KANRI   THRU TUKI-KANRI-OWARI.
           PERFORM JURYO-KENSAN THRU JURYO-KENSAN-OWARI.
           PERFORM KAIKYU-KEISU THRU KAIKYU-KEISU-OWARI.
       KAKIDASI-P.
           MOVE SPACES         TO OLD-REC.
           MOVE KEN-SPT-NO     TO OLD-SPT.
           MOVE KEN-NENGETU    TO OLD-YM.
           MOVE SIYO-RYO       TO OLD-SIYO.
           MOVE KIHON-KIN      TO OLD-KIHON.
           MOVE JURYO-KIN      TO OLD-JURYO.
           MOVE ZEI-KIN        TO OLD-ZEI.
           MOVE GOKEI-KIN      TO OLD-GOKEI.
           WRITE OLD-REC.
           ADD 1               TO KAKIDASI-KENSU.
           ADD GOKEI-KIN       TO SOU-GOKEI.
           GO TO YOMIKOMI-P.
      *----------------------------------------------------------------
      *    供給地点 検査数字の確かめ (共通化前の複製がここにも残る)
      *----------------------------------------------------------------
       KENSA-SUJI.
           MOVE ZERO           TO CD-GOKEI.
           MOVE 1              TO CD-KETA.
       KENSA-SUJI-KURIKAESI.
           IF CD-KETA GREATER 20
               GO TO KENSA-SUJI-HANTEI.
           MOVE KEN-SPT-NO (CD-KETA:1) TO CD-MOJI.
           IF CD-MOJI NOT NUMERIC
               GO TO KENSA-SUJI-HANTEI.
           MOVE CD-MOJI        TO CD-SUJI.
           ADD CD-SUJI         TO CD-GOKEI.
           ADD 1               TO CD-KETA.
           GO TO KENSA-SUJI-KURIKAESI.
       KENSA-SUJI-HANTEI.
           DIVIDE CD-GOKEI BY 97 GIVING CD-SYO
               REMAINDER CD-AMARI.
           IF KEN-SPT-NO (21:2) NOT NUMERIC
               ADD 1 TO CD-FUICCHI
               GO TO KENSA-SUJI-OWARI.
           IF CD-AMARI NOT = FUNCTION NUMVAL (KEN-SPT-NO (21:2))
               IF KEN-KENSHIN-KBN NOT = '9'
                   IF SIYO-RYO GREATER ZERO
                       ADD 1 TO CD-FUICCHI
                   END-IF
               END-IF
           END-IF.
       KENSA-SUJI-OWARI.
           EXIT.
      *----------------------------------------------------------------
      *    月区切り管理 (帳票の月合計行用. 現行は単月投入だが残置)
      *----------------------------------------------------------------
       TUKI-KANRI.
           IF ZENKAI-YM = ZERO
               MOVE KEN-NENGETU TO ZENKAI-YM
               MOVE ZERO       TO TUKI-KENSU.
           IF KEN-NENGETU NOT = ZENKAI-YM
               DISPLAY 'RBRYO00C I301 TUKI KIRIKAE ' ZENKAI-YM
               MOVE KEN-NENGETU TO ZENKAI-YM
               MOVE ZERO       TO TUKI-KENSU
               GO TO TUKI-KANRI-KASAN.
           GO TO TUKI-KANRI-KASAN.
       TUKI-KANRI-KASAN.
           ADD 1               TO TUKI-KENSU.
           IF TUKI-KENSU GREATER 9999998
               GO TO IJO-SYURYO.
       TUKI-KANRI-OWARI.
           EXIT.
      *----------------------------------------------------------------
      *    従量検算: 乗算結果を 100kWh 単位の加算で確かめる
      *    (S60 当時 乗算命令の障害があり以後この検算を残す)
      *----------------------------------------------------------------
       JURYO-KENSAN.
           MOVE ZERO           TO KENSAN-KIN.
           MOVE SIYO-RYO       TO KENSAN-ZAN.
       KENSAN-KURIKAESI.
           IF KENSAN-ZAN LESS 100
               GO TO KENSAN-HASU.
           ADD HYAKU-TANKA     TO KENSAN-KIN.
           SUBTRACT 100      FROM KENSAN-ZAN.
           GO TO KENSAN-KURIKAESI.
       KENSAN-HASU.
           IF KENSAN-ZAN GREATER 99
               GO TO IJO-SYURYO.
           IF KENSAN-ZAN GREATER ZERO
               COMPUTE KENSAN-KIN = KENSAN-KIN
                   + JURYO-TANKA * KENSAN-ZAN.
           IF KENSAN-KIN NOT = JURYO-KIN
               ADD 1           TO KENSAN-NG
               GO TO JURYO-KENSAN-OWARI.
           IF KENSAN-KIN GREATER 9999999
               GO TO IJO-SYURYO.
       JURYO-KENSAN-OWARI.
           EXIT.
      *----------------------------------------------------------------
      *    料金階級別計数 (S63 経営資料)
      *----------------------------------------------------------------
       KAIKYU-KEISU.
           IF GOKEI-KIN LESS 5000
               ADD 1 TO KAIKYU-1
               GO TO KAIKYU-KEISU-OWARI.
           IF GOKEI-KIN LESS 10000
               ADD 1 TO KAIKYU-2
               GO TO KAIKYU-KEISU-OWARI.
           IF GOKEI-KIN LESS 15000
               IF KEN-KENSHIN-KBN = '9'
                   ADD 1 TO KAIKYU-4
                   GO TO KAIKYU-KEISU-OWARI
               ELSE
                   IF SIYO-RYO GREATER 400
                       ADD 1 TO KAIKYU-3
                       GO TO KAIKYU-KEISU-OWARI
                   END-IF
               END-IF
               ADD 1 TO KAIKYU-3
               GO TO KAIKYU-KEISU-OWARI.
           ADD 1               TO KAIKYU-4.
       KAIKYU-KEISU-OWARI.
           EXIT.
      *----------------------------------------------------------------
       SYUKEI-P.
           CLOSE KENIN RYOOLD KENSAN.
           DISPLAY 'RBRYO00C YM=' PRM-YM ' YOMI=' YOMI-KENSU
                   ' TAISYO=' TAISYO-KENSU.
           DISPLAY 'KAKIDASI=' KAKIDASI-KENSU
                   ' HAJIKI=' HAJIKI-KENSU
                   ' SOUGOKEI=' SOU-GOKEI.
           DISPLAY 'CD-FUICCHI=' CD-FUICCHI.
           DISPLAY 'KENSAN-NG=' KENSAN-NG
                   ' KAIKYU=' KAIKYU-1 '/' KAIKYU-2 '/'
                   KAIKYU-3 '/' KAIKYU-4.
           MOVE 0              TO RETURN-CODE.
           STOP RUN.
       IJO-SYURYO.
           DISPLAY 'RBRYO00C IJO SYURYO'.
           MOVE 16             TO RETURN-CODE.
           STOP RUN.
      *----------------------------------------------------------------
       KENSA-YOBI SECTION 60.
      *    (S63 検査用. 現行では未使用のまま残置)
       KENSA-P.
           IF SIYO-RYO = ZERO
               ADD 0 TO HAJIKI-KENSU.
           GO TO KENSA-OWARI.
       KENSA-OWARI.
           EXIT.
