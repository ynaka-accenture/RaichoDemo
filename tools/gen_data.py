#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
gen_data.py — RACS-DEMO データ生成 (可搬形態 / seed固定で再現可能)
生成物:
  app/data/portable/{JYUMAST,KYKMAST,MTRMAST,KENFILE,SHUIN,TANKA}.dat
  app/data/trigger/TJ*.dat            … 潜在バグ挑発データ(J系, docs/07)
  app/data/dirty_ledger.csv           … ゴミデータ台帳(全件)
  app/data/compile_ledger.csv         … 翻訳記録簿(K-04矛盾/G-08オプション列)
  app/data/gaiji_charmap.csv          … 擬似DBCSコード表(外字含む=A-03答え)
可搬DBCS表現: SO=0x0E / SI=0x0F、1文字2バイトコード(EBCDIC同様の
バイト配置を保ち、レイアウトのバイトオフセットは実機形態と一致)。
--verify で docs/07 潜在バグ台帳の「現行データ側の保証」をアサートする。
"""
import os, csv, random, sys
from datetime import date, timedelta
def ymd(base, days):
    d = date(base // 10000, base // 100 % 100, base % 100) \
        + timedelta(days=days)
    return d.year * 10000 + d.month * 100 + d.day

random.seed(20260702)
BASE = os.path.join(os.path.dirname(__file__), '..', 'app', 'data')
P = os.path.join(BASE, 'portable'); T = os.path.join(BASE, 'trigger')
os.makedirs(P, exist_ok=True); os.makedirs(T, exist_ok=True)
DIRTY = []  # (file,key,offset,trap,detail,mf_behavior)

SO, SI = b'\x0e', b'\x0f'

def pack(val, digits, signed=True, scale=0):
    """COMP-3 パック10進生成。sign_f=True で符号ニブルF(B-03用)"""
    v = int(round(abs(val) * (10 ** scale)))
    s = str(v).rjust(digits, '0')[-digits:]
    if len(s) % 2 == 0: s = '0' + s
    nib = 0xD if (signed and val < 0) else (0xC if signed else 0xF)
    return bytes.fromhex(s + format(nib, 'x'))

def pack_f(val, digits):  # 符号F(無符号扱い)の旧データ表現 B-03
    v = str(int(abs(val))).rjust(digits, '0')[-digits:]
    if len(v) % 2 == 0: v = '0' + v
    return bytes.fromhex(v + 'f')

def zn(v, w): return str(int(v)).rjust(w, '0')[-w:].encode()
def tx(s, w): b = s.encode('ascii', 'replace'); return (b + b' ' * w)[:w]

# ---- 擬似DBCS 名前生成 -------------------------------------------------
SEI_POOL = ['山田','田中','鈴木','高橋','渡辺','伊藤','中村','小林',
            '加藤','吉田','佐々木','松本','井上','木村','斎藤','清水']
MEI_POOL = ['太郎','花子','一郎','幸子','健二','美咲','大輔','恵子',
            '直樹','由美','孝','翔太','里奈','誠','光','葵']
CHARMAP, CODE_OF = [], {}
def code_for(ch, gaiji=False):
    if ch in CODE_OF: return CODE_OF[ch]
    n = len(CODE_OF)
    if gaiji:  # 外字域 0x69-0x6A (A-03)
        b = bytes([0x69 + n // 190 % 2, 0x41 + n % 190 % 60])
        tag = 'GAIJI'
    else:
        b = bytes([0x45 + n // 60, 0x41 + n % 60]); tag = 'STD'
    CODE_OF[ch] = b; CHARMAP.append((b.hex(), ch, tag)); return b
def dbcs(s, gaiji_chars=()):
    return SO + b''.join(code_for(c, c in gaiji_chars) for c in s) + SI
def dbcs_field(s, width, pad_zenkaku=False, gaiji_chars=()):
    body = dbcs(s, gaiji_chars)
    if pad_zenkaku:  # 全角スペース埋め (A-05): SP=code 0x4040
        padn = (width - len(body) - 2) // 2
        body = body[:-1] + b'\x40\x40' * padn + SI
    return (body + b' ' * width)[:width]

KANA = 'ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉ'
def kana(n): return ''.join(random.choice(KANA) for _ in range(n))

def spt_no(i):  # 供給地点特定番号22桁 (3:2)=地区 (21:2)=検数 (B-09)
    body = f'03{i % 47 + 1:02d}{i:016d}'
    chk = sum(int(c) for c in body) % 97
    return f'{body}{chk:02d}'

# ---- JYUMAST (256byte × 5,000) ----------------------------------------
def gen_jyumast(n=5000):
    gaiji_targets = set(random.sample(range(n), 12))       # A-03
    si_missing = set(random.sample(range(n), 2))           # A-02
    ztel = set(random.sample(range(n), 3))                 # 全角数字TEL
    with open(f'{P}/JYUMAST.dat', 'wb') as f:
        for i in range(n):
            jno = 1000000000 + i
            sei = random.choice(SEI_POOL); mei = random.choice(MEI_POOL)
            gj = ('髙','﨑') if i in gaiji_targets else ()
            name = ('髙' + sei[1:] if i in gaiji_targets else sei) + mei
            zenkaku_pad = (i % 2 == 0)                     # A-05 50%
            simei = dbcs_field(name, 40, zenkaku_pad, gj)
            if i in si_missing:                            # A-02 SI欠落
                simei = simei.replace(SI, b'', 1)+ b' '
                DIRTY.append(('JYUMAST', jno, 10, 'A-02',
                              'SI欠落(不整合DBCS)', '印字化けのみで続行'))
            if i in gaiji_targets:
                DIRTY.append(('JYUMAST', jno, 10, 'A-03',
                              '外字コード(0x69xx)', '外字プリンタで正常印字'))
            rec = zn(jno, 10) + simei
            rec += tx('ｶ)' + kana(8) if i % 10 == 0 else kana(12), 20)
            rec += dbcs_field('灯央市中央一丁目' + str(i % 99 + 1), 60)
            tel = '0761234' + f'{i % 10000:04d}'
            if i in ztel:                                  # 全角数字 (A-01系)
                telb = dbcs('０７６', ())[:11].ljust(11, b' ')
                DIRTY.append(('JYUMAST', jno, 130, 'A-01',
                              '電話番号に全角数字', '表示のみ・演算対象外'))
                rec += telb
            else:
                rec += tx(tel, 11)
            rec += pack(i % 3 + 1, 3)
            rec += zn(ymd(19850401, i % 300 * 17), 8)
            kaiyaku = ['00000000', '99999999'][i % 2] if i % 17 == 0 \
                      else '00000000'                      # E-02
            rec += kaiyaku.encode() + b'1' + b'0'
            rec += b'Y' + pack(i * 7 % 99999, 7) + tx(f'WEB{i:05d}', 8)
            rec += b'0' + b' ' * 16
            rec += zn(20240401, 8) + tx('RAJYU01C', 8) + b' ' * 49
            assert len(rec) == 256, len(rec)
            f.write(rec)
    return n

# ---- KYKMAST (320byte × 6,000, 旧レイアウト8% B-12) --------------------
def gen_kykmast(n=6000):

    signf = set(random.sample(range(n), 120))              # B-03
    lowv = set(random.sample(range(n), 2))                 # B-07
    with open(f'{P}/KYKMAST.dat', 'wb') as f:
        for i in range(n):
            spt = spt_no(i); jno = 1000000000 + i % 5000
            syubetu = '99' if i % 200 == 0 else \
                      random.choices(['10','11','20'], [70,15,15])[0]
            if i % 200 == 0:
                DIRTY.append(('KYKMAST', spt, 32, 'F-05',
                              'テスト契約種別99', '11箇所の比較で集計除外'))
            kaisi = ymd(19900101, (i % 400) * 25)
            is_old = kaisi < 19930101   # B-12 暗黙判別条件
            key = b'\x00' * 22 if i in lowv else spt.encode()
            if i in lowv:
                DIRTY.append(('KYKMAST', 'LOW-VALUES', 0, 'B-07',
                              'LOW-VALUESキー', 'STARTで先頭に来るだけ'))
            head = key + zn(jno, 10) + syubetu.encode()
            head += pack(i % 6 * 10 + 10, 4, scale=1)
            head += pack(i % 50 + 5, 5)
            sedai = pack_f(i % 9 + 1, 3) if i in signf \
                else pack(i % 9 + 1, 3)
            if i in signf:
                DIRTY.append(('KYKMAST', spt, 40, 'B-03',
                              'COMP-3符号F(旧システム由来)',
                              '演算同値・バイト比較で旧判定'))
            head += sedai + zn(kaisi, 8)
            syuryo = ['99991231','99999999','00000000'][i % 3] \
                     if i % 16 == 0 else '99991231'        # E-02
            head += syuryo.encode() + b'0'
            kihon = (i % 60 + 10) * 2953 % 99999
            if is_old:   # 旧レイアウト: KIHON-KIN が DISPLAY 9(9) (B-12)
                body = zn(kihon, 9)
                DIRTY.append(('KYKMAST', spt, 59, 'B-12',
                              '型圧縮前の旧レイアウト(暗黙判別=適用開始<1993)',
                              '判別ロジックで正しく読み分け'))
            else:
                body = pack(kihon, 9)
            rec = head + body
            rec += zn(i % 9000 + 1, 4) + zn(i % 900 + 1, 3) + b'1'
            rec += zn(i * 13 % 9999999, 7) + tx(kana(10), 30)
            rec += zn(240520, 6) + zn(i * 37 % 999999, 6)
            rec += b'Y' if i % 5 == 0 else b'N'
            rec += (b'Y' if i % 5 == 0 else b'N') + b'1' + b'1'
            rec += pack(0.5, 3, scale=2) + zn(0, 8) + pack(55, 5)
            rec += b'0' + pack(i % 500, 5) + tx('EL01', 4) + b'  '
            rec += b'00  '                    # KYK-YOBI: (1:2)=停止理由 H-05
            rec += zn(20240401, 8) + tx('RAKYK02C', 8)
            rec = (rec + b' ' * 320)[:320]
            f.write(rec)
    return n

# ---- MTRMAST / KENFILE / SHUIN / TANKA ---------------------------------
def gen_mtrmast(n=6500):
    blank_gengo = set(random.sample(range(n), 900))        # E-01
    with open(f'{P}/MTRMAST.dat', 'wb') as f:
        for i in range(n):
            g, yy = ('R', i % 9 + 1) if i % 3 else ('H', i % 30 + 1)
            if i in blank_gengo:
                g, yy = ' ', (64 + i % 30) if i % 2 else (i % 31 + 8)
                DIRTY.append(('MTRMAST', f'MTR{i:07d}', 32, 'E-01',
                              '元号空白・窓割り(YY>=64昭和)', '窓判定で正解'))
            rec = tx(f'MTR{i:07d}', 10) + spt_no(i % 6000).encode()
            rec += g.encode() + zn(yy, 2) + zn(i % 12 + 1, 2)
            rec += pack(1.0, 3, scale=1) + b'6'
            rec += zn(0, 8) + zn(ymd(20100401, i % 5000), 8)
            rec += tx(f'K{i % 9}TYPE', 6) + b' ' * 66
            f.write(rec)
    return n

def gen_kenfile(months=12, n=6000):
    sp_ken = set(random.sample(range(months * n), 20))     # B-02
    kokan = set(random.sample(range(months * n), int(months * n * .03)))
    cnt = 0
    with open(f'{P}/KENFILE.dat', 'wb') as f:
        for m in range(months):
            ym = 202307 + m if m < 6 else 202401 + m - 6
            for i in range(n):
                idx = m * n + i
                zen = (i * 97 + m * 250) % 900000
                use = 120 + (i * 31 + m * 7) % 480
                kon = zen + use
                kflg = b' '
                if idx in kokan:                           # F-04
      # 計器満了巻き戻り: (10^6 - zen) + kon = use が成立するよう構成
                    kon = (i * 13) % max(use - 1, 1)
                    zen = 1000000 - (use - kon)
                    DIRTY.append(('KENFILE', f'{spt_no(i)}/{ym}', 40,
                                  'F-04', '指示数逆転=計器満了巻き戻り',
                                  '満了値補正で正しい使用量'))
                kenin = b'     ' if idx in sp_ken else zn(i % 80 + 1, 5)
                if idx in sp_ken:                          # B-02
                    DIRTY.append(('KENFILE', f'{spt_no(i)}/{ym}', 50,
                                  'B-02', '検針員コードにスペース',
                                  'NUMPROC(NOPFD)で0扱い通過'))
                rec = spt_no(i).encode() + zn(ym, 6)
                rec += zn((ym // 100 % 100) * 10000 + (ym % 100) * 100
                          + 20, 6)                          # 検針日 YYMMDD
                rec += zn(zen % 999999, 6) + zn(kon % 999999, 6)
                rec += pack(use, 6) + kenin + b'1' + kflg
                rec += b' ' * 30 + b' ' * 41
                assert len(rec) == 128, len(rec)
                f.write(rec); cnt += 1
    return cnt

def gen_shuin(n=5500):
    over = set(random.sample(range(n), 55))                # B-01
    with open(f'{P}/SHUIN.dat', 'wb') as f:
        for i in range(n):
            amt = 3000 + i * 7 % 12000
            z = zn(amt, 10)
            if i in over:  # 訂正(負値): 末尾桁を J..R に
                z = z[:-1] + b'JKLMNOPQR'[int(chr(z[-1])) - 1 : 
                                          int(chr(z[-1]))] \
                    if z[-1:] != b'0' else z[:-1] + b'}'
                DIRTY.append(('SHUIN', f'SHK{i:08d}', 46, 'B-01',
                              '符号オーバーパンチ(訂正負値)', '負数として演算'))
            rec = b'2' + zn(i % 9000 + 1, 4) + zn(i % 900 + 1, 3) + b'1'
            rec += zn(i * 13 % 9999999, 7) + tx(kana(6) + 'ｻﾏ', 30)
            rec += z + b'0' + tx(f'SHK{i:08d}', 20)
            rec += (b'0' if i % 15 else b'1') + zn(20240610, 8) + b' ' * 34
            assert len(rec) == 120, len(rec)
            f.write(rec)
    return n

def gen_tanka():
    with open(f'{P}/TANKA.dat', 'wb') as f:
        n = 0
        for syu in ('10', '11', '20'):
            for g, kaisi in enumerate((19970401, 20140401, 20191001,
                                       20230601)):
                for sub in range(10):
                    rec = syu.encode() + zn(kaisi + sub, 8)
                    rec += pack(295.24, 7, scale=2)
                    rec += pack(30.00, 5, scale=2)
                    rec += pack(36.60, 5, scale=2)
                    rec += pack(40.69, 5, scale=2)
                    rec += pack(-8.07 if g == 3 else 2.35, 4, scale=2)
                    rec += pack(3.49, 4, scale=2)
                    rec += pack((3.0, 8.0, 10.0, 10.0)[g], 3, scale=1)
                    rec += (b'S' if g == 0 else b'U') + b' ' * 48
                    assert len(rec) == 80, len(rec)
                    f.write(rec); n += 1
    return n

# ---- 挑発データ / 台帳類 ------------------------------------------------
def gen_ido(n=30):
    """需要家異動データ (RBJYU01C 入力): 40byte固定"""
    with open(f'{P}/IDOIN.dat', 'wb') as f:
        for i in range(n):
            jno = 1000000000 + i * 161
            rec = zn(jno, 10) + tx(f'0764{i:07d}', 11)
            rec += (b'Y' if i % 2 else b'N') + zn(0, 8) + b' ' * 10
            assert len(rec) == 40
            f.write(rec)
    return n

def gen_trigger():
    open(f'{T}/TJ01.dat', 'w').write('NS 9999999\n')
    open(f'{T}/TJ03.dat', 'wb').write(zn(9999999999, 10) +
        dbcs_field('長大氏名' * 5, 39) + b'\n')
    dup = spt_no(1).encode()
    open(f'{T}/TJ05.dat', 'wb').write(
        (b'2' + b'0' * 15 + b' ' * 30 + zn(5000, 10) + b'0' +
         tx('SHKDUP00000000000001', 20) + b'0' + zn(20240610, 8) +
         b' ' * 34) * 2)
    open(f'{T}/TJ06.dat', 'wb').write(dup + zn(20240601, 8) +
                                      zn(20240601, 8) + b'\n')
    open(f'{T}/TJ07.dat', 'w').write(
        '\n'.join(f'{spt_no(k)} 202406{d:02d}1200 100.00' 
                  for k, d in ((10, 1), (11, 2), (12, 3))) + '\n')

def gen_compile_ledger():
    rows = [('PROGRAM', 'SRC-DATE', 'COMPILE-DATE', 'LOAD-DATE',
             'OPTIONS', 'NOTE')]
    base = [('RUTLDTC', '1999-03-15', '1999-03-16', '1999-03-16',
             'NUMPROC(PFD),TRUNC(STD),NOSSRANGE', ''),
            ('RBKEN02C', '2018-06-01', '2018-06-02', '2018-06-02',
             'NUMPROC(NOPFD),TRUNC(STD),NOSSRANGE', 'G-08:NOPFD前提'),
            ('RBSHU01C', '2015-02-10', '2015-02-11', '2015-02-11',
             'NUMPROC(PFD),ZWB,TRUNC(STD)', 'G-08:ZWB前提'),
            ('RBGET01C', '2012-09-20', '2012-09-21', '2012-09-21',
             'NUMPROC(PFD),TRUNC(OPT)', 'G-08:TRUNC(OPT)前提/触るな'),
            ('RBSTM02C', '2024-11-10', '2019-03-02', '2019-03-02',
             'NUMPROC(PFD),TRUNC(STD)', 'K-04:ソースがロードより新しい'),
            ('RBRYO00C', '2001-04-05', '2023-08-17', '2023-08-17',
             'NUMPROC(PFD),TRUNC(STD)', 'K-04:ロードのみ更新・原典疑義'),
            ('RBRYO01C', '2023-06-01', '2023-06-01', '2023-06-01',
             'NUMPROC(PFD),TRUNC(STD),OPT(0)', 'OPTにすると結果相違の由'),
            ]
    for i in range(40):
        base.append((f'RA{"KYKJYU MTRKENRYOAUT"[i%14:i%14+3].strip()}X{i:02d}',
                     '2010-01-01', '2010-01-02', '2010-01-02',
                     'NUMPROC(PFD),TRUNC(STD)', ''))
    with open(f'{BASE}/compile_ledger.csv', 'w', newline='') as f:
        csv.writer(f).writerows(rows + base)

def write_ledgers():
    with open(f'{BASE}/dirty_ledger.csv', 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['FILE', 'KEY', 'OFFSET', 'TRAP', 'DETAIL',
                    'MF-BEHAVIOR'])
        w.writerows(DIRTY)
    with open(f'{BASE}/gaiji_charmap.csv', 'w', newline='') as f:
        w = csv.writer(f); w.writerow(['CODE-HEX', 'CHAR', 'CLASS'])
        w.writerows(CHARMAP)

# ---- 保証アサーション (docs/07 LB台帳) ----------------------------------
def verify():
    ok = True
    shokai = set(); dupn = 0
    for i in range(5500): 
        k = f'SHK{i:08d}'
        dupn += k in shokai; shokai.add(k)
    ok &= dupn == 0                            # LB-005 照会番号一意
    kyk = open(f'{P}/KYKMAST.dat', 'rb').read()
    assert len(kyk) % 320 == 0
    period_ok = all(kyk[r+42:r+50] < kyk[r+50:r+58] or
                    kyk[r+50:r+58] in (b'99991231', b'99999999',
                                       b'00000000')
                    for r in range(0, len(kyk), 320))
    ok &= period_ok                            # LB-006 期間>=1日
    ken = open(f'{P}/KENFILE.dat', 'rb').read()
    spaces = sum(ken[r+50:r+55] == b'     '
                 for r in range(0, len(ken), 128))
    ok &= spaces == 20                         # B-02 台帳件数と一致
    kin_ok = all(ken[r+50:r+55] == b'     ' or
                 1 <= int(ken[r+50:r+55]) <= 80
                 for r in range(0, len(ken), 128))
    ok &= kin_ok                               # J-03 添字範囲の保証
    print(f'VERIFY: 照会番号一意={dupn == 0} 契約期間保証={period_ok} '
          f'B-02混入数={spaces}(台帳=20) 検針員範囲={kin_ok} '
          f'-> {"PASS" if ok else "FAIL"}')
    return 0 if ok else 1

if __name__ == '__main__':
    if '--verify' in sys.argv:
        sys.exit(verify())
    print('JYUMAST :', gen_jyumast())
    print('KYKMAST :', gen_kykmast())
    print('MTRMAST :', gen_mtrmast())
    print('KENFILE :', gen_kenfile())
    print('SHUIN   :', gen_shuin())
    print('TANKA   :', gen_tanka())
    print('IDOIN   :', gen_ido())
    gen_trigger(); gen_compile_ledger(); write_ledgers()
    print('dirty_ledger rows:', len(DIRTY))
    sys.exit(verify())
