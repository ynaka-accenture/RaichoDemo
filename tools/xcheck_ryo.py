#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
xcheck_ryo.py — RBRYO01C 対照実装(独立検算)
生データ(KENFILE/KYKMAST/TANKA)から料金を Python で再計算し、
COBOL が出力した RYOFILE.dat と全レコード・全金額項目を突合する。
丸め仕様(F-01/F-07)は docs/01 §3 のとおり:
  基本=切捨て / 段階=四捨五入 / 燃調=0方向切捨て / 再エネ=切捨て /
  税=切捨て(内税)
"""
import sys, os
P = os.path.join(os.path.dirname(__file__), '..', 'app', 'data', 'portable')

def unpack(b):
    h = b.hex()
    v = int(h[:-1]); s = -1 if h[-1] in 'db' else 1
    return s * v

def tz(a, b):  # 0方向切捨て除算
    q = abs(a) // abs(b)
    return q if (a >= 0) == (b >= 0) else -q

def rhu(num, den):  # 正数の四捨五入
    return (num * 2 + den) // (den * 2)

# ---- TANKA ----
tanka = {}
d = open(f'{P}/TANKA.dat', 'rb').read()
for r in range(0, len(d), 80):
    b = d[r:r+80]
    row = dict(kaisi=int(b[2:10]), kihon=unpack(b[10:14]),
               d1=unpack(b[14:17]), d2=unpack(b[17:20]),
               d3=unpack(b[20:23]), nen=unpack(b[23:26]),
               sai=unpack(b[26:29]), ritu=unpack(b[29:31]),
               kbn=b[31:32])
    tanka.setdefault(b[0:2].decode(), []).append(row)

# ---- KYKMAST ----
kyk = {}
d = open(f'{P}/KYKMAST.dat', 'rb').read()
for r in range(0, len(d), 320):
    b = d[r:r+320]
    kaisi = int(b[42:50])
    kihon = int(b[59:68]) if kaisi < 19930101 else unpack(b[59:64])
    kyk[b[0:22]] = dict(syu=b[32:34].decode(),
                        yoryo10=unpack(b[34:37]), kihon=kihon)

def calc(spt, ymd6, use):
    c = kyk.get(spt)
    if c is None:
        return ('NOTFND', None)
    if c['syu'] == '99':
        return ('SKIP99', None)
    yy = int(ymd6[:2]); ymd8 = (1900 + yy if yy >= 50 else 2000 + yy) \
        * 10000 + int(ymd6[2:])
    rows = [t for t in tanka[c['syu']] if t['kaisi'] <= ymd8]
    t = max(rows, key=lambda x: x['kaisi'])
    u = abs(use)
    kihon = (t['kihon'] * c['yoryo10']) // 10000       # 単価は10Aあたり
    k1 = min(u, 120); k2 = min(max(u - 120, 0), 180)
    k3 = max(u - 300, 0)
    d1 = rhu(t['d1'] * k1, 100)
    d2 = rhu(t['d2'] * k2, 100)
    d3 = rhu(t['d3'] * k3, 100)
    nen = tz(t['nen'] * u, 100)                        # 0方向切捨て
    sai = (t['sai'] * u) // 100                        # 切捨て(正)
    war = 0 if c['syu'] == '20' else 55
    kei = kihon + d1 + d2 + d3 + nen + sai - war
    zei = (kei * t['ritu']) // (1000 + t['ritu'])      # ritu は x10
    return ('OK', dict(kihon=kihon, d1=d1, d2=d2, d3=d3, nen=nen,
                       sai=sai, war=war, zei=zei, kei=kei, use=u))

def main(ym):
    ken = open(f'{P}/KENFILE.dat', 'rb').read()
    ryo = open(f'{P}/RYOFILE.dat', 'rb').read()
    ri, ok, ng, total = 0, 0, 0, 0
    st = dict(NOTFND=0, SKIP99=0)
    for r in range(0, len(ken), 128):
        b = ken[r:r+128]
        if int(b[22:28]) != ym:
            continue
        spt = b[0:22]; ymd6 = b[28:34].decode()
        use = unpack(b[46:50])
        kind, e = calc(spt, ymd6, use)
        if kind != 'OK':
            st[kind] += 1
            continue
        o = ryo[ri*256:(ri+1)*256]; ri += 1
        got = dict(kihon=unpack(o[32:36]), d1=unpack(o[36:40]),
                   d2=unpack(o[40:44]), d3=unpack(o[44:48]),
                   nen=unpack(o[48:52]), sai=unpack(o[52:56]),
                   war=unpack(o[56:59]), zei=unpack(o[61:65]),
                   kei=unpack(o[65:70]), use=unpack(o[28:32]))
        if o[0:22] != spt:
            ng += 1
            print(f'KEY MISMATCH rec{ri}: {o[0:22]} vs {spt}')
            continue
        diff = {k: (e[k], got[k]) for k in e if e[k] != got[k]}
        if diff:
            ng += 1
            if ng <= 5:
                print(f'DIFF {spt.decode()} {diff}')
        else:
            ok += 1
        total += e['kei']
    print(f'XCHECK YM={ym}: OK={ok} NG={ng} '
          f'NOTFND={st["NOTFND"]} SKIP99={st["SKIP99"]} '
          f'RYO-RECS={len(ryo)//256} PY-GOKEI={total}')
    return 0 if ng == 0 and ok == len(ryo)//256 else 1

if __name__ == '__main__':
    sys.exit(main(int(sys.argv[1]) if len(sys.argv) > 1 else 202406))
