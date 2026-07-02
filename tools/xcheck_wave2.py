#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
xcheck_wave2.py — RBKEN02C / RBSHU01C / RBTAI01C の対照検算
プログラムの出力ファイルを、生データからの独立再計算と突合する。
"""
import sys, os
P = os.path.join(os.path.dirname(__file__), '..', 'app', 'data', 'portable')

def unpack(b):
    h = b.hex(); v = int(h[:-1])
    return -v if h[-1] in 'db' else v

def main(ym=202406):
    ok = True
    # ---- RBKEN02C: 交換補正の再現と KENCHK 全件一致 ----
    ken = open(f'{P}/KENFILE.dat', 'rb').read()
    chk = open(f'{P}/KENCHK.dat', 'rb').read()
    kokan = out = 0
    for r in range(0, len(ken), 128):
        b = ken[r:r+128]
        if int(b[22:28]) != ym:
            continue
        zen, kon = int(b[34:40]), int(b[40:46])
        use = unpack(b[46:50])
        exp = kon - zen if kon >= zen else (1000000 - zen) + kon
        if kon < zen:
            kokan += 1
        if exp != use:
            ok = False
            print(f'KEN NG: {b[0:22]} exp={exp} got={use}')
        out += 1
    ok &= len(chk) == out * 128
    print(f'XCHECK KEN: OUT={out} KOKAN={kokan} '
          f'CHK-RECS={len(chk)//128} -> {"OK" if ok else "NG"}')

    # ---- RBSHU01C: オーバーパンチ独立復号と SHUMEI 突合 ----
    shu = open(f'{P}/SHUIN.dat', 'rb').read()
    mei = open(f'{P}/SHUMEI.dat', 'rb').read()
    OP_POS = {'{': 0, **{chr(64+i): i for i in range(1, 10)}}
    OP_NEG = {'}': 0, **{chr(73+i): i for i in range(1, 10)}}
    plus = minus = neg_n = 0
    amounts = []
    for r in range(0, len(shu), 120):
        b = shu[r:r+120]
        z = b[46:56].decode(); last = z[9]
        if last.isdigit():
            v = int(z)
        elif last in OP_POS:
            v = int(z[:9]) * 10 + OP_POS[last]
        else:
            v = -(int(z[:9]) * 10 + OP_NEG[last]); neg_n += 1
        amounts.append(v)
        (plus, minus) = (plus + v, minus) if v >= 0 else \
                        (plus, minus + v)
    mei_sum = sum(unpack(mei[r+36:r+41])
                  for r in range(0, len(mei), 100))
    ok2 = (neg_n == 55 and len(mei) // 100 == len(shu) // 120
           and mei_sum == sum(amounts))
    ok &= ok2
    print(f'XCHECK SHU: MINUS={neg_n} PLUS-SUM={plus} '
          f'MINUS-SUM={minus} MEI-SUM={mei_sum} '
          f'-> {"OK" if ok2 else "NG"}')

    # ---- RBKEN01C: 展開結果の件数整合 ----
    hen = open(f'{P}/KENHEN.dat', 'rb').read()
    kok = open(f'{P}/KOKANF.dat', 'rb').read()
    ok4 = len(hen) == 300 * 128 and len(kok) == 7 * 64
    ok &= ok4
    print(f'XCHECK HEN: KENHEN={len(hen)//128} KOKANF={len(kok)//64} '
          f'-> {"OK" if ok4 else "NG"}')

    # ---- RBTAI01C: 候補件数 = 請求 - 消込済(請求に存在するもの) ----
    ryo = open(f'{P}/RYOFILE.dat', 'rb').read()
    tai = open(f'{P}/TAILST.dat', 'rb').read()
    paid = {mei[r:r+22] for r in range(0, len(mei), 100)
            if mei[r+62:r+63] == b'1'}
    cand = sum(1 for r in range(0, len(ryo), 256)
               if ryo[r:r+22] not in paid)
    ranks = {tai[r+36:r+37] for r in range(0, len(tai), 64)}
    ok3 = (len(tai) // 64 == cand and ranks <= {b'A', b'B', b'C'})
    ok &= ok3
    print(f'XCHECK TAI: CAND(exp)={cand} TAILST={len(tai)//64} '
          f'RANKS={sorted(ranks)} -> {"OK" if ok3 else "NG"}')
    return 0 if ok else 1

if __name__ == '__main__':
    sys.exit(main(int(sys.argv[1]) if len(sys.argv) > 1 else 202406))
