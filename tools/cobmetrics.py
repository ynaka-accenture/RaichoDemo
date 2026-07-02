#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
cobmetrics.py — RACS-DEMO 複雑度計測・レガシー度ゲート
固定形式COBOLを対象に以下を計測する:
  LOC/SLOC/コメント率, McCabe循環的複雑度(CC), GO TO数/密度(非構造化度の代理),
  ALTER数, 最大ネスト深度(1文内ネストIFの近似), Halstead(V,D,E),
  保守性指数MI(古典式), テスト難易度指数TDI(独自合成)
ゲートは「レガシー再現」のため下限判定(複雑さが不足したらFAIL)。
"""
import sys, re, math, json

VERBS = set("""ACCEPT ADD ALTER CALL CANCEL CLOSE COMPUTE CONTINUE DELETE DISPLAY
DIVIDE EVALUATE EXIT GO GOBACK IF INITIALIZE INSPECT MERGE MOVE MULTIPLY OPEN
PERFORM READ RELEASE RETURN REWRITE SEARCH SET SORT START STOP STRING SUBTRACT
UNSTRING WRITE WHEN ELSE""".split())
DECISION_PAT = [
    (r'\bIF\b', 1), (r'\bWHEN\b', 1), (r'\bUNTIL\b', 1), (r'\bVARYING\b', 1),
    (r'\bAND\b', 1), (r'\bOR\b', 1), (r'\bAT\s+END\b', 1),
    (r'\bINVALID\s+KEY\b', 1), (r'\bON\s+SIZE\s+ERROR\b', 1),
    (r'\bON\s+OVERFLOW\b', 1), (r'\bDEPENDING\b', 1),
]

def read_fixed(path):
    src, comments = [], 0
    for raw in open(path, encoding='utf-8', errors='replace'):
        line = raw.rstrip('\n')
        if len(line) < 7:
            continue
        ind = line[6]
        if ind in ('*', '/'):
            comments += 1
            continue
        src.append(line[7:72].upper())
    return src, comments

def analyze(path):
    src, comments = read_fixed(path)
    body = [l for l in src if l.strip()]
    sloc = len(body)
    # PROCEDURE DIVISION 以降のみ制御系計測
    pidx = next((i for i, l in enumerate(body) if 'PROCEDURE DIVISION' in l), 0)
    proc = body[pidx:]
    text = ' '.join(proc)
    cc = 1 + sum(len(re.findall(p, text)) for p, w in DECISION_PAT)
    gotos = len(re.findall(r'\bGO\s+TO\b', text))
    # 上方向GO TO: 既出ラベル(段落/セクション)への後方分岐
    labels = {}
    for i, l in enumerate(proc):
        m = re.match(r'\s*([A-Z0-9][A-Z0-9-]*)\s+SECTION\s*\.', l) or \
            re.match(r'\s*([A-Z0-9][A-Z0-9-]*)\s*\.\s*$', l)
        if m and m.group(1) not in labels:
            labels[m.group(1)] = i
    bgoto = 0
    for i, l in enumerate(proc):
        for tgt in re.findall(r'GO\s+TO\s+([A-Z0-9-]+)', l):
            if tgt in labels and labels[tgt] < i:
                bgoto += 1
    # 部分参照(参照修飾): 定義なき部分項目アクセス
    refmod = len(re.findall(r'[A-Z0-9][A-Z0-9-]*\s*\(\s*[A-Z0-9-]+\s*:', text))
    redefs = len(re.findall(r'\bREDEFINES\b', ' '.join(body)))
    # クローン率: 正規化4行窓の重複割合(プログラム内 Type-1/2 近似)
    norm = [re.sub(r'\s+', ' ', l.strip()) for l in proc if l.strip()]
    wins = [' | '.join(norm[i:i+4]) for i in range(max(len(norm)-3, 0))]
    from collections import Counter
    wc = Counter(wins)
    dup_ratio = round(sum(c for c in wc.values() if c > 1) /
                      max(len(wins), 1) * 100, 1)
    # 未参照ラベル: PERFORM/GO TO のどこからも参照されない段落/セクション
    refs = set(re.findall(r'(?:GO\s+TO|PERFORM|THRU)\s+([A-Z0-9-]+)', text))
    unref = sum(1 for lb in labels if lb not in refs)
    alters = len(re.findall(r'\bALTER\b', text))
    performs_thru = len(re.findall(r'\bPERFORM\s+\S+\s+THRU\b', text))
    sections = len(re.findall(r'\bSECTION\s*\.', text))
    calls = len(set(re.findall(r"\bCALL\s+'(\S+)'", text)))
    files = len(re.findall(r'\bSELECT\b', ' '.join(body[:pidx])))
    # ネスト近似: ピリオド区切り1文中のIF個数の最大(期間終端様式のネスト)
    max_nest = max((s.count(' IF ') + (1 if s.strip().startswith('IF ') else 0)
                    for s in text.split('.')), default=0)
    # Halstead(近似トークン法)
    toks = re.findall(r"[A-Z0-9][A-Z0-9-]*|'[^']*'|[=<>+\-*/()]", text)
    ops = [t for t in toks if t in VERBS or t in list('=<>+-*/()')
           or t in ('NOT', 'TO', 'FROM', 'BY', 'GIVING', 'USING', 'THRU')]
    opd = [t for t in toks if t not in ops]
    n1, n2 = max(len(set(ops)), 1), max(len(set(opd)), 1)
    N1, N2 = len(ops), len(opd)
    vol = (N1 + N2) * math.log2(n1 + n2)
    dif = (n1 / 2) * (N2 / n2)
    eff = vol * dif
    mi = max(0.0, (171 - 5.2 * math.log(max(vol, 1))
                   - 0.23 * cc - 16.2 * math.log(max(sloc, 1))) * 100 / 171)
    tdi = round(cc * 0.4 + gotos * 1.0 + alters * 5 + performs_thru * 2
                + files * 4 + calls * 3 + max_nest * 3, 1)
    return dict(program=path.split('/')[-1], sloc=sloc, comments=comments,
                comment_ratio=round(comments / max(sloc + comments, 1) * 100, 1),
                cc=cc, gotos=gotos, goto_density=round(gotos / max(sloc, 1) * 100, 1),
                alters=alters, perform_thru=performs_thru, sections=sections,
                bgoto=bgoto, refmod=refmod, redefs=redefs,
                dup_ratio=dup_ratio, unref_labels=unref,
                max_nest=max_nest, halstead_v=round(vol), halstead_d=round(dif, 1),
                halstead_e=round(eff), mi=round(mi, 1), tdi=tdi,
                calls=calls, files=files)

# 区分別「レガシー複雑度」下限ゲート(不足=FAIL)。docs/06 と同期させること。
GATES = {  # prefix: (min_cc, min_gotos, min_bgoto, min_refmod, min_nest, max_mi, min_tdi)
    'RBRYO01': (120, 35, 8, 6, 5, 35, 220),   # 中核バッチ
    'RBSTM02': (30, 12, 2, 8, 3, 60, 55),     # 編集子(ALTER2+は台帳)
    'RAKYK02': (110, 25, 4, 2, 5, 35, 200),   # 大型CICS
    'RB':      (45, 12, 3, 2, 3, 55, 90),     # 一般バッチ
    'RX':      (20,  6, 1, 0, 2, 65, 40),     # SORT出口
    'RA':      (35,  8, 2, 1, 3, 60, 70),     # 一般CICS
    'RU':      (30, 12, 2, 1, 3, 60, 55),     # 共通サブ
}

def gate_for(name):
    for k in sorted(GATES, key=len, reverse=True):
        if name.startswith(k):
            return GATES[k]
    return None

def main():
    args = [a for a in sys.argv[1:] if not a.startswith('--')]
    do_gate = '--gate' in sys.argv
    fails = 0
    hdr = f"{'PROGRAM':<12}{'SLOC':>6}{'CC':>5}{'GOTO':>6}{'BGOTO':>6}" \
          f"{'REFMOD':>7}{'REDEF':>6}{'NEST':>5}{'DUP%':>6}{'UNREF':>6}{'MI':>7}{'TDI':>7}  GATE"
    print(hdr); print('-' * len(hdr))
    for path in args:
        m = analyze(path)
        verdict = ''
        g = gate_for(m['program'].split('.')[0])
        if do_gate and g:
            ok = (m['cc'] >= g[0] and m['gotos'] >= g[1] and
                  m['bgoto'] >= g[2] and m['refmod'] >= g[3] and
                  m['max_nest'] >= g[4] and m['mi'] <= g[5] and m['tdi'] >= g[6])
            verdict = 'PASS' if ok else \
                f"FAIL(min CC={g[0]} GOTO={g[1]} BGOTO={g[2]} REFMOD={g[3]}" \
                f" NEST={g[4]} maxMI={g[5]} TDI={g[6]})"
            fails += 0 if ok else 1
        print(f"{m['program']:<12}{m['sloc']:>6}{m['cc']:>5}{m['gotos']:>6}"
              f"{m['bgoto']:>6}{m['refmod']:>7}{m['redefs']:>6}"
              f"{m['max_nest']:>5}{m['dup_ratio']:>6}{m['unref_labels']:>6}"
              f"{m['mi']:>7}{m['tdi']:>7}  {verdict}")
    sys.exit(1 if fails else 0)

if __name__ == '__main__':
    main()
