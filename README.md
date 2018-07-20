# OPML - Proof Assistant for Modal Logic in OCaml

Haskellで書かれた様相論理の証明支援システム[HPML](https://github.com/sndtkrh/hpml)をOCamlに移植したものです．  
`HPML`と同じように使えます．  
また，現在はインタラクティブモードのみサポートしています．  
  
  
## Requirements

- OPAM >= 1.2.2
- ocaml(ocamlopt) >= 4.06.1
- jbuilder >= 1.0+beta20

の環境で動作することを確認しています．  
また，OPAMを用いて以下のライブラリをインストールする必要があります．(ビルド及びインストールに書かれたコマンドを実行すると自動的にインストールされます).  
- core
- ppx\_deriving
- ppx\_let
- sexplib

なお，jbuilderがduneという名前に変わったようですが，まだその変更は追従していないので，今後追従したいと思います．(基本的に単純にjbuilderをduneと書き換えたりjbuildというファイルを弄るだけで移行できるようですが)  

## インストール(ビルド及びインストール)
依存ライブラリも自動的にインストールされます．  

```zsh
$ git clone https://github.com/alphaKAI/opml
$ cd opml
$ opam pin add opam .
$ opam install opml
```

## ビルド(インストールしない場合)

この方法では，依存ライブラリは自動的にはインストールされないので，`opam`を用いてRequirementsの項に書かれたライブラリ(`core`など)をインストールしてください．  

```zsh
$ git clone https://github.com/alphaKAI/opml
$ cd opml
$ jbuilder build
```

この場合，実行するにはそのディレクトリで，  

```zsh
$ jbuilder exec opml
```

とすると，起動することができます．  

## 様相言語

| シンボル | 読み方 |
|:-:|:--|
| 英小文字からなる文字列 | 命題変数 |
| `T` | 真を意味する命題定数 |
| `F` | 偽を意味する命題定数 |
| `~` | 否定 |
| `[]` | ボックス |
| `<>` | ダイヤモンド |
| `->` | 含意 |
| `<->` | 論理的同値 |
| `/\` | 論理積 |
| `\/` | 論理和 |

## コマンド

### 公理

| 構文 | 操作 |
|:-:|:-|
| `Axiom φ` | 論理式 φ は公理である． |

### 推論規則

| 構文 | 規則 | 操作 |
|:-:|:-:|:-|
| `MP #n #m` | modus ponens | 既に証明されている n 番目の論理式 φ と m 番目の論理式 φ→ψ から ψ を得る． |
| `US #n φ p` | uniform substitution | 既に証明されている n 番目の論理式の命題変数 p に論理式 φ を一様代入したものを得る． |
| `G #n` | generalization | 既に証明されている n 番目の論理式 φ から □φ を得る． |

### 補助的なコマンド

| 構文 | 操作 |
|:-:|:-|
| `Q` | 終了する． |
| `// this is comment` | コメント行． |
| `Name #n <name>` | 論理式 `#n` に名前 `<name>` を付ける．以後 `#n` の代わりに `#<name>` でその論理式を参照できる． |

上で `<name>` は英小文字からなる文字列でなければなりません．
また， `#n` の代わりに `#^` を使うことで直前に証明された論理式を参照できます．

## 使い方
とりあえず，現状は`HPML`をそっくりそのまま移植したので，`HPML`と同じように使えるようになっているはずです．  
`HPML`のREADMEにかかれている例などを試してみるとつかめると思います．  

## License
OPMLはMITライセンスのもとで公開しています．  
ライブラリの詳細は`LICENSE`ファイルを参照してください．  
