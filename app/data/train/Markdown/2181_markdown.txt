# ファイル一覧

```
ghrb-jp $ find ./ -type f -not -path "./.git/*" -not -path "./.idea/*"
./.github/actions/README.md
./.github/CODE_OF_CONDUCT.md
./.github/CONTRIBUTING.md
./.github/ISSUE_TEMPLATE/bug_report.md
./.github/ISSUE_TEMPLATE/custom.md
./.github/ISSUE_TEMPLATE/feature_request.md
./.github/PULL_REQUEST_TEMPLATE.md
./.github/SUPPORT.md
./.github/workflows/README.md
./.gitignore
./docs/index.md
./LICENSE
./README.md

```

# 全体的な説明

- リポジトリ作成当初から時間が経過しているので仕様変更されている部分が多くある（ので対応中）

  - ISSUE_TEMPLATE まわりが全滅（元のままでも使えるけど新しいほうが断然楽）
  - ISSUE_TEMPLATE はリポジトリから外して[初期設定の方法](https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests/about-issue-and-pull-request-templates)を書いたほうがいいのでは。
  - SUPPORT.md は表示されないと思ってたけど、ISSUE 作成時の右下にリンクされるようになってた

- [community](https://github.com/officel/ghrb-jp/community) を 100% にする
- わかりにくい機能を明確にする
- リポジトリルートをシンプルに保ちたいので置いていいファイルは .github ディレクトリに配置する


