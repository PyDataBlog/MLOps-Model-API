#!/bin/bash

# install.js ( indirect version: for ie6 )
java -jar ~/softwares/yui-2.4.7/yuicompressor-2.4.7.jar --type js src/install.js \
    | sed -Ee 's/^(.)/javascript:\1/' \
    | cat .tmp - .tmp 

# install.js ( direct version ) and main.js
java -jar ~/softwares/yui-2.4.7/yuicompressor-2.4.7.jar --type js src/showAssistLinks.js \
    | tee release/main.js \
    | sed -Ee 's/^(.)/javascript:\1/' \
    | sed -Ee "s/['\"\\]/\\\\&/g" \
    | cat .tmp - .tmp 

# | sed -Ee "s/['\"\\]/\\\\&/g" \
# 用于js字符串转义，以上输出可以直接放在引号中


# test
