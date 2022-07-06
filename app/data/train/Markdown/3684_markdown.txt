##Alcatraz - Xcode 的包管理器
[Alcatraz](http://alcatraz.io) : Windows / Package Manager

第一步：关闭 Xcode

第二步：如果你之前安装过Alcatraz，卸载它：

```
rm -rf ~/Library/Application\ Support/Developer/Shared/Xcode/Plug-ins/Alcatraz.xcplugin
```

第三步：最关键的一步，运行命令：

```
find ~/Library/Application\ Support/Developer/Shared/Xcode/Plug-ins -name Info.plist -maxdepth 3 | xargs -I{} defaults write {} DVTPlugInCompatibilityUUIDs -array-add defaults read /Applications/Xcode.app/Contents/Info DVTPlugInCompatibilityUUID

sudo xcode-select --reset
```

第四步：安装 Alcatraz

```
curl -fsSL https://raw.github.com/supermarin/Alcatraz/master/Scripts/install.sh | sh

```

##Injection - 即时编译和运行插件
Xcode => Windows => Package Manager => Search => Injection


