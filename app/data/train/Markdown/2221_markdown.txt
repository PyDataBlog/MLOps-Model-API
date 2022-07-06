---
title: '[Xamarin.Forms] 安裝CurrentActivity Plugin藉以存取Xamarin.Android Activity'
categories: Xamarin Xamarin.forms plugin
---

在Xamarin Forms跨平台存取Android的Activity，需要安裝與設定CurrentActivityPlugin才能達成，比如使用MediaPlugin在Android就需要用到，請參考[Access the Current Android Activity from Anywhere](https://montemagno.com/access-the-current-android-activity-from-anywhere/)，安裝步驟請參考[Xamarin.Forms CurrentActivityPlugin](https://github.com/jamesmontemagno/CurrentActivityPlugin)

1. 安裝CurrentActivity Plugin
在Xamarin.Android專案使用Manage Nuget Packages找到Plugin.CurrentActivity，目前版本是v1.0.1，最後更新時間是2015-12-19，但已看到有beta版釋出，比較放心勇敢地使用😸
  ![install-currentactivity-01](/images/2018/04/install-currentactivity-01.png)

2. 新增一個名為"MainApplication.cs"的C# class
  * 安裝CurrentActivity後會開啟readme.txt內容敘述中，說明了Xamarin.Android根目錄會安裝"MainApplication.cs"，這樣的敘述我認為應該是自動產生，但看起來並沒有，就自己建立吧。
  * 在class加註[Application]。
  * "MainApplication"繼承Application, 以及Applicaion.IActivityLifecycleCallbacks。
  * 用工具Implement IActivityLifecycleCallbacks的Interface(在IActivityLifecycleCallbacks的Interface上點選即可)。
  * 增加一個constructor。
  * override OnCreate以及OnTerminate，分別加入RegisterActivityLifecycleCallbacks(this)以及UnregisterActivityLifecycleCallbacks(this)
  * 在OnActivityCreated, OnActivityStarted, OnActivityResumed加入CrossCurrentActivity.Current.Activity = activity。
3. 完整程式碼如下
  ```csharp
     [Application]
     public class MainApplication : Application, Application.IActivityLifecycleCallbacks
     {
         protected MainApplication(IntPtr javaReference, JniHandleOwnership transfer) : base(javaReference, transfer)
         {
         }

         public override void OnCreate()
         {
             base.OnCreate();
             RegisterActivityLifecycleCallbacks(this);
         }

         public override void OnTerminate()
         {
             base.OnTerminate();
             UnregisterActivityLifecycleCallbacks(this);
         }

         public void OnActivityCreated(Activity activity, Bundle savedInstanceState)
         {
             CrossCurrentActivity.Current.Activity = activity;
         }

         public void OnActivityResumed(Activity activity)
         {
             CrossCurrentActivity.Current.Activity = activity;
         }

         public void OnActivityStarted(Activity activity)
         {
             CrossCurrentActivity.Current.Activity = activity;
         }
     }
  ```

到這邊安裝設定完成，在跨專案存取Activity。
