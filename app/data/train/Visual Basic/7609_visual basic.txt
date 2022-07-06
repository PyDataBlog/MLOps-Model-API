<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frm_Main
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frm_Main))
        Me.gb_Logs = New System.Windows.Forms.GroupBox()
        Me.btn_ClearLogs = New System.Windows.Forms.Button()
        Me.txt_Logs = New System.Windows.Forms.TextBox()
        Me.lbl_DevInfo = New System.Windows.Forms.Label()
        Me.gb_RebootOptions = New System.Windows.Forms.GroupBox()
        Me.btn_Reboot = New System.Windows.Forms.Button()
        Me.cb_RebootOptions = New System.Windows.Forms.ComboBox()
        Me.gb_DevInfo = New System.Windows.Forms.GroupBox()
        Me.lbl_Batt = New System.Windows.Forms.Label()
        Me.lbl_Battery = New System.Windows.Forms.Label()
        Me.lbl_DevMdl = New System.Windows.Forms.Label()
        Me.ll_Email = New System.Windows.Forms.LinkLabel()
        Me.tc_MAIN = New System.Windows.Forms.TabControl()
        Me.tb_MainTools = New System.Windows.Forms.TabPage()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.lbl_warn2 = New System.Windows.Forms.Label()
        Me.gb_ADBCommand = New System.Windows.Forms.GroupBox()
        Me.btn_GetSerialNumber = New System.Windows.Forms.Button()
        Me.btn_DeviceList = New System.Windows.Forms.Button()
        Me.btn_PushFile = New System.Windows.Forms.Button()
        Me.btn_ShellConsole = New System.Windows.Forms.Button()
        Me.gb_Sideload = New System.Windows.Forms.GroupBox()
        Me.cb_SideLoad = New System.Windows.Forms.CheckBox()
        Me.btn_Sideload = New System.Windows.Forms.Button()
        Me.txt_SideloadPath = New System.Windows.Forms.TextBox()
        Me.gb_ManualADB = New System.Windows.Forms.GroupBox()
        Me.btn_ExecuteADB = New System.Windows.Forms.Button()
        Me.txt_RunADB = New System.Windows.Forms.TextBox()
        Me.tb_AppTools = New System.Windows.Forms.TabPage()
        Me.rb_FilterAppAll = New System.Windows.Forms.RadioButton()
        Me.rb_FilterSystemApp = New System.Windows.Forms.RadioButton()
        Me.rb_FilterAppUser = New System.Windows.Forms.RadioButton()
        Me.lbl_ApkPath = New System.Windows.Forms.Label()
        Me.lbl_AppName = New System.Windows.Forms.Label()
        Me.gb_APKOptions = New System.Windows.Forms.GroupBox()
        Me.lbl_warn3 = New System.Windows.Forms.Label()
        Me.btn_RestoreAPK = New System.Windows.Forms.Button()
        Me.btn_ClearAPKData = New System.Windows.Forms.Button()
        Me.btn_BackupAPK = New System.Windows.Forms.Button()
        Me.btn_RunAPK = New System.Windows.Forms.Button()
        Me.btn_ForceCloseAPK = New System.Windows.Forms.Button()
        Me.btn_InstallAPK = New System.Windows.Forms.Button()
        Me.btn_RefreshListAPK = New System.Windows.Forms.Button()
        Me.btn_UninstallAPK = New System.Windows.Forms.Button()
        Me.list_APK = New System.Windows.Forms.ListBox()
        Me.txt_Path = New System.Windows.Forms.TextBox()
        Me.txt_AppName = New System.Windows.Forms.TextBox()
        Me.tb_Xtools = New System.Windows.Forms.TabPage()
        Me.tc_Extra = New System.Windows.Forms.TabControl()
        Me.tb_InputKeyEvent = New System.Windows.Forms.TabPage()
        Me.txt_InputKey = New System.Windows.Forms.TextBox()
        Me.lbl_KeyInputTextWarn = New System.Windows.Forms.Label()
        Me.lbl_InputKey = New System.Windows.Forms.Label()
        Me.btn_KeyInputText = New System.Windows.Forms.Button()
        Me.tb_KeyEvent = New System.Windows.Forms.TabPage()
        Me.btn_SwipeSend = New System.Windows.Forms.Button()
        Me.lbl_SwipeDuration = New System.Windows.Forms.Label()
        Me.txt_SwipeDuration = New System.Windows.Forms.TextBox()
        Me.lbl_SwipeY2 = New System.Windows.Forms.Label()
        Me.txt_SwipeY2 = New System.Windows.Forms.TextBox()
        Me.lbl_SwipeY1 = New System.Windows.Forms.Label()
        Me.txt_SwipeY1 = New System.Windows.Forms.TextBox()
        Me.lbl_SwipeX2 = New System.Windows.Forms.Label()
        Me.txt_SwipeX2 = New System.Windows.Forms.TextBox()
        Me.lbl_SwipeX1 = New System.Windows.Forms.Label()
        Me.txt_SwipeX1 = New System.Windows.Forms.TextBox()
        Me.tb_Misc = New System.Windows.Forms.TabPage()
        Me.btn_KeyboardInput = New System.Windows.Forms.Button()
        Me.gb_ToogleRoot = New System.Windows.Forms.GroupBox()
        Me.btn_ToogleRoot = New System.Windows.Forms.Button()
        Me.cb_ToogleRoot = New System.Windows.Forms.ComboBox()
        Me.btn_TurnOnOffScreen = New System.Windows.Forms.Button()
        Me.btn_Screenshoot = New System.Windows.Forms.Button()
        Me.tb_prefNetwork = New System.Windows.Forms.TabPage()
        Me.lbl_network2 = New System.Windows.Forms.Label()
        Me.btn_prefnetwork = New System.Windows.Forms.Button()
        Me.cb_prefnetwork = New System.Windows.Forms.ComboBox()
        Me.lbl_network1 = New System.Windows.Forms.Label()
        Me.gb_Dialer = New System.Windows.Forms.GroupBox()
        Me.lbl_inputPhone = New System.Windows.Forms.Label()
        Me.btn_Call = New System.Windows.Forms.Button()
        Me.txt_Dialer = New System.Windows.Forms.TextBox()
        Me.gb_ScreenRecord = New System.Windows.Forms.GroupBox()
        Me.cb_ShowTouches = New System.Windows.Forms.CheckBox()
        Me.lbl_RecordProg = New System.Windows.Forms.Label()
        Me.lbl_Bitrate_2 = New System.Windows.Forms.Label()
        Me.lbl_TimeVid_2 = New System.Windows.Forms.Label()
        Me.lbl_x = New System.Windows.Forms.Label()
        Me.txt_VideoBitrate = New System.Windows.Forms.TextBox()
        Me.txt_VideoTime = New System.Windows.Forms.TextBox()
        Me.txt_VidResH = New System.Windows.Forms.TextBox()
        Me.txt_VidResW = New System.Windows.Forms.TextBox()
        Me.lbl_TimeVid = New System.Windows.Forms.Label()
        Me.lbl_Bitrate = New System.Windows.Forms.Label()
        Me.lbl_VidRes = New System.Windows.Forms.Label()
        Me.btn_RecordVideo = New System.Windows.Forms.Button()
        Me.pb_RecordVideo = New System.Windows.Forms.ProgressBar()
        Me.tb_buildprop = New System.Windows.Forms.TabPage()
        Me.lbl_BuildPropStats = New System.Windows.Forms.Label()
        Me.txt_buildprop = New ICSharpCode.TextEditor.TextEditorControl()
        Me.lbl_buildprophow = New System.Windows.Forms.Label()
        Me.gb_buildprop = New System.Windows.Forms.GroupBox()
        Me.btn_LoadBuildProp = New System.Windows.Forms.Button()
        Me.btn_BackupBuildProp = New System.Windows.Forms.Button()
        Me.btn_SaveBuildProp = New System.Windows.Forms.Button()
        Me.tb_ADBSettings = New System.Windows.Forms.TabPage()
        Me.lbl_Warn1 = New System.Windows.Forms.Label()
        Me.gb_ADBSettings = New System.Windows.Forms.GroupBox()
        Me.btn_jwdp = New System.Windows.Forms.Button()
        Me.btn_DisableVerity = New System.Windows.Forms.Button()
        Me.btn_EnableVerity = New System.Windows.Forms.Button()
        Me.btn_ADBKeygen = New System.Windows.Forms.Button()
        Me.gb_ADBRestart = New System.Windows.Forms.GroupBox()
        Me.lbl_tcport = New System.Windows.Forms.Label()
        Me.txt_tcport = New System.Windows.Forms.TextBox()
        Me.btn_adbTCPIP = New System.Windows.Forms.Button()
        Me.btn_adbUSB = New System.Windows.Forms.Button()
        Me.btn_AdbUnroot = New System.Windows.Forms.Button()
        Me.btn_adbRoot = New System.Windows.Forms.Button()
        Me.gb_ADBWifi = New System.Windows.Forms.GroupBox()
        Me.lbl_HowToIP = New System.Windows.Forms.Label()
        Me.lbl_defPort = New System.Windows.Forms.Label()
        Me.btn_connectwifi = New System.Windows.Forms.Button()
        Me.lbl_DevIP = New System.Windows.Forms.Label()
        Me.lbl_port = New System.Windows.Forms.Label()
        Me.txt_ipaddr = New System.Windows.Forms.TextBox()
        Me.txt_tcpip = New System.Windows.Forms.TextBox()
        Me.tb_About = New System.Windows.Forms.TabPage()
        Me.gb_Thanks = New System.Windows.Forms.GroupBox()
        Me.lbl_Thanks = New System.Windows.Forms.Label()
        Me.lbl_disclaimer = New System.Windows.Forms.Label()
        Me.txtbox_License = New System.Windows.Forms.TextBox()
        Me.gb_About = New System.Windows.Forms.GroupBox()
        Me.lbl_About = New System.Windows.Forms.Label()
        Me.Timer1 = New System.Windows.Forms.Timer(Me.components)
        Me.lbl_Header = New System.Windows.Forms.Label()
        Me.ll_Update = New System.Windows.Forms.LinkLabel()
        Me.ll_Github = New System.Windows.Forms.LinkLabel()
        Me.gb_Logs.SuspendLayout()
        Me.gb_RebootOptions.SuspendLayout()
        Me.gb_DevInfo.SuspendLayout()
        Me.tc_MAIN.SuspendLayout()
        Me.tb_MainTools.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.gb_ADBCommand.SuspendLayout()
        Me.gb_Sideload.SuspendLayout()
        Me.gb_ManualADB.SuspendLayout()
        Me.tb_AppTools.SuspendLayout()
        Me.gb_APKOptions.SuspendLayout()
        Me.tb_Xtools.SuspendLayout()
        Me.tc_Extra.SuspendLayout()
        Me.tb_InputKeyEvent.SuspendLayout()
        Me.tb_KeyEvent.SuspendLayout()
        Me.tb_Misc.SuspendLayout()
        Me.gb_ToogleRoot.SuspendLayout()
        Me.tb_prefNetwork.SuspendLayout()
        Me.gb_Dialer.SuspendLayout()
        Me.gb_ScreenRecord.SuspendLayout()
        Me.tb_buildprop.SuspendLayout()
        Me.gb_buildprop.SuspendLayout()
        Me.tb_ADBSettings.SuspendLayout()
        Me.gb_ADBSettings.SuspendLayout()
        Me.gb_ADBRestart.SuspendLayout()
        Me.gb_ADBWifi.SuspendLayout()
        Me.tb_About.SuspendLayout()
        Me.gb_Thanks.SuspendLayout()
        Me.gb_About.SuspendLayout()
        Me.SuspendLayout()
        '
        'gb_Logs
        '
        Me.gb_Logs.Controls.Add(Me.btn_ClearLogs)
        Me.gb_Logs.Controls.Add(Me.txt_Logs)
        Me.gb_Logs.Location = New System.Drawing.Point(13, 314)
        Me.gb_Logs.Name = "gb_Logs"
        Me.gb_Logs.Size = New System.Drawing.Size(561, 129)
        Me.gb_Logs.TabIndex = 0
        Me.gb_Logs.TabStop = False
        Me.gb_Logs.Text = "Logs"
        '
        'btn_ClearLogs
        '
        Me.btn_ClearLogs.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_ClearLogs.Location = New System.Drawing.Point(453, 99)
        Me.btn_ClearLogs.Name = "btn_ClearLogs"
        Me.btn_ClearLogs.Size = New System.Drawing.Size(102, 24)
        Me.btn_ClearLogs.TabIndex = 1
        Me.btn_ClearLogs.Text = "Clear Logs"
        Me.btn_ClearLogs.UseVisualStyleBackColor = True
        '
        'txt_Logs
        '
        Me.txt_Logs.Cursor = System.Windows.Forms.Cursors.Default
        Me.txt_Logs.Font = New System.Drawing.Font("Consolas", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txt_Logs.Location = New System.Drawing.Point(6, 19)
        Me.txt_Logs.Multiline = True
        Me.txt_Logs.Name = "txt_Logs"
        Me.txt_Logs.ReadOnly = True
        Me.txt_Logs.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.txt_Logs.Size = New System.Drawing.Size(549, 74)
        Me.txt_Logs.TabIndex = 0
        Me.txt_Logs.WordWrap = False
        '
        'lbl_DevInfo
        '
        Me.lbl_DevInfo.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_DevInfo.ForeColor = System.Drawing.Color.Blue
        Me.lbl_DevInfo.Location = New System.Drawing.Point(131, 16)
        Me.lbl_DevInfo.Name = "lbl_DevInfo"
        Me.lbl_DevInfo.Size = New System.Drawing.Size(98, 26)
        Me.lbl_DevInfo.TabIndex = 2
        Me.lbl_DevInfo.Text = "No Device"
        Me.lbl_DevInfo.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'gb_RebootOptions
        '
        Me.gb_RebootOptions.Controls.Add(Me.btn_Reboot)
        Me.gb_RebootOptions.Controls.Add(Me.cb_RebootOptions)
        Me.gb_RebootOptions.Location = New System.Drawing.Point(196, 67)
        Me.gb_RebootOptions.Name = "gb_RebootOptions"
        Me.gb_RebootOptions.Size = New System.Drawing.Size(173, 92)
        Me.gb_RebootOptions.TabIndex = 3
        Me.gb_RebootOptions.TabStop = False
        Me.gb_RebootOptions.Text = "Reboot Options"
        '
        'btn_Reboot
        '
        Me.btn_Reboot.Enabled = False
        Me.btn_Reboot.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_Reboot.Location = New System.Drawing.Point(6, 55)
        Me.btn_Reboot.Name = "btn_Reboot"
        Me.btn_Reboot.Size = New System.Drawing.Size(161, 25)
        Me.btn_Reboot.TabIndex = 1
        Me.btn_Reboot.Text = "Reboot Now"
        Me.btn_Reboot.UseVisualStyleBackColor = True
        '
        'cb_RebootOptions
        '
        Me.cb_RebootOptions.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb_RebootOptions.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cb_RebootOptions.FormattingEnabled = True
        Me.cb_RebootOptions.Items.AddRange(New Object() {"Normal Reboot", "Recovery", "Bootloader"})
        Me.cb_RebootOptions.Location = New System.Drawing.Point(7, 20)
        Me.cb_RebootOptions.Name = "cb_RebootOptions"
        Me.cb_RebootOptions.Size = New System.Drawing.Size(160, 23)
        Me.cb_RebootOptions.TabIndex = 0
        '
        'gb_DevInfo
        '
        Me.gb_DevInfo.Controls.Add(Me.lbl_Batt)
        Me.gb_DevInfo.Controls.Add(Me.lbl_Battery)
        Me.gb_DevInfo.Controls.Add(Me.lbl_DevMdl)
        Me.gb_DevInfo.Controls.Add(Me.lbl_DevInfo)
        Me.gb_DevInfo.Location = New System.Drawing.Point(12, 453)
        Me.gb_DevInfo.Name = "gb_DevInfo"
        Me.gb_DevInfo.Size = New System.Drawing.Size(357, 48)
        Me.gb_DevInfo.TabIndex = 4
        Me.gb_DevInfo.TabStop = False
        Me.gb_DevInfo.Text = "Info"
        '
        'lbl_Batt
        '
        Me.lbl_Batt.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_Batt.ForeColor = System.Drawing.Color.Black
        Me.lbl_Batt.Location = New System.Drawing.Point(235, 16)
        Me.lbl_Batt.Name = "lbl_Batt"
        Me.lbl_Batt.Size = New System.Drawing.Size(74, 26)
        Me.lbl_Batt.TabIndex = 4
        Me.lbl_Batt.Text = "Battery  :"
        Me.lbl_Batt.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'lbl_Battery
        '
        Me.lbl_Battery.Font = New System.Drawing.Font("Microsoft Sans Serif", 11.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_Battery.ForeColor = System.Drawing.Color.Green
        Me.lbl_Battery.Location = New System.Drawing.Point(309, 16)
        Me.lbl_Battery.Name = "lbl_Battery"
        Me.lbl_Battery.Size = New System.Drawing.Size(37, 26)
        Me.lbl_Battery.TabIndex = 3
        Me.lbl_Battery.Text = "-"
        Me.lbl_Battery.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'lbl_DevMdl
        '
        Me.lbl_DevMdl.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_DevMdl.ForeColor = System.Drawing.Color.Black
        Me.lbl_DevMdl.Location = New System.Drawing.Point(6, 16)
        Me.lbl_DevMdl.Name = "lbl_DevMdl"
        Me.lbl_DevMdl.Size = New System.Drawing.Size(119, 26)
        Me.lbl_DevMdl.TabIndex = 3
        Me.lbl_DevMdl.Text = "Device Model :"
        Me.lbl_DevMdl.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'll_Email
        '
        Me.ll_Email.AutoSize = True
        Me.ll_Email.Location = New System.Drawing.Point(513, 487)
        Me.ll_Email.Name = "ll_Email"
        Me.ll_Email.Size = New System.Drawing.Size(61, 13)
        Me.ll_Email.TabIndex = 5
        Me.ll_Email.TabStop = True
        Me.ll_Email.Text = "Bug Report"
        '
        'tc_MAIN
        '
        Me.tc_MAIN.Controls.Add(Me.tb_MainTools)
        Me.tc_MAIN.Controls.Add(Me.tb_AppTools)
        Me.tc_MAIN.Controls.Add(Me.tb_Xtools)
        Me.tc_MAIN.Controls.Add(Me.tb_buildprop)
        Me.tc_MAIN.Controls.Add(Me.tb_ADBSettings)
        Me.tc_MAIN.Controls.Add(Me.tb_About)
        Me.tc_MAIN.Location = New System.Drawing.Point(12, 68)
        Me.tc_MAIN.Name = "tc_MAIN"
        Me.tc_MAIN.SelectedIndex = 0
        Me.tc_MAIN.Size = New System.Drawing.Size(562, 240)
        Me.tc_MAIN.TabIndex = 5
        '
        'tb_MainTools
        '
        Me.tb_MainTools.Controls.Add(Me.GroupBox1)
        Me.tb_MainTools.Controls.Add(Me.gb_ADBCommand)
        Me.tb_MainTools.Controls.Add(Me.gb_Sideload)
        Me.tb_MainTools.Controls.Add(Me.gb_ManualADB)
        Me.tb_MainTools.Controls.Add(Me.gb_RebootOptions)
        Me.tb_MainTools.Location = New System.Drawing.Point(4, 22)
        Me.tb_MainTools.Name = "tb_MainTools"
        Me.tb_MainTools.Padding = New System.Windows.Forms.Padding(3)
        Me.tb_MainTools.Size = New System.Drawing.Size(554, 214)
        Me.tb_MainTools.TabIndex = 0
        Me.tb_MainTools.Text = "Main Tools"
        Me.tb_MainTools.UseVisualStyleBackColor = True
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.lbl_warn2)
        Me.GroupBox1.Location = New System.Drawing.Point(375, 67)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(173, 92)
        Me.GroupBox1.TabIndex = 10
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Fastboot Commands"
        '
        'lbl_warn2
        '
        Me.lbl_warn2.ForeColor = System.Drawing.Color.Red
        Me.lbl_warn2.Location = New System.Drawing.Point(6, 20)
        Me.lbl_warn2.Name = "lbl_warn2"
        Me.lbl_warn2.Size = New System.Drawing.Size(160, 60)
        Me.lbl_warn2.TabIndex = 0
        Me.lbl_warn2.Text = "Not Implemented Yet"
        Me.lbl_warn2.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'gb_ADBCommand
        '
        Me.gb_ADBCommand.Controls.Add(Me.btn_GetSerialNumber)
        Me.gb_ADBCommand.Controls.Add(Me.btn_DeviceList)
        Me.gb_ADBCommand.Controls.Add(Me.btn_PushFile)
        Me.gb_ADBCommand.Controls.Add(Me.btn_ShellConsole)
        Me.gb_ADBCommand.Location = New System.Drawing.Point(7, 7)
        Me.gb_ADBCommand.Name = "gb_ADBCommand"
        Me.gb_ADBCommand.Size = New System.Drawing.Size(183, 152)
        Me.gb_ADBCommand.TabIndex = 9
        Me.gb_ADBCommand.TabStop = False
        Me.gb_ADBCommand.Text = "ADB Commands"
        '
        'btn_GetSerialNumber
        '
        Me.btn_GetSerialNumber.Location = New System.Drawing.Point(6, 86)
        Me.btn_GetSerialNumber.Name = "btn_GetSerialNumber"
        Me.btn_GetSerialNumber.Size = New System.Drawing.Size(171, 25)
        Me.btn_GetSerialNumber.TabIndex = 6
        Me.btn_GetSerialNumber.Text = "Get Device Serial Number"
        Me.btn_GetSerialNumber.UseVisualStyleBackColor = True
        '
        'btn_DeviceList
        '
        Me.btn_DeviceList.Location = New System.Drawing.Point(6, 55)
        Me.btn_DeviceList.Name = "btn_DeviceList"
        Me.btn_DeviceList.Size = New System.Drawing.Size(171, 25)
        Me.btn_DeviceList.TabIndex = 5
        Me.btn_DeviceList.Text = "Devices List"
        Me.btn_DeviceList.UseVisualStyleBackColor = True
        '
        'btn_PushFile
        '
        Me.btn_PushFile.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_PushFile.Location = New System.Drawing.Point(6, 24)
        Me.btn_PushFile.Name = "btn_PushFile"
        Me.btn_PushFile.Size = New System.Drawing.Size(171, 25)
        Me.btn_PushFile.TabIndex = 4
        Me.btn_PushFile.Text = "Push / Pull"
        Me.btn_PushFile.UseVisualStyleBackColor = True
        '
        'btn_ShellConsole
        '
        Me.btn_ShellConsole.Location = New System.Drawing.Point(6, 121)
        Me.btn_ShellConsole.Name = "btn_ShellConsole"
        Me.btn_ShellConsole.Size = New System.Drawing.Size(171, 25)
        Me.btn_ShellConsole.TabIndex = 0
        Me.btn_ShellConsole.Text = "Shell Console"
        Me.btn_ShellConsole.UseVisualStyleBackColor = True
        '
        'gb_Sideload
        '
        Me.gb_Sideload.Controls.Add(Me.cb_SideLoad)
        Me.gb_Sideload.Controls.Add(Me.btn_Sideload)
        Me.gb_Sideload.Controls.Add(Me.txt_SideloadPath)
        Me.gb_Sideload.Location = New System.Drawing.Point(196, 7)
        Me.gb_Sideload.Name = "gb_Sideload"
        Me.gb_Sideload.Size = New System.Drawing.Size(352, 54)
        Me.gb_Sideload.TabIndex = 7
        Me.gb_Sideload.TabStop = False
        Me.gb_Sideload.Text = "Sideload"
        '
        'cb_SideLoad
        '
        Me.cb_SideLoad.AutoSize = True
        Me.cb_SideLoad.Location = New System.Drawing.Point(6, 22)
        Me.cb_SideLoad.Name = "cb_SideLoad"
        Me.cb_SideLoad.Size = New System.Drawing.Size(47, 17)
        Me.cb_SideLoad.TabIndex = 3
        Me.cb_SideLoad.Text = "6.0+"
        Me.cb_SideLoad.UseVisualStyleBackColor = True
        '
        'btn_Sideload
        '
        Me.btn_Sideload.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_Sideload.Location = New System.Drawing.Point(283, 13)
        Me.btn_Sideload.Name = "btn_Sideload"
        Me.btn_Sideload.Size = New System.Drawing.Size(63, 33)
        Me.btn_Sideload.TabIndex = 2
        Me.btn_Sideload.Text = "Flash"
        Me.btn_Sideload.UseVisualStyleBackColor = True
        '
        'txt_SideloadPath
        '
        Me.txt_SideloadPath.Location = New System.Drawing.Point(59, 20)
        Me.txt_SideloadPath.Name = "txt_SideloadPath"
        Me.txt_SideloadPath.ReadOnly = True
        Me.txt_SideloadPath.Size = New System.Drawing.Size(218, 20)
        Me.txt_SideloadPath.TabIndex = 0
        '
        'gb_ManualADB
        '
        Me.gb_ManualADB.Controls.Add(Me.btn_ExecuteADB)
        Me.gb_ManualADB.Controls.Add(Me.txt_RunADB)
        Me.gb_ManualADB.Location = New System.Drawing.Point(7, 165)
        Me.gb_ManualADB.Name = "gb_ManualADB"
        Me.gb_ManualADB.Size = New System.Drawing.Size(541, 43)
        Me.gb_ManualADB.TabIndex = 6
        Me.gb_ManualADB.TabStop = False
        Me.gb_ManualADB.Text = "ADB Manual Commands"
        '
        'btn_ExecuteADB
        '
        Me.btn_ExecuteADB.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_ExecuteADB.Location = New System.Drawing.Point(460, 17)
        Me.btn_ExecuteADB.Name = "btn_ExecuteADB"
        Me.btn_ExecuteADB.Size = New System.Drawing.Size(75, 20)
        Me.btn_ExecuteADB.TabIndex = 1
        Me.btn_ExecuteADB.Text = "Execute"
        Me.btn_ExecuteADB.UseVisualStyleBackColor = True
        '
        'txt_RunADB
        '
        Me.txt_RunADB.Location = New System.Drawing.Point(6, 17)
        Me.txt_RunADB.Name = "txt_RunADB"
        Me.txt_RunADB.Size = New System.Drawing.Size(448, 20)
        Me.txt_RunADB.TabIndex = 0
        '
        'tb_AppTools
        '
        Me.tb_AppTools.Controls.Add(Me.rb_FilterAppAll)
        Me.tb_AppTools.Controls.Add(Me.rb_FilterSystemApp)
        Me.tb_AppTools.Controls.Add(Me.rb_FilterAppUser)
        Me.tb_AppTools.Controls.Add(Me.lbl_ApkPath)
        Me.tb_AppTools.Controls.Add(Me.lbl_AppName)
        Me.tb_AppTools.Controls.Add(Me.gb_APKOptions)
        Me.tb_AppTools.Controls.Add(Me.list_APK)
        Me.tb_AppTools.Controls.Add(Me.txt_Path)
        Me.tb_AppTools.Controls.Add(Me.txt_AppName)
        Me.tb_AppTools.Location = New System.Drawing.Point(4, 22)
        Me.tb_AppTools.Name = "tb_AppTools"
        Me.tb_AppTools.Size = New System.Drawing.Size(554, 214)
        Me.tb_AppTools.TabIndex = 2
        Me.tb_AppTools.Text = "Application Tools"
        Me.tb_AppTools.UseVisualStyleBackColor = True
        '
        'rb_FilterAppAll
        '
        Me.rb_FilterAppAll.AutoSize = True
        Me.rb_FilterAppAll.Location = New System.Drawing.Point(172, 6)
        Me.rb_FilterAppAll.Name = "rb_FilterAppAll"
        Me.rb_FilterAppAll.Size = New System.Drawing.Size(118, 17)
        Me.rb_FilterAppAll.TabIndex = 22
        Me.rb_FilterAppAll.Text = "User  + System App"
        Me.rb_FilterAppAll.UseVisualStyleBackColor = True
        '
        'rb_FilterSystemApp
        '
        Me.rb_FilterSystemApp.AutoSize = True
        Me.rb_FilterSystemApp.Location = New System.Drawing.Point(85, 6)
        Me.rb_FilterSystemApp.Name = "rb_FilterSystemApp"
        Me.rb_FilterSystemApp.Size = New System.Drawing.Size(81, 17)
        Me.rb_FilterSystemApp.TabIndex = 21
        Me.rb_FilterSystemApp.Text = "System App"
        Me.rb_FilterSystemApp.UseVisualStyleBackColor = True
        '
        'rb_FilterAppUser
        '
        Me.rb_FilterAppUser.AutoSize = True
        Me.rb_FilterAppUser.Checked = True
        Me.rb_FilterAppUser.Location = New System.Drawing.Point(10, 6)
        Me.rb_FilterAppUser.Name = "rb_FilterAppUser"
        Me.rb_FilterAppUser.Size = New System.Drawing.Size(69, 17)
        Me.rb_FilterAppUser.TabIndex = 20
        Me.rb_FilterAppUser.TabStop = True
        Me.rb_FilterAppUser.Text = "User App"
        Me.rb_FilterAppUser.UseVisualStyleBackColor = True
        '
        'lbl_ApkPath
        '
        Me.lbl_ApkPath.AutoSize = True
        Me.lbl_ApkPath.Location = New System.Drawing.Point(7, 188)
        Me.lbl_ApkPath.Name = "lbl_ApkPath"
        Me.lbl_ApkPath.Size = New System.Drawing.Size(35, 13)
        Me.lbl_ApkPath.TabIndex = 19
        Me.lbl_ApkPath.Text = "Path :"
        '
        'lbl_AppName
        '
        Me.lbl_AppName.AutoSize = True
        Me.lbl_AppName.Location = New System.Drawing.Point(7, 162)
        Me.lbl_AppName.Name = "lbl_AppName"
        Me.lbl_AppName.Size = New System.Drawing.Size(63, 13)
        Me.lbl_AppName.TabIndex = 18
        Me.lbl_AppName.Text = "App Name :"
        '
        'gb_APKOptions
        '
        Me.gb_APKOptions.Controls.Add(Me.lbl_warn3)
        Me.gb_APKOptions.Controls.Add(Me.btn_RestoreAPK)
        Me.gb_APKOptions.Controls.Add(Me.btn_ClearAPKData)
        Me.gb_APKOptions.Controls.Add(Me.btn_BackupAPK)
        Me.gb_APKOptions.Controls.Add(Me.btn_RunAPK)
        Me.gb_APKOptions.Controls.Add(Me.btn_ForceCloseAPK)
        Me.gb_APKOptions.Controls.Add(Me.btn_InstallAPK)
        Me.gb_APKOptions.Controls.Add(Me.btn_RefreshListAPK)
        Me.gb_APKOptions.Controls.Add(Me.btn_UninstallAPK)
        Me.gb_APKOptions.Location = New System.Drawing.Point(343, 6)
        Me.gb_APKOptions.Name = "gb_APKOptions"
        Me.gb_APKOptions.Size = New System.Drawing.Size(205, 199)
        Me.gb_APKOptions.TabIndex = 5
        Me.gb_APKOptions.TabStop = False
        Me.gb_APKOptions.Text = "Options"
        '
        'lbl_warn3
        '
        Me.lbl_warn3.Location = New System.Drawing.Point(7, 140)
        Me.lbl_warn3.Name = "lbl_warn3"
        Me.lbl_warn3.Size = New System.Drawing.Size(191, 55)
        Me.lbl_warn3.TabIndex = 19
        Me.lbl_warn3.Text = "- Be Careful -" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "App Backup And Restore Is Still Buggy" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "Check The Backup File Befo" & _
            "re Restore"
        Me.lbl_warn3.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'btn_RestoreAPK
        '
        Me.btn_RestoreAPK.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_RestoreAPK.Location = New System.Drawing.Point(107, 79)
        Me.btn_RestoreAPK.Name = "btn_RestoreAPK"
        Me.btn_RestoreAPK.Size = New System.Drawing.Size(91, 24)
        Me.btn_RestoreAPK.TabIndex = 18
        Me.btn_RestoreAPK.Text = "Restore"
        Me.btn_RestoreAPK.UseVisualStyleBackColor = True
        '
        'btn_ClearAPKData
        '
        Me.btn_ClearAPKData.Enabled = False
        Me.btn_ClearAPKData.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_ClearAPKData.Location = New System.Drawing.Point(6, 109)
        Me.btn_ClearAPKData.Name = "btn_ClearAPKData"
        Me.btn_ClearAPKData.Size = New System.Drawing.Size(91, 24)
        Me.btn_ClearAPKData.TabIndex = 12
        Me.btn_ClearAPKData.Text = "Clear Data"
        Me.btn_ClearAPKData.UseVisualStyleBackColor = True
        '
        'btn_BackupAPK
        '
        Me.btn_BackupAPK.Enabled = False
        Me.btn_BackupAPK.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_BackupAPK.Location = New System.Drawing.Point(6, 79)
        Me.btn_BackupAPK.Name = "btn_BackupAPK"
        Me.btn_BackupAPK.Size = New System.Drawing.Size(91, 24)
        Me.btn_BackupAPK.TabIndex = 16
        Me.btn_BackupAPK.Text = "Backup"
        Me.btn_BackupAPK.UseVisualStyleBackColor = True
        '
        'btn_RunAPK
        '
        Me.btn_RunAPK.Enabled = False
        Me.btn_RunAPK.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_RunAPK.Location = New System.Drawing.Point(6, 49)
        Me.btn_RunAPK.Name = "btn_RunAPK"
        Me.btn_RunAPK.Size = New System.Drawing.Size(91, 24)
        Me.btn_RunAPK.TabIndex = 15
        Me.btn_RunAPK.Text = "Run App"
        Me.btn_RunAPK.UseVisualStyleBackColor = True
        '
        'btn_ForceCloseAPK
        '
        Me.btn_ForceCloseAPK.Enabled = False
        Me.btn_ForceCloseAPK.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_ForceCloseAPK.Location = New System.Drawing.Point(107, 49)
        Me.btn_ForceCloseAPK.Name = "btn_ForceCloseAPK"
        Me.btn_ForceCloseAPK.Size = New System.Drawing.Size(91, 24)
        Me.btn_ForceCloseAPK.TabIndex = 14
        Me.btn_ForceCloseAPK.Text = "Force Close"
        Me.btn_ForceCloseAPK.UseVisualStyleBackColor = True
        '
        'btn_InstallAPK
        '
        Me.btn_InstallAPK.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_InstallAPK.Location = New System.Drawing.Point(6, 19)
        Me.btn_InstallAPK.Name = "btn_InstallAPK"
        Me.btn_InstallAPK.Size = New System.Drawing.Size(91, 24)
        Me.btn_InstallAPK.TabIndex = 13
        Me.btn_InstallAPK.Text = "Install"
        Me.btn_InstallAPK.UseVisualStyleBackColor = True
        '
        'btn_RefreshListAPK
        '
        Me.btn_RefreshListAPK.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_RefreshListAPK.Location = New System.Drawing.Point(107, 109)
        Me.btn_RefreshListAPK.Name = "btn_RefreshListAPK"
        Me.btn_RefreshListAPK.Size = New System.Drawing.Size(91, 24)
        Me.btn_RefreshListAPK.TabIndex = 8
        Me.btn_RefreshListAPK.Text = "Refresh"
        Me.btn_RefreshListAPK.UseVisualStyleBackColor = True
        '
        'btn_UninstallAPK
        '
        Me.btn_UninstallAPK.Enabled = False
        Me.btn_UninstallAPK.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_UninstallAPK.Location = New System.Drawing.Point(107, 19)
        Me.btn_UninstallAPK.Name = "btn_UninstallAPK"
        Me.btn_UninstallAPK.Size = New System.Drawing.Size(91, 24)
        Me.btn_UninstallAPK.TabIndex = 10
        Me.btn_UninstallAPK.Text = "Uninstall"
        Me.btn_UninstallAPK.UseVisualStyleBackColor = True
        '
        'list_APK
        '
        Me.list_APK.FormattingEnabled = True
        Me.list_APK.Location = New System.Drawing.Point(7, 27)
        Me.list_APK.Name = "list_APK"
        Me.list_APK.Size = New System.Drawing.Size(329, 121)
        Me.list_APK.TabIndex = 9
        '
        'txt_Path
        '
        Me.txt_Path.Location = New System.Drawing.Point(48, 185)
        Me.txt_Path.Name = "txt_Path"
        Me.txt_Path.ReadOnly = True
        Me.txt_Path.Size = New System.Drawing.Size(288, 20)
        Me.txt_Path.TabIndex = 17
        '
        'txt_AppName
        '
        Me.txt_AppName.Location = New System.Drawing.Point(76, 159)
        Me.txt_AppName.Name = "txt_AppName"
        Me.txt_AppName.ReadOnly = True
        Me.txt_AppName.Size = New System.Drawing.Size(261, 20)
        Me.txt_AppName.TabIndex = 11
        '
        'tb_Xtools
        '
        Me.tb_Xtools.Controls.Add(Me.tc_Extra)
        Me.tb_Xtools.Controls.Add(Me.gb_Dialer)
        Me.tb_Xtools.Controls.Add(Me.gb_ScreenRecord)
        Me.tb_Xtools.Location = New System.Drawing.Point(4, 22)
        Me.tb_Xtools.Name = "tb_Xtools"
        Me.tb_Xtools.Padding = New System.Windows.Forms.Padding(3)
        Me.tb_Xtools.Size = New System.Drawing.Size(554, 214)
        Me.tb_Xtools.TabIndex = 1
        Me.tb_Xtools.Text = "Extra Tools"
        Me.tb_Xtools.UseVisualStyleBackColor = True
        '
        'tc_Extra
        '
        Me.tc_Extra.Controls.Add(Me.tb_InputKeyEvent)
        Me.tc_Extra.Controls.Add(Me.tb_KeyEvent)
        Me.tc_Extra.Controls.Add(Me.tb_Misc)
        Me.tc_Extra.Controls.Add(Me.tb_prefNetwork)
        Me.tc_Extra.Location = New System.Drawing.Point(7, 7)
        Me.tc_Extra.Name = "tc_Extra"
        Me.tc_Extra.SelectedIndex = 0
        Me.tc_Extra.Size = New System.Drawing.Size(286, 126)
        Me.tc_Extra.TabIndex = 5
        '
        'tb_InputKeyEvent
        '
        Me.tb_InputKeyEvent.Controls.Add(Me.txt_InputKey)
        Me.tb_InputKeyEvent.Controls.Add(Me.lbl_KeyInputTextWarn)
        Me.tb_InputKeyEvent.Controls.Add(Me.lbl_InputKey)
        Me.tb_InputKeyEvent.Controls.Add(Me.btn_KeyInputText)
        Me.tb_InputKeyEvent.Location = New System.Drawing.Point(4, 22)
        Me.tb_InputKeyEvent.Name = "tb_InputKeyEvent"
        Me.tb_InputKeyEvent.Padding = New System.Windows.Forms.Padding(3)
        Me.tb_InputKeyEvent.Size = New System.Drawing.Size(278, 100)
        Me.tb_InputKeyEvent.TabIndex = 1
        Me.tb_InputKeyEvent.Text = "Input Key"
        Me.tb_InputKeyEvent.UseVisualStyleBackColor = True
        '
        'txt_InputKey
        '
        Me.txt_InputKey.Location = New System.Drawing.Point(9, 20)
        Me.txt_InputKey.Name = "txt_InputKey"
        Me.txt_InputKey.Size = New System.Drawing.Size(184, 20)
        Me.txt_InputKey.TabIndex = 4
        '
        'lbl_KeyInputTextWarn
        '
        Me.lbl_KeyInputTextWarn.Font = New System.Drawing.Font("Segoe UI", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_KeyInputTextWarn.Location = New System.Drawing.Point(3, 44)
        Me.lbl_KeyInputTextWarn.Name = "lbl_KeyInputTextWarn"
        Me.lbl_KeyInputTextWarn.Size = New System.Drawing.Size(269, 50)
        Me.lbl_KeyInputTextWarn.TabIndex = 3
        Me.lbl_KeyInputTextWarn.Text = "Make Sure You are in Text Box or Anything" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "(Some symbol does not work)"
        Me.lbl_KeyInputTextWarn.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'lbl_InputKey
        '
        Me.lbl_InputKey.AutoSize = True
        Me.lbl_InputKey.Location = New System.Drawing.Point(6, 4)
        Me.lbl_InputKey.Name = "lbl_InputKey"
        Me.lbl_InputKey.Size = New System.Drawing.Size(101, 13)
        Me.lbl_InputKey.TabIndex = 2
        Me.lbl_InputKey.Text = "Input Text to Send :"
        '
        'btn_KeyInputText
        '
        Me.btn_KeyInputText.Location = New System.Drawing.Point(197, 18)
        Me.btn_KeyInputText.Name = "btn_KeyInputText"
        Me.btn_KeyInputText.Size = New System.Drawing.Size(75, 23)
        Me.btn_KeyInputText.TabIndex = 1
        Me.btn_KeyInputText.Text = "Send"
        Me.btn_KeyInputText.UseVisualStyleBackColor = True
        '
        'tb_KeyEvent
        '
        Me.tb_KeyEvent.Controls.Add(Me.btn_SwipeSend)
        Me.tb_KeyEvent.Controls.Add(Me.lbl_SwipeDuration)
        Me.tb_KeyEvent.Controls.Add(Me.txt_SwipeDuration)
        Me.tb_KeyEvent.Controls.Add(Me.lbl_SwipeY2)
        Me.tb_KeyEvent.Controls.Add(Me.txt_SwipeY2)
        Me.tb_KeyEvent.Controls.Add(Me.lbl_SwipeY1)
        Me.tb_KeyEvent.Controls.Add(Me.txt_SwipeY1)
        Me.tb_KeyEvent.Controls.Add(Me.lbl_SwipeX2)
        Me.tb_KeyEvent.Controls.Add(Me.txt_SwipeX2)
        Me.tb_KeyEvent.Controls.Add(Me.lbl_SwipeX1)
        Me.tb_KeyEvent.Controls.Add(Me.txt_SwipeX1)
        Me.tb_KeyEvent.Location = New System.Drawing.Point(4, 22)
        Me.tb_KeyEvent.Name = "tb_KeyEvent"
        Me.tb_KeyEvent.Size = New System.Drawing.Size(278, 100)
        Me.tb_KeyEvent.TabIndex = 2
        Me.tb_KeyEvent.Text = "Swipe Key"
        Me.tb_KeyEvent.UseVisualStyleBackColor = True
        '
        'btn_SwipeSend
        '
        Me.btn_SwipeSend.Location = New System.Drawing.Point(157, 62)
        Me.btn_SwipeSend.Name = "btn_SwipeSend"
        Me.btn_SwipeSend.Size = New System.Drawing.Size(110, 32)
        Me.btn_SwipeSend.TabIndex = 10
        Me.btn_SwipeSend.Text = "Send"
        Me.btn_SwipeSend.UseVisualStyleBackColor = True
        '
        'lbl_SwipeDuration
        '
        Me.lbl_SwipeDuration.AutoSize = True
        Me.lbl_SwipeDuration.Location = New System.Drawing.Point(5, 62)
        Me.lbl_SwipeDuration.Name = "lbl_SwipeDuration"
        Me.lbl_SwipeDuration.Size = New System.Drawing.Size(53, 13)
        Me.lbl_SwipeDuration.TabIndex = 9
        Me.lbl_SwipeDuration.Text = "Duration :"
        '
        'txt_SwipeDuration
        '
        Me.txt_SwipeDuration.Location = New System.Drawing.Point(75, 59)
        Me.txt_SwipeDuration.MaxLength = 4
        Me.txt_SwipeDuration.Name = "txt_SwipeDuration"
        Me.txt_SwipeDuration.Size = New System.Drawing.Size(43, 20)
        Me.txt_SwipeDuration.TabIndex = 8
        '
        'lbl_SwipeY2
        '
        Me.lbl_SwipeY2.AutoSize = True
        Me.lbl_SwipeY2.Location = New System.Drawing.Point(154, 36)
        Me.lbl_SwipeY2.Name = "lbl_SwipeY2"
        Me.lbl_SwipeY2.Size = New System.Drawing.Size(64, 13)
        Me.lbl_SwipeY2.TabIndex = 7
        Me.lbl_SwipeY2.Text = "Y2 Postion :"
        '
        'txt_SwipeY2
        '
        Me.txt_SwipeY2.Location = New System.Drawing.Point(224, 33)
        Me.txt_SwipeY2.MaxLength = 4
        Me.txt_SwipeY2.Name = "txt_SwipeY2"
        Me.txt_SwipeY2.Size = New System.Drawing.Size(43, 20)
        Me.txt_SwipeY2.TabIndex = 6
        '
        'lbl_SwipeY1
        '
        Me.lbl_SwipeY1.AutoSize = True
        Me.lbl_SwipeY1.Location = New System.Drawing.Point(154, 10)
        Me.lbl_SwipeY1.Name = "lbl_SwipeY1"
        Me.lbl_SwipeY1.Size = New System.Drawing.Size(64, 13)
        Me.lbl_SwipeY1.TabIndex = 5
        Me.lbl_SwipeY1.Text = "Y1 Postion :"
        '
        'txt_SwipeY1
        '
        Me.txt_SwipeY1.Location = New System.Drawing.Point(224, 7)
        Me.txt_SwipeY1.MaxLength = 4
        Me.txt_SwipeY1.Name = "txt_SwipeY1"
        Me.txt_SwipeY1.Size = New System.Drawing.Size(43, 20)
        Me.txt_SwipeY1.TabIndex = 4
        '
        'lbl_SwipeX2
        '
        Me.lbl_SwipeX2.AutoSize = True
        Me.lbl_SwipeX2.Location = New System.Drawing.Point(5, 36)
        Me.lbl_SwipeX2.Name = "lbl_SwipeX2"
        Me.lbl_SwipeX2.Size = New System.Drawing.Size(64, 13)
        Me.lbl_SwipeX2.TabIndex = 3
        Me.lbl_SwipeX2.Text = "X2 Postion :"
        '
        'txt_SwipeX2
        '
        Me.txt_SwipeX2.Location = New System.Drawing.Point(75, 33)
        Me.txt_SwipeX2.MaxLength = 4
        Me.txt_SwipeX2.Name = "txt_SwipeX2"
        Me.txt_SwipeX2.Size = New System.Drawing.Size(43, 20)
        Me.txt_SwipeX2.TabIndex = 2
        '
        'lbl_SwipeX1
        '
        Me.lbl_SwipeX1.AutoSize = True
        Me.lbl_SwipeX1.Location = New System.Drawing.Point(5, 10)
        Me.lbl_SwipeX1.Name = "lbl_SwipeX1"
        Me.lbl_SwipeX1.Size = New System.Drawing.Size(64, 13)
        Me.lbl_SwipeX1.TabIndex = 1
        Me.lbl_SwipeX1.Text = "X1 Postion :"
        '
        'txt_SwipeX1
        '
        Me.txt_SwipeX1.Location = New System.Drawing.Point(75, 7)
        Me.txt_SwipeX1.MaxLength = 4
        Me.txt_SwipeX1.Name = "txt_SwipeX1"
        Me.txt_SwipeX1.Size = New System.Drawing.Size(43, 20)
        Me.txt_SwipeX1.TabIndex = 0
        '
        'tb_Misc
        '
        Me.tb_Misc.Controls.Add(Me.btn_KeyboardInput)
        Me.tb_Misc.Controls.Add(Me.gb_ToogleRoot)
        Me.tb_Misc.Controls.Add(Me.btn_TurnOnOffScreen)
        Me.tb_Misc.Controls.Add(Me.btn_Screenshoot)
        Me.tb_Misc.Location = New System.Drawing.Point(4, 22)
        Me.tb_Misc.Name = "tb_Misc"
        Me.tb_Misc.Padding = New System.Windows.Forms.Padding(3)
        Me.tb_Misc.Size = New System.Drawing.Size(278, 100)
        Me.tb_Misc.TabIndex = 0
        Me.tb_Misc.Text = "Misc"
        Me.tb_Misc.UseVisualStyleBackColor = True
        '
        'btn_KeyboardInput
        '
        Me.btn_KeyboardInput.Location = New System.Drawing.Point(6, 68)
        Me.btn_KeyboardInput.Name = "btn_KeyboardInput"
        Me.btn_KeyboardInput.Size = New System.Drawing.Size(117, 26)
        Me.btn_KeyboardInput.TabIndex = 3
        Me.btn_KeyboardInput.Text = "Keyboard Input"
        Me.btn_KeyboardInput.UseVisualStyleBackColor = True
        '
        'gb_ToogleRoot
        '
        Me.gb_ToogleRoot.Controls.Add(Me.btn_ToogleRoot)
        Me.gb_ToogleRoot.Controls.Add(Me.cb_ToogleRoot)
        Me.gb_ToogleRoot.ForeColor = System.Drawing.Color.Red
        Me.gb_ToogleRoot.Location = New System.Drawing.Point(130, 7)
        Me.gb_ToogleRoot.Name = "gb_ToogleRoot"
        Me.gb_ToogleRoot.Size = New System.Drawing.Size(142, 87)
        Me.gb_ToogleRoot.TabIndex = 2
        Me.gb_ToogleRoot.TabStop = False
        Me.gb_ToogleRoot.Text = "Root Device Only"
        '
        'btn_ToogleRoot
        '
        Me.btn_ToogleRoot.Enabled = False
        Me.btn_ToogleRoot.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_ToogleRoot.ForeColor = System.Drawing.SystemColors.ControlText
        Me.btn_ToogleRoot.Location = New System.Drawing.Point(7, 50)
        Me.btn_ToogleRoot.Name = "btn_ToogleRoot"
        Me.btn_ToogleRoot.Size = New System.Drawing.Size(129, 25)
        Me.btn_ToogleRoot.TabIndex = 1
        Me.btn_ToogleRoot.Text = "OK"
        Me.btn_ToogleRoot.UseVisualStyleBackColor = True
        '
        'cb_ToogleRoot
        '
        Me.cb_ToogleRoot.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb_ToogleRoot.FormattingEnabled = True
        Me.cb_ToogleRoot.Items.AddRange(New Object() {"Wifi On", "WIfi Off", "Data On", "Data Off", "Stay Awake On", "Stay Awake Off"})
        Me.cb_ToogleRoot.Location = New System.Drawing.Point(7, 23)
        Me.cb_ToogleRoot.Name = "cb_ToogleRoot"
        Me.cb_ToogleRoot.Size = New System.Drawing.Size(129, 21)
        Me.cb_ToogleRoot.TabIndex = 0
        '
        'btn_TurnOnOffScreen
        '
        Me.btn_TurnOnOffScreen.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_TurnOnOffScreen.Location = New System.Drawing.Point(6, 37)
        Me.btn_TurnOnOffScreen.Name = "btn_TurnOnOffScreen"
        Me.btn_TurnOnOffScreen.Size = New System.Drawing.Size(118, 25)
        Me.btn_TurnOnOffScreen.TabIndex = 1
        Me.btn_TurnOnOffScreen.Text = "Turn On/Off Screen"
        Me.btn_TurnOnOffScreen.UseVisualStyleBackColor = True
        '
        'btn_Screenshoot
        '
        Me.btn_Screenshoot.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.btn_Screenshoot.Location = New System.Drawing.Point(5, 6)
        Me.btn_Screenshoot.Name = "btn_Screenshoot"
        Me.btn_Screenshoot.Size = New System.Drawing.Size(118, 25)
        Me.btn_Screenshoot.TabIndex = 0
        Me.btn_Screenshoot.Text = "Screenshoot"
        Me.btn_Screenshoot.UseVisualStyleBackColor = True
        '
        'tb_prefNetwork
        '
        Me.tb_prefNetwork.Controls.Add(Me.lbl_network2)
        Me.tb_prefNetwork.Controls.Add(Me.btn_prefnetwork)
        Me.tb_prefNetwork.Controls.Add(Me.cb_prefnetwork)
        Me.tb_prefNetwork.Controls.Add(Me.lbl_network1)
        Me.tb_prefNetwork.Location = New System.Drawing.Point(4, 22)
        Me.tb_prefNetwork.Name = "tb_prefNetwork"
        Me.tb_prefNetwork.Padding = New System.Windows.Forms.Padding(3)
        Me.tb_prefNetwork.Size = New System.Drawing.Size(278, 100)
        Me.tb_prefNetwork.TabIndex = 3
        Me.tb_prefNetwork.Text = "Network"
        Me.tb_prefNetwork.UseVisualStyleBackColor = True
        '
        'lbl_network2
        '
        Me.lbl_network2.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.lbl_network2.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_network2.ForeColor = System.Drawing.Color.Maroon
        Me.lbl_network2.Location = New System.Drawing.Point(7, 49)
        Me.lbl_network2.Name = "lbl_network2"
        Me.lbl_network2.Size = New System.Drawing.Size(265, 45)
        Me.lbl_network2.TabIndex = 3
        Me.lbl_network2.Text = "Sometimes Airplane Mode Is Not Getting Turned Off, Just Turning Off Manually And " & _
            "Voila Your Network is Changed.  (For Root Only)"
        Me.lbl_network2.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'btn_prefnetwork
        '
        Me.btn_prefnetwork.Enabled = False
        Me.btn_prefnetwork.Location = New System.Drawing.Point(197, 21)
        Me.btn_prefnetwork.Name = "btn_prefnetwork"
        Me.btn_prefnetwork.Size = New System.Drawing.Size(75, 23)
        Me.btn_prefnetwork.TabIndex = 2
        Me.btn_prefnetwork.Text = "Apply"
        Me.btn_prefnetwork.UseVisualStyleBackColor = True
        '
        'cb_prefnetwork
        '
        Me.cb_prefnetwork.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb_prefnetwork.FormattingEnabled = True
        Me.cb_prefnetwork.Items.AddRange(New Object() {"GSM Only", "GSM/WCDMA", "GSM/WCDMA/LTE", "WCDMA Only", "WCDMA/LTE", "LTE Only", "CDMA Only", "EvDo Only", "(Auto Mode)", "LTE/CDMA/EvDo"})
        Me.cb_prefnetwork.Location = New System.Drawing.Point(10, 23)
        Me.cb_prefnetwork.Name = "cb_prefnetwork"
        Me.cb_prefnetwork.Size = New System.Drawing.Size(181, 21)
        Me.cb_prefnetwork.TabIndex = 1
        '
        'lbl_network1
        '
        Me.lbl_network1.AutoSize = True
        Me.lbl_network1.Location = New System.Drawing.Point(7, 7)
        Me.lbl_network1.Name = "lbl_network1"
        Me.lbl_network1.Size = New System.Drawing.Size(118, 13)
        Me.lbl_network1.TabIndex = 0
        Me.lbl_network1.Text = "Preferred Network List :"
        '
        'gb_Dialer
        '
        Me.gb_Dialer.Controls.Add(Me.lbl_inputPhone)
        Me.gb_Dialer.Controls.Add(Me.btn_Call)
        Me.gb_Dialer.Controls.Add(Me.txt_Dialer)
        Me.gb_Dialer.Location = New System.Drawing.Point(7, 139)
        Me.gb_Dialer.Name = "gb_Dialer"
        Me.gb_Dialer.Size = New System.Drawing.Size(286, 69)
        Me.gb_Dialer.TabIndex = 4
        Me.gb_Dialer.TabStop = False
        Me.gb_Dialer.Text = "Dialer"
        '
        'lbl_inputPhone
        '
        Me.lbl_inputPhone.AutoSize = True
        Me.lbl_inputPhone.Location = New System.Drawing.Point(6, 23)
        Me.lbl_inputPhone.Name = "lbl_inputPhone"
        Me.lbl_inputPhone.Size = New System.Drawing.Size(111, 13)
        Me.lbl_inputPhone.TabIndex = 2
        Me.lbl_inputPhone.Text = "Input Phone Number :"
        '
        'btn_Call
        '
        Me.btn_Call.Location = New System.Drawing.Point(224, 38)
        Me.btn_Call.Name = "btn_Call"
        Me.btn_Call.Size = New System.Drawing.Size(55, 25)
        Me.btn_Call.TabIndex = 1
        Me.btn_Call.Text = "Call"
        Me.btn_Call.UseVisualStyleBackColor = True
        '
        'txt_Dialer
        '
        Me.txt_Dialer.Location = New System.Drawing.Point(7, 41)
        Me.txt_Dialer.Name = "txt_Dialer"
        Me.txt_Dialer.Size = New System.Drawing.Size(212, 20)
        Me.txt_Dialer.TabIndex = 0
        '
        'gb_ScreenRecord
        '
        Me.gb_ScreenRecord.Controls.Add(Me.cb_ShowTouches)
        Me.gb_ScreenRecord.Controls.Add(Me.lbl_RecordProg)
        Me.gb_ScreenRecord.Controls.Add(Me.lbl_Bitrate_2)
        Me.gb_ScreenRecord.Controls.Add(Me.lbl_TimeVid_2)
        Me.gb_ScreenRecord.Controls.Add(Me.lbl_x)
        Me.gb_ScreenRecord.Controls.Add(Me.txt_VideoBitrate)
        Me.gb_ScreenRecord.Controls.Add(Me.txt_VideoTime)
        Me.gb_ScreenRecord.Controls.Add(Me.txt_VidResH)
        Me.gb_ScreenRecord.Controls.Add(Me.txt_VidResW)
        Me.gb_ScreenRecord.Controls.Add(Me.lbl_TimeVid)
        Me.gb_ScreenRecord.Controls.Add(Me.lbl_Bitrate)
        Me.gb_ScreenRecord.Controls.Add(Me.lbl_VidRes)
        Me.gb_ScreenRecord.Controls.Add(Me.btn_RecordVideo)
        Me.gb_ScreenRecord.Controls.Add(Me.pb_RecordVideo)
        Me.gb_ScreenRecord.Location = New System.Drawing.Point(299, 6)
        Me.gb_ScreenRecord.Name = "gb_ScreenRecord"
        Me.gb_ScreenRecord.Size = New System.Drawing.Size(249, 202)
        Me.gb_ScreenRecord.TabIndex = 2
        Me.gb_ScreenRecord.TabStop = False
        Me.gb_ScreenRecord.Text = "Screen Recording (Android 4.4+)"
        '
        'cb_ShowTouches
        '
        Me.cb_ShowTouches.AutoSize = True
        Me.cb_ShowTouches.Location = New System.Drawing.Point(10, 100)
        Me.cb_ShowTouches.Name = "cb_ShowTouches"
        Me.cb_ShowTouches.Size = New System.Drawing.Size(98, 17)
        Me.cb_ShowTouches.TabIndex = 13
        Me.cb_ShowTouches.Text = "Show Touches"
        Me.cb_ShowTouches.UseVisualStyleBackColor = True
        '
        'lbl_RecordProg
        '
        Me.lbl_RecordProg.AutoSize = True
        Me.lbl_RecordProg.Location = New System.Drawing.Point(4, 126)
        Me.lbl_RecordProg.Name = "lbl_RecordProg"
        Me.lbl_RecordProg.Size = New System.Drawing.Size(106, 13)
        Me.lbl_RecordProg.TabIndex = 12
        Me.lbl_RecordProg.Text = "Recording Progress :"
        '
        'lbl_Bitrate_2
        '
        Me.lbl_Bitrate_2.AutoSize = True
        Me.lbl_Bitrate_2.Location = New System.Drawing.Point(132, 79)
        Me.lbl_Bitrate_2.Name = "lbl_Bitrate_2"
        Me.lbl_Bitrate_2.Size = New System.Drawing.Size(100, 13)
        Me.lbl_Bitrate_2.TabIndex = 11
        Me.lbl_Bitrate_2.Text = "(eg. 4000000/4MB)"
        '
        'lbl_TimeVid_2
        '
        Me.lbl_TimeVid_2.AutoSize = True
        Me.lbl_TimeVid_2.Location = New System.Drawing.Point(132, 53)
        Me.lbl_TimeVid_2.Name = "lbl_TimeVid_2"
        Me.lbl_TimeVid_2.Size = New System.Drawing.Size(76, 13)
        Me.lbl_TimeVid_2.TabIndex = 10
        Me.lbl_TimeVid_2.Text = "(Max 180 Sec)"
        '
        'lbl_x
        '
        Me.lbl_x.AutoSize = True
        Me.lbl_x.Location = New System.Drawing.Point(132, 23)
        Me.lbl_x.Name = "lbl_x"
        Me.lbl_x.Size = New System.Drawing.Size(14, 13)
        Me.lbl_x.TabIndex = 9
        Me.lbl_x.Text = "X"
        '
        'txt_VideoBitrate
        '
        Me.txt_VideoBitrate.Location = New System.Drawing.Point(76, 72)
        Me.txt_VideoBitrate.MaxLength = 9
        Me.txt_VideoBitrate.Name = "txt_VideoBitrate"
        Me.txt_VideoBitrate.Size = New System.Drawing.Size(50, 20)
        Me.txt_VideoBitrate.TabIndex = 8
        '
        'txt_VideoTime
        '
        Me.txt_VideoTime.Location = New System.Drawing.Point(76, 46)
        Me.txt_VideoTime.MaxLength = 4
        Me.txt_VideoTime.Name = "txt_VideoTime"
        Me.txt_VideoTime.Size = New System.Drawing.Size(50, 20)
        Me.txt_VideoTime.TabIndex = 7
        '
        'txt_VidResH
        '
        Me.txt_VidResH.Location = New System.Drawing.Point(152, 20)
        Me.txt_VidResH.MaxLength = 4
        Me.txt_VidResH.Name = "txt_VidResH"
        Me.txt_VidResH.Size = New System.Drawing.Size(50, 20)
        Me.txt_VidResH.TabIndex = 6
        '
        'txt_VidResW
        '
        Me.txt_VidResW.Location = New System.Drawing.Point(76, 20)
        Me.txt_VidResW.MaxLength = 4
        Me.txt_VidResW.Name = "txt_VidResW"
        Me.txt_VidResW.Size = New System.Drawing.Size(50, 20)
        Me.txt_VidResW.TabIndex = 5
        '
        'lbl_TimeVid
        '
        Me.lbl_TimeVid.AutoSize = True
        Me.lbl_TimeVid.Location = New System.Drawing.Point(7, 53)
        Me.lbl_TimeVid.Name = "lbl_TimeVid"
        Me.lbl_TimeVid.Size = New System.Drawing.Size(36, 13)
        Me.lbl_TimeVid.TabIndex = 4
        Me.lbl_TimeVid.Text = "Time :"
        '
        'lbl_Bitrate
        '
        Me.lbl_Bitrate.AutoSize = True
        Me.lbl_Bitrate.Location = New System.Drawing.Point(7, 79)
        Me.lbl_Bitrate.Name = "lbl_Bitrate"
        Me.lbl_Bitrate.Size = New System.Drawing.Size(43, 13)
        Me.lbl_Bitrate.TabIndex = 3
        Me.lbl_Bitrate.Text = "Bitrate :"
        '
        'lbl_VidRes
        '
        Me.lbl_VidRes.AutoSize = True
        Me.lbl_VidRes.Location = New System.Drawing.Point(7, 27)
        Me.lbl_VidRes.Name = "lbl_VidRes"
        Me.lbl_VidRes.Size = New System.Drawing.Size(63, 13)
        Me.lbl_VidRes.TabIndex = 2
        Me.lbl_VidRes.Text = "Resolution :"
        '
        'btn_RecordVideo
        '
        Me.btn_RecordVideo.Location = New System.Drawing.Point(6, 171)
        Me.btn_RecordVideo.Name = "btn_RecordVideo"
        Me.btn_RecordVideo.Size = New System.Drawing.Size(236, 23)
        Me.btn_RecordVideo.TabIndex = 1
        Me.btn_RecordVideo.Text = "Record Now"
        Me.btn_RecordVideo.UseVisualStyleBackColor = True
        '
        'pb_RecordVideo
        '
        Me.pb_RecordVideo.Location = New System.Drawing.Point(7, 142)
        Me.pb_RecordVideo.Name = "pb_RecordVideo"
        Me.pb_RecordVideo.Size = New System.Drawing.Size(236, 23)
        Me.pb_RecordVideo.TabIndex = 0
        '
        'tb_buildprop
        '
        Me.tb_buildprop.Controls.Add(Me.lbl_BuildPropStats)
        Me.tb_buildprop.Controls.Add(Me.txt_buildprop)
        Me.tb_buildprop.Controls.Add(Me.lbl_buildprophow)
        Me.tb_buildprop.Controls.Add(Me.gb_buildprop)
        Me.tb_buildprop.Location = New System.Drawing.Point(4, 22)
        Me.tb_buildprop.Name = "tb_buildprop"
        Me.tb_buildprop.Padding = New System.Windows.Forms.Padding(3)
        Me.tb_buildprop.Size = New System.Drawing.Size(554, 214)
        Me.tb_buildprop.TabIndex = 5
        Me.tb_buildprop.Text = "Build Prop Editor"
        Me.tb_buildprop.UseVisualStyleBackColor = True
        '
        'lbl_BuildPropStats
        '
        Me.lbl_BuildPropStats.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.lbl_BuildPropStats.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_BuildPropStats.ForeColor = System.Drawing.Color.Black
        Me.lbl_BuildPropStats.Location = New System.Drawing.Point(242, 165)
        Me.lbl_BuildPropStats.Name = "lbl_BuildPropStats"
        Me.lbl_BuildPropStats.Size = New System.Drawing.Size(188, 43)
        Me.lbl_BuildPropStats.TabIndex = 4
        Me.lbl_BuildPropStats.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'txt_buildprop
        '
        Me.txt_buildprop.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.txt_buildprop.Enabled = False
        Me.txt_buildprop.IsReadOnly = False
        Me.txt_buildprop.LineViewerStyle = ICSharpCode.TextEditor.Document.LineViewerStyle.FullRow
        Me.txt_buildprop.Location = New System.Drawing.Point(3, 6)
        Me.txt_buildprop.Name = "txt_buildprop"
        Me.txt_buildprop.Size = New System.Drawing.Size(549, 153)
        Me.txt_buildprop.TabIndex = 3
        '
        'lbl_buildprophow
        '
        Me.lbl_buildprophow.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_buildprophow.ForeColor = System.Drawing.Color.DarkRed
        Me.lbl_buildprophow.Location = New System.Drawing.Point(436, 165)
        Me.lbl_buildprophow.Name = "lbl_buildprophow"
        Me.lbl_buildprophow.Size = New System.Drawing.Size(112, 43)
        Me.lbl_buildprophow.TabIndex = 2
        Me.lbl_buildprophow.Text = "Always add empty line on last line"
        Me.lbl_buildprophow.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'gb_buildprop
        '
        Me.gb_buildprop.Controls.Add(Me.btn_LoadBuildProp)
        Me.gb_buildprop.Controls.Add(Me.btn_BackupBuildProp)
        Me.gb_buildprop.Controls.Add(Me.btn_SaveBuildProp)
        Me.gb_buildprop.Location = New System.Drawing.Point(8, 165)
        Me.gb_buildprop.Name = "gb_buildprop"
        Me.gb_buildprop.Size = New System.Drawing.Size(228, 43)
        Me.gb_buildprop.TabIndex = 0
        Me.gb_buildprop.TabStop = False
        Me.gb_buildprop.Text = "Commands"
        '
        'btn_LoadBuildProp
        '
        Me.btn_LoadBuildProp.Location = New System.Drawing.Point(6, 14)
        Me.btn_LoadBuildProp.Name = "btn_LoadBuildProp"
        Me.btn_LoadBuildProp.Size = New System.Drawing.Size(66, 23)
        Me.btn_LoadBuildProp.TabIndex = 0
        Me.btn_LoadBuildProp.Text = "Load"
        Me.btn_LoadBuildProp.UseVisualStyleBackColor = True
        '
        'btn_BackupBuildProp
        '
        Me.btn_BackupBuildProp.Enabled = False
        Me.btn_BackupBuildProp.Location = New System.Drawing.Point(152, 14)
        Me.btn_BackupBuildProp.Name = "btn_BackupBuildProp"
        Me.btn_BackupBuildProp.Size = New System.Drawing.Size(66, 23)
        Me.btn_BackupBuildProp.TabIndex = 2
        Me.btn_BackupBuildProp.Text = "Backup"
        Me.btn_BackupBuildProp.UseVisualStyleBackColor = True
        '
        'btn_SaveBuildProp
        '
        Me.btn_SaveBuildProp.Enabled = False
        Me.btn_SaveBuildProp.Location = New System.Drawing.Point(78, 14)
        Me.btn_SaveBuildProp.Name = "btn_SaveBuildProp"
        Me.btn_SaveBuildProp.Size = New System.Drawing.Size(66, 23)
        Me.btn_SaveBuildProp.TabIndex = 1
        Me.btn_SaveBuildProp.Text = "Save"
        Me.btn_SaveBuildProp.UseVisualStyleBackColor = True
        '
        'tb_ADBSettings
        '
        Me.tb_ADBSettings.Controls.Add(Me.lbl_Warn1)
        Me.tb_ADBSettings.Controls.Add(Me.gb_ADBSettings)
        Me.tb_ADBSettings.Controls.Add(Me.gb_ADBRestart)
        Me.tb_ADBSettings.Controls.Add(Me.gb_ADBWifi)
        Me.tb_ADBSettings.Location = New System.Drawing.Point(4, 22)
        Me.tb_ADBSettings.Name = "tb_ADBSettings"
        Me.tb_ADBSettings.Size = New System.Drawing.Size(554, 214)
        Me.tb_ADBSettings.TabIndex = 3
        Me.tb_ADBSettings.Text = "ADB Settings"
        Me.tb_ADBSettings.UseVisualStyleBackColor = True
        '
        'lbl_Warn1
        '
        Me.lbl_Warn1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_Warn1.ForeColor = System.Drawing.Color.Red
        Me.lbl_Warn1.Location = New System.Drawing.Point(4, 167)
        Me.lbl_Warn1.Name = "lbl_Warn1"
        Me.lbl_Warn1.Size = New System.Drawing.Size(303, 44)
        Me.lbl_Warn1.TabIndex = 3
        Me.lbl_Warn1.Text = "Some Settings Is Not Implemented Yet" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "Still Need More Testing"
        Me.lbl_Warn1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'gb_ADBSettings
        '
        Me.gb_ADBSettings.Controls.Add(Me.btn_jwdp)
        Me.gb_ADBSettings.Controls.Add(Me.btn_DisableVerity)
        Me.gb_ADBSettings.Controls.Add(Me.btn_EnableVerity)
        Me.gb_ADBSettings.Controls.Add(Me.btn_ADBKeygen)
        Me.gb_ADBSettings.Enabled = False
        Me.gb_ADBSettings.Location = New System.Drawing.Point(313, 117)
        Me.gb_ADBSettings.Name = "gb_ADBSettings"
        Me.gb_ADBSettings.Size = New System.Drawing.Size(235, 94)
        Me.gb_ADBSettings.TabIndex = 2
        Me.gb_ADBSettings.TabStop = False
        Me.gb_ADBSettings.Text = "Other Settings"
        '
        'btn_jwdp
        '
        Me.btn_jwdp.Location = New System.Drawing.Point(118, 59)
        Me.btn_jwdp.Name = "btn_jwdp"
        Me.btn_jwdp.Size = New System.Drawing.Size(111, 29)
        Me.btn_jwdp.TabIndex = 8
        Me.btn_jwdp.Text = "JWDP"
        Me.btn_jwdp.UseVisualStyleBackColor = True
        '
        'btn_DisableVerity
        '
        Me.btn_DisableVerity.Location = New System.Drawing.Point(118, 18)
        Me.btn_DisableVerity.Name = "btn_DisableVerity"
        Me.btn_DisableVerity.Size = New System.Drawing.Size(111, 29)
        Me.btn_DisableVerity.TabIndex = 7
        Me.btn_DisableVerity.Text = "Disable Verity"
        Me.btn_DisableVerity.UseVisualStyleBackColor = True
        '
        'btn_EnableVerity
        '
        Me.btn_EnableVerity.Location = New System.Drawing.Point(6, 18)
        Me.btn_EnableVerity.Name = "btn_EnableVerity"
        Me.btn_EnableVerity.Size = New System.Drawing.Size(111, 29)
        Me.btn_EnableVerity.TabIndex = 6
        Me.btn_EnableVerity.Text = "Enable Verity"
        Me.btn_EnableVerity.UseVisualStyleBackColor = True
        '
        'btn_ADBKeygen
        '
        Me.btn_ADBKeygen.Location = New System.Drawing.Point(6, 59)
        Me.btn_ADBKeygen.Name = "btn_ADBKeygen"
        Me.btn_ADBKeygen.Size = New System.Drawing.Size(111, 29)
        Me.btn_ADBKeygen.TabIndex = 4
        Me.btn_ADBKeygen.Text = "Re-Create Keygen"
        Me.btn_ADBKeygen.UseVisualStyleBackColor = True
        '
        'gb_ADBRestart
        '
        Me.gb_ADBRestart.Controls.Add(Me.lbl_tcport)
        Me.gb_ADBRestart.Controls.Add(Me.txt_tcport)
        Me.gb_ADBRestart.Controls.Add(Me.btn_adbTCPIP)
        Me.gb_ADBRestart.Controls.Add(Me.btn_adbUSB)
        Me.gb_ADBRestart.Controls.Add(Me.btn_AdbUnroot)
        Me.gb_ADBRestart.Controls.Add(Me.btn_adbRoot)
        Me.gb_ADBRestart.Cursor = System.Windows.Forms.Cursors.Default
        Me.gb_ADBRestart.Location = New System.Drawing.Point(312, 4)
        Me.gb_ADBRestart.Name = "gb_ADBRestart"
        Me.gb_ADBRestart.Size = New System.Drawing.Size(236, 106)
        Me.gb_ADBRestart.TabIndex = 1
        Me.gb_ADBRestart.TabStop = False
        Me.gb_ADBRestart.Text = "ADB Restart"
        '
        'lbl_tcport
        '
        Me.lbl_tcport.AutoSize = True
        Me.lbl_tcport.Location = New System.Drawing.Point(124, 82)
        Me.lbl_tcport.Name = "lbl_tcport"
        Me.lbl_tcport.Size = New System.Drawing.Size(66, 13)
        Me.lbl_tcport.TabIndex = 5
        Me.lbl_tcport.Text = "TCPIP Port :"
        '
        'txt_tcport
        '
        Me.txt_tcport.Location = New System.Drawing.Point(196, 78)
        Me.txt_tcport.Name = "txt_tcport"
        Me.txt_tcport.Size = New System.Drawing.Size(34, 20)
        Me.txt_tcport.TabIndex = 4
        Me.txt_tcport.Text = "5555"
        Me.txt_tcport.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'btn_adbTCPIP
        '
        Me.btn_adbTCPIP.Location = New System.Drawing.Point(119, 49)
        Me.btn_adbTCPIP.Name = "btn_adbTCPIP"
        Me.btn_adbTCPIP.Size = New System.Drawing.Size(111, 23)
        Me.btn_adbTCPIP.TabIndex = 3
        Me.btn_adbTCPIP.Text = "ADB TCPIP"
        Me.btn_adbTCPIP.UseVisualStyleBackColor = True
        '
        'btn_adbUSB
        '
        Me.btn_adbUSB.Location = New System.Drawing.Point(7, 49)
        Me.btn_adbUSB.Name = "btn_adbUSB"
        Me.btn_adbUSB.Size = New System.Drawing.Size(111, 23)
        Me.btn_adbUSB.TabIndex = 2
        Me.btn_adbUSB.Text = "ADB USB"
        Me.btn_adbUSB.UseVisualStyleBackColor = True
        '
        'btn_AdbUnroot
        '
        Me.btn_AdbUnroot.Location = New System.Drawing.Point(119, 20)
        Me.btn_AdbUnroot.Name = "btn_AdbUnroot"
        Me.btn_AdbUnroot.Size = New System.Drawing.Size(111, 23)
        Me.btn_AdbUnroot.TabIndex = 1
        Me.btn_AdbUnroot.Text = "ADB Unroot"
        Me.btn_AdbUnroot.UseVisualStyleBackColor = True
        '
        'btn_adbRoot
        '
        Me.btn_adbRoot.Location = New System.Drawing.Point(7, 20)
        Me.btn_adbRoot.Name = "btn_adbRoot"
        Me.btn_adbRoot.Size = New System.Drawing.Size(111, 23)
        Me.btn_adbRoot.TabIndex = 0
        Me.btn_adbRoot.Text = "ADB Root"
        Me.btn_adbRoot.UseVisualStyleBackColor = True
        '
        'gb_ADBWifi
        '
        Me.gb_ADBWifi.Controls.Add(Me.lbl_HowToIP)
        Me.gb_ADBWifi.Controls.Add(Me.lbl_defPort)
        Me.gb_ADBWifi.Controls.Add(Me.btn_connectwifi)
        Me.gb_ADBWifi.Controls.Add(Me.lbl_DevIP)
        Me.gb_ADBWifi.Controls.Add(Me.lbl_port)
        Me.gb_ADBWifi.Controls.Add(Me.txt_ipaddr)
        Me.gb_ADBWifi.Controls.Add(Me.txt_tcpip)
        Me.gb_ADBWifi.Location = New System.Drawing.Point(7, 4)
        Me.gb_ADBWifi.Name = "gb_ADBWifi"
        Me.gb_ADBWifi.Size = New System.Drawing.Size(299, 160)
        Me.gb_ADBWifi.TabIndex = 0
        Me.gb_ADBWifi.TabStop = False
        Me.gb_ADBWifi.Text = "Connect Over WiFi"
        '
        'lbl_HowToIP
        '
        Me.lbl_HowToIP.Location = New System.Drawing.Point(9, 82)
        Me.lbl_HowToIP.Name = "lbl_HowToIP"
        Me.lbl_HowToIP.Size = New System.Drawing.Size(284, 44)
        Me.lbl_HowToIP.TabIndex = 7
        Me.lbl_HowToIP.Text = "To Check IP On Your Device :" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "Settings -> About -> Status -> IP Address"
        Me.lbl_HowToIP.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'lbl_defPort
        '
        Me.lbl_defPort.AutoSize = True
        Me.lbl_defPort.Location = New System.Drawing.Point(157, 25)
        Me.lbl_defPort.Name = "lbl_defPort"
        Me.lbl_defPort.Size = New System.Drawing.Size(138, 13)
        Me.lbl_defPort.TabIndex = 5
        Me.lbl_defPort.Text = "(Default / Standard is 5555)"
        '
        'btn_connectwifi
        '
        Me.btn_connectwifi.Location = New System.Drawing.Point(9, 131)
        Me.btn_connectwifi.Name = "btn_connectwifi"
        Me.btn_connectwifi.Size = New System.Drawing.Size(284, 23)
        Me.btn_connectwifi.TabIndex = 4
        Me.btn_connectwifi.Text = "Connect"
        Me.btn_connectwifi.UseVisualStyleBackColor = True
        '
        'lbl_DevIP
        '
        Me.lbl_DevIP.AutoSize = True
        Me.lbl_DevIP.Location = New System.Drawing.Point(6, 51)
        Me.lbl_DevIP.Name = "lbl_DevIP"
        Me.lbl_DevIP.Size = New System.Drawing.Size(101, 13)
        Me.lbl_DevIP.TabIndex = 3
        Me.lbl_DevIP.Text = "Device IP Address :"
        '
        'lbl_port
        '
        Me.lbl_port.AutoSize = True
        Me.lbl_port.Location = New System.Drawing.Point(6, 25)
        Me.lbl_port.Name = "lbl_port"
        Me.lbl_port.Size = New System.Drawing.Size(97, 13)
        Me.lbl_port.TabIndex = 2
        Me.lbl_port.Text = "ADB Port (TCPIP) :"
        '
        'txt_ipaddr
        '
        Me.txt_ipaddr.Location = New System.Drawing.Point(113, 48)
        Me.txt_ipaddr.Name = "txt_ipaddr"
        Me.txt_ipaddr.Size = New System.Drawing.Size(180, 20)
        Me.txt_ipaddr.TabIndex = 1
        '
        'txt_tcpip
        '
        Me.txt_tcpip.Location = New System.Drawing.Point(113, 22)
        Me.txt_tcpip.Name = "txt_tcpip"
        Me.txt_tcpip.Size = New System.Drawing.Size(38, 20)
        Me.txt_tcpip.TabIndex = 0
        Me.txt_tcpip.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'tb_About
        '
        Me.tb_About.Controls.Add(Me.gb_Thanks)
        Me.tb_About.Controls.Add(Me.lbl_disclaimer)
        Me.tb_About.Controls.Add(Me.txtbox_License)
        Me.tb_About.Controls.Add(Me.gb_About)
        Me.tb_About.Location = New System.Drawing.Point(4, 22)
        Me.tb_About.Name = "tb_About"
        Me.tb_About.Padding = New System.Windows.Forms.Padding(3)
        Me.tb_About.Size = New System.Drawing.Size(554, 214)
        Me.tb_About.TabIndex = 4
        Me.tb_About.Text = "About"
        Me.tb_About.UseVisualStyleBackColor = True
        '
        'gb_Thanks
        '
        Me.gb_Thanks.Controls.Add(Me.lbl_Thanks)
        Me.gb_Thanks.Location = New System.Drawing.Point(213, 116)
        Me.gb_Thanks.Name = "gb_Thanks"
        Me.gb_Thanks.Size = New System.Drawing.Size(334, 89)
        Me.gb_Thanks.TabIndex = 3
        Me.gb_Thanks.TabStop = False
        Me.gb_Thanks.Text = "Special Thanks To"
        '
        'lbl_Thanks
        '
        Me.lbl_Thanks.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_Thanks.ForeColor = System.Drawing.Color.ForestGreen
        Me.lbl_Thanks.Location = New System.Drawing.Point(6, 16)
        Me.lbl_Thanks.Name = "lbl_Thanks"
        Me.lbl_Thanks.Size = New System.Drawing.Size(322, 70)
        Me.lbl_Thanks.TabIndex = 0
        Me.lbl_Thanks.Text = resources.GetString("lbl_Thanks.Text")
        Me.lbl_Thanks.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'lbl_disclaimer
        '
        Me.lbl_disclaimer.AutoSize = True
        Me.lbl_disclaimer.Location = New System.Drawing.Point(214, 7)
        Me.lbl_disclaimer.Name = "lbl_disclaimer"
        Me.lbl_disclaimer.Size = New System.Drawing.Size(61, 13)
        Me.lbl_disclaimer.TabIndex = 2
        Me.lbl_disclaimer.Text = "Disclaimer :"
        '
        'txtbox_License
        '
        Me.txtbox_License.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.txtbox_License.Cursor = System.Windows.Forms.Cursors.Default
        Me.txtbox_License.Font = New System.Drawing.Font("Courier New", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.txtbox_License.Location = New System.Drawing.Point(213, 23)
        Me.txtbox_License.Multiline = True
        Me.txtbox_License.Name = "txtbox_License"
        Me.txtbox_License.ReadOnly = True
        Me.txtbox_License.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.txtbox_License.Size = New System.Drawing.Size(334, 87)
        Me.txtbox_License.TabIndex = 1
        Me.txtbox_License.Text = resources.GetString("txtbox_License.Text")
        Me.txtbox_License.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'gb_About
        '
        Me.gb_About.Controls.Add(Me.ll_Github)
        Me.gb_About.Controls.Add(Me.lbl_About)
        Me.gb_About.Location = New System.Drawing.Point(7, 0)
        Me.gb_About.Name = "gb_About"
        Me.gb_About.Size = New System.Drawing.Size(200, 205)
        Me.gb_About.TabIndex = 0
        Me.gb_About.TabStop = False
        '
        'lbl_About
        '
        Me.lbl_About.Font = New System.Drawing.Font("Comic Sans MS", 9.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_About.Location = New System.Drawing.Point(7, 16)
        Me.lbl_About.Name = "lbl_About"
        Me.lbl_About.Size = New System.Drawing.Size(187, 165)
        Me.lbl_About.TabIndex = 0
        Me.lbl_About.Text = "Created by :" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "Muhammad Ravhi Rizaldi" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "ADB version :" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "1.0.32" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "This software is" & _
            " freeware" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "Under GNU GPL 3.0 License" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "Check Github for Source Code"
        Me.lbl_About.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'Timer1
        '
        Me.Timer1.Interval = 1000
        '
        'lbl_Header
        '
        Me.lbl_Header.Font = New System.Drawing.Font("Bookman Old Style", 36.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbl_Header.ForeColor = System.Drawing.Color.ForestGreen
        Me.lbl_Header.Location = New System.Drawing.Point(13, 9)
        Me.lbl_Header.Name = "lbl_Header"
        Me.lbl_Header.Size = New System.Drawing.Size(561, 56)
        Me.lbl_Header.TabIndex = 6
        Me.lbl_Header.Text = "Android Tools"
        Me.lbl_Header.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'll_Update
        '
        Me.ll_Update.AutoSize = True
        Me.ll_Update.Location = New System.Drawing.Point(431, 487)
        Me.ll_Update.Name = "ll_Update"
        Me.ll_Update.Size = New System.Drawing.Size(76, 13)
        Me.ll_Update.TabIndex = 8
        Me.ll_Update.TabStop = True
        Me.ll_Update.Text = "Check Update"
        '
        'll_Github
        '
        Me.ll_Github.AutoSize = True
        Me.ll_Github.Location = New System.Drawing.Point(81, 181)
        Me.ll_Github.Name = "ll_Github"
        Me.ll_Github.Size = New System.Drawing.Size(38, 13)
        Me.ll_Github.TabIndex = 9
        Me.ll_Github.TabStop = True
        Me.ll_Github.Text = "Github"
        '
        'frm_Main
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.White
        Me.ClientSize = New System.Drawing.Size(586, 509)
        Me.Controls.Add(Me.ll_Update)
        Me.Controls.Add(Me.lbl_Header)
        Me.Controls.Add(Me.ll_Email)
        Me.Controls.Add(Me.tc_MAIN)
        Me.Controls.Add(Me.gb_DevInfo)
        Me.Controls.Add(Me.gb_Logs)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "frm_Main"
        Me.Opacity = 0.99R
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Android Tools"
        Me.gb_Logs.ResumeLayout(False)
        Me.gb_Logs.PerformLayout()
        Me.gb_RebootOptions.ResumeLayout(False)
        Me.gb_DevInfo.ResumeLayout(False)
        Me.tc_MAIN.ResumeLayout(False)
        Me.tb_MainTools.ResumeLayout(False)
        Me.GroupBox1.ResumeLayout(False)
        Me.gb_ADBCommand.ResumeLayout(False)
        Me.gb_Sideload.ResumeLayout(False)
        Me.gb_Sideload.PerformLayout()
        Me.gb_ManualADB.ResumeLayout(False)
        Me.gb_ManualADB.PerformLayout()
        Me.tb_AppTools.ResumeLayout(False)
        Me.tb_AppTools.PerformLayout()
        Me.gb_APKOptions.ResumeLayout(False)
        Me.tb_Xtools.ResumeLayout(False)
        Me.tc_Extra.ResumeLayout(False)
        Me.tb_InputKeyEvent.ResumeLayout(False)
        Me.tb_InputKeyEvent.PerformLayout()
        Me.tb_KeyEvent.ResumeLayout(False)
        Me.tb_KeyEvent.PerformLayout()
        Me.tb_Misc.ResumeLayout(False)
        Me.gb_ToogleRoot.ResumeLayout(False)
        Me.tb_prefNetwork.ResumeLayout(False)
        Me.tb_prefNetwork.PerformLayout()
        Me.gb_Dialer.ResumeLayout(False)
        Me.gb_Dialer.PerformLayout()
        Me.gb_ScreenRecord.ResumeLayout(False)
        Me.gb_ScreenRecord.PerformLayout()
        Me.tb_buildprop.ResumeLayout(False)
        Me.gb_buildprop.ResumeLayout(False)
        Me.tb_ADBSettings.ResumeLayout(False)
        Me.gb_ADBSettings.ResumeLayout(False)
        Me.gb_ADBRestart.ResumeLayout(False)
        Me.gb_ADBRestart.PerformLayout()
        Me.gb_ADBWifi.ResumeLayout(False)
        Me.gb_ADBWifi.PerformLayout()
        Me.tb_About.ResumeLayout(False)
        Me.tb_About.PerformLayout()
        Me.gb_Thanks.ResumeLayout(False)
        Me.gb_About.ResumeLayout(False)
        Me.gb_About.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents gb_Logs As System.Windows.Forms.GroupBox
    Friend WithEvents txt_Logs As System.Windows.Forms.TextBox
    Friend WithEvents lbl_DevInfo As System.Windows.Forms.Label
    Friend WithEvents gb_RebootOptions As System.Windows.Forms.GroupBox
    Friend WithEvents gb_DevInfo As System.Windows.Forms.GroupBox
    Friend WithEvents cb_RebootOptions As System.Windows.Forms.ComboBox
    Friend WithEvents btn_Reboot As System.Windows.Forms.Button
    Friend WithEvents tc_MAIN As System.Windows.Forms.TabControl
    Friend WithEvents tb_MainTools As System.Windows.Forms.TabPage
    Friend WithEvents tb_Xtools As System.Windows.Forms.TabPage
    Friend WithEvents tb_AppTools As System.Windows.Forms.TabPage
    Friend WithEvents gb_APKOptions As System.Windows.Forms.GroupBox
    Friend WithEvents txt_AppName As System.Windows.Forms.TextBox
    Friend WithEvents btn_UninstallAPK As System.Windows.Forms.Button
    Friend WithEvents list_APK As System.Windows.Forms.ListBox
    Friend WithEvents btn_RefreshListAPK As System.Windows.Forms.Button
    Friend WithEvents btn_ClearAPKData As System.Windows.Forms.Button
    Friend WithEvents btn_InstallAPK As System.Windows.Forms.Button
    Friend WithEvents btn_ClearLogs As System.Windows.Forms.Button
    Friend WithEvents btn_TurnOnOffScreen As System.Windows.Forms.Button
    Friend WithEvents btn_Screenshoot As System.Windows.Forms.Button
    Friend WithEvents btn_ForceCloseAPK As System.Windows.Forms.Button
    Friend WithEvents gb_ManualADB As System.Windows.Forms.GroupBox
    Friend WithEvents btn_ExecuteADB As System.Windows.Forms.Button
    Friend WithEvents txt_RunADB As System.Windows.Forms.TextBox
    Friend WithEvents btn_RunAPK As System.Windows.Forms.Button
    Friend WithEvents btn_BackupAPK As System.Windows.Forms.Button
    Friend WithEvents lbl_Battery As System.Windows.Forms.Label
    Friend WithEvents lbl_Batt As System.Windows.Forms.Label
    Friend WithEvents lbl_DevMdl As System.Windows.Forms.Label
    Friend WithEvents txt_Path As System.Windows.Forms.TextBox
    Friend WithEvents btn_RestoreAPK As System.Windows.Forms.Button
    Friend WithEvents btn_PushFile As System.Windows.Forms.Button
    Friend WithEvents gb_Sideload As System.Windows.Forms.GroupBox
    Friend WithEvents cb_SideLoad As System.Windows.Forms.CheckBox
    Friend WithEvents btn_Sideload As System.Windows.Forms.Button
    Friend WithEvents txt_SideloadPath As System.Windows.Forms.TextBox
    Friend WithEvents gb_ADBCommand As System.Windows.Forms.GroupBox
    Friend WithEvents btn_ShellConsole As System.Windows.Forms.Button
    Friend WithEvents lbl_ApkPath As System.Windows.Forms.Label
    Friend WithEvents lbl_AppName As System.Windows.Forms.Label
    Friend WithEvents gb_ScreenRecord As System.Windows.Forms.GroupBox
    Friend WithEvents lbl_Bitrate_2 As System.Windows.Forms.Label
    Friend WithEvents lbl_TimeVid_2 As System.Windows.Forms.Label
    Friend WithEvents lbl_x As System.Windows.Forms.Label
    Friend WithEvents txt_VideoBitrate As System.Windows.Forms.TextBox
    Friend WithEvents txt_VideoTime As System.Windows.Forms.TextBox
    Friend WithEvents txt_VidResH As System.Windows.Forms.TextBox
    Friend WithEvents txt_VidResW As System.Windows.Forms.TextBox
    Friend WithEvents lbl_TimeVid As System.Windows.Forms.Label
    Friend WithEvents lbl_Bitrate As System.Windows.Forms.Label
    Friend WithEvents lbl_VidRes As System.Windows.Forms.Label
    Friend WithEvents btn_RecordVideo As System.Windows.Forms.Button
    Friend WithEvents pb_RecordVideo As System.Windows.Forms.ProgressBar
    Friend WithEvents lbl_RecordProg As System.Windows.Forms.Label
    Friend WithEvents Timer1 As System.Windows.Forms.Timer
    Friend WithEvents gb_Dialer As System.Windows.Forms.GroupBox
    Friend WithEvents lbl_inputPhone As System.Windows.Forms.Label
    Friend WithEvents btn_Call As System.Windows.Forms.Button
    Friend WithEvents txt_Dialer As System.Windows.Forms.TextBox
    Friend WithEvents cb_ShowTouches As System.Windows.Forms.CheckBox
    Friend WithEvents btn_DeviceList As System.Windows.Forms.Button
    Friend WithEvents btn_GetSerialNumber As System.Windows.Forms.Button
    Friend WithEvents tb_ADBSettings As System.Windows.Forms.TabPage
    Friend WithEvents gb_ADBSettings As System.Windows.Forms.GroupBox
    Friend WithEvents gb_ADBRestart As System.Windows.Forms.GroupBox
    Friend WithEvents lbl_tcport As System.Windows.Forms.Label
    Friend WithEvents txt_tcport As System.Windows.Forms.TextBox
    Friend WithEvents btn_adbTCPIP As System.Windows.Forms.Button
    Friend WithEvents btn_adbUSB As System.Windows.Forms.Button
    Friend WithEvents btn_AdbUnroot As System.Windows.Forms.Button
    Friend WithEvents btn_adbRoot As System.Windows.Forms.Button
    Friend WithEvents gb_ADBWifi As System.Windows.Forms.GroupBox
    Friend WithEvents lbl_HowToIP As System.Windows.Forms.Label
    Friend WithEvents lbl_defPort As System.Windows.Forms.Label
    Friend WithEvents btn_connectwifi As System.Windows.Forms.Button
    Friend WithEvents lbl_DevIP As System.Windows.Forms.Label
    Friend WithEvents lbl_port As System.Windows.Forms.Label
    Friend WithEvents txt_ipaddr As System.Windows.Forms.TextBox
    Friend WithEvents txt_tcpip As System.Windows.Forms.TextBox
    Friend WithEvents lbl_Warn1 As System.Windows.Forms.Label
    Friend WithEvents btn_DisableVerity As System.Windows.Forms.Button
    Friend WithEvents btn_EnableVerity As System.Windows.Forms.Button
    Friend WithEvents btn_ADBKeygen As System.Windows.Forms.Button
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents lbl_warn2 As System.Windows.Forms.Label
    Friend WithEvents lbl_warn3 As System.Windows.Forms.Label
    Friend WithEvents btn_jwdp As System.Windows.Forms.Button
    Friend WithEvents tc_Extra As System.Windows.Forms.TabControl
    Friend WithEvents tb_Misc As System.Windows.Forms.TabPage
    Friend WithEvents tb_InputKeyEvent As System.Windows.Forms.TabPage
    Friend WithEvents txt_InputKey As System.Windows.Forms.TextBox
    Friend WithEvents lbl_KeyInputTextWarn As System.Windows.Forms.Label
    Friend WithEvents lbl_InputKey As System.Windows.Forms.Label
    Friend WithEvents btn_KeyInputText As System.Windows.Forms.Button
    Friend WithEvents tb_KeyEvent As System.Windows.Forms.TabPage
    Friend WithEvents btn_SwipeSend As System.Windows.Forms.Button
    Friend WithEvents lbl_SwipeDuration As System.Windows.Forms.Label
    Friend WithEvents txt_SwipeDuration As System.Windows.Forms.TextBox
    Friend WithEvents lbl_SwipeY2 As System.Windows.Forms.Label
    Friend WithEvents txt_SwipeY2 As System.Windows.Forms.TextBox
    Friend WithEvents lbl_SwipeY1 As System.Windows.Forms.Label
    Friend WithEvents txt_SwipeY1 As System.Windows.Forms.TextBox
    Friend WithEvents lbl_SwipeX2 As System.Windows.Forms.Label
    Friend WithEvents txt_SwipeX2 As System.Windows.Forms.TextBox
    Friend WithEvents lbl_SwipeX1 As System.Windows.Forms.Label
    Friend WithEvents txt_SwipeX1 As System.Windows.Forms.TextBox
    Friend WithEvents gb_ToogleRoot As System.Windows.Forms.GroupBox
    Friend WithEvents btn_ToogleRoot As System.Windows.Forms.Button
    Friend WithEvents cb_ToogleRoot As System.Windows.Forms.ComboBox
    Friend WithEvents tb_About As System.Windows.Forms.TabPage
    Friend WithEvents gb_About As System.Windows.Forms.GroupBox
    Friend WithEvents lbl_About As System.Windows.Forms.Label
    Friend WithEvents lbl_disclaimer As System.Windows.Forms.Label
    Friend WithEvents txtbox_License As System.Windows.Forms.TextBox
    Friend WithEvents gb_Thanks As System.Windows.Forms.GroupBox
    Friend WithEvents lbl_Thanks As System.Windows.Forms.Label
    Friend WithEvents ll_Email As System.Windows.Forms.LinkLabel
    Friend WithEvents rb_FilterAppAll As System.Windows.Forms.RadioButton
    Friend WithEvents rb_FilterSystemApp As System.Windows.Forms.RadioButton
    Friend WithEvents rb_FilterAppUser As System.Windows.Forms.RadioButton
    Friend WithEvents btn_KeyboardInput As System.Windows.Forms.Button
    Friend WithEvents lbl_Header As System.Windows.Forms.Label
    Friend WithEvents tb_buildprop As System.Windows.Forms.TabPage
    Friend WithEvents gb_buildprop As System.Windows.Forms.GroupBox
    Friend WithEvents btn_SaveBuildProp As System.Windows.Forms.Button
    Friend WithEvents btn_LoadBuildProp As System.Windows.Forms.Button
    Friend WithEvents btn_BackupBuildProp As System.Windows.Forms.Button
    Friend WithEvents lbl_buildprophow As System.Windows.Forms.Label
    Friend WithEvents tb_prefNetwork As System.Windows.Forms.TabPage
    Friend WithEvents lbl_network2 As System.Windows.Forms.Label
    Friend WithEvents btn_prefnetwork As System.Windows.Forms.Button
    Friend WithEvents cb_prefnetwork As System.Windows.Forms.ComboBox
    Friend WithEvents lbl_network1 As System.Windows.Forms.Label
    Friend WithEvents txt_buildprop As ICSharpCode.TextEditor.TextEditorControl
    Friend WithEvents lbl_BuildPropStats As System.Windows.Forms.Label
    Friend WithEvents ll_Update As System.Windows.Forms.LinkLabel
    Friend WithEvents ll_Github As System.Windows.Forms.LinkLabel

End Class
