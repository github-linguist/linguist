import { hilog } from '@kit.PerformanceAnalysisKit'

@Entry
@Component
struct Index {
  @State currentTime: string = ''
  @State isConnected: boolean = false
  @State systemInfo: SystemInfo | null = null

  aboutToAppear() {
    this.updateTime()
    this.checkNetworkStatus()
    this.getSystemInfo()
    
    setInterval(() => {
      this.updateTime()
    }, 1000)
  }

  build() {
    Column() {
      // 顶部状态栏
      this.buildStatusBar()
      
      // 主要内容区域
      this.buildMainContent()
      
      // 底部操作区域
      this.buildActionArea()
    }
    .width('100%')
    .height('100%')
    .backgroundColor('#F5F5F5')
    .padding(16)
  }

  @Builder
  buildStatusBar() {
    Row() {
      Text('My App')
        .fontSize(18)
        .fontWeight(FontWeight.Bold)
        .fontColor('#333333')
      
      Blank()
      
      Row() {
        Image("icon.png")
          .width(16)
          .height(16)
          .fillColor(this.isConnected ? '#00C851' : '#FF4444')
        
        Text(this.isConnected ? 'Connected' : 'Disconnected')
          .fontSize(12)
          .fontColor(this.isConnected ? '#00C851' : '#FF4444')
          .margin({ left: 4 })
      }
    }
    .width('100%')
    .padding({ bottom: 16 })
  }

  @Builder
  buildMainContent() {
    Column() {
      // 时间显示
      Text(this.currentTime)
        .fontSize(32)
        .fontWeight(FontWeight.Bold)
        .fontColor('#007AFF')
        .margin({ bottom: 24 })
      
      // 系统信息卡片
      if (this.systemInfo) {
        this.buildSystemInfoCard()
      }
      
      // 功能按钮区域
      this.buildFeatureButtons()
    }
    .width('100%')
    .layoutWeight(1)
    .justifyContent(FlexAlign.Center)
  }

  @Builder
  buildSystemInfoCard() {
    Column() {
      Text('System information')
        .fontSize(16)
        .fontWeight(FontWeight.Medium)
        .margin({ bottom: 12 })
      
      Row() {
        Text('Device model:')
          .fontSize(14)
          .fontColor('#666666')
        
        Text(this.systemInfo?.deviceModel || 'Unknown')
          .fontSize(14)
          .fontColor('#333333')
          .margin({ left: 8 })
      }
      .width('100%')
      .justifyContent(FlexAlign.SpaceBetween)
      .margin({ bottom: 8 })
      
      Row() {
        Text('System version:')
          .fontSize(14)
          .fontColor('#666666')
        
        Text(this.systemInfo?.osVersion || 'Unknown')
          .fontSize(14)
          .fontColor('#333333')
          .margin({ left: 8 })
      }
      .width('100%')
      .justifyContent(FlexAlign.SpaceBetween)
    }
    .width('100%')
    .backgroundColor(Color.White)
    .borderRadius(12)
    .padding(16)
    .margin({ bottom: 24 })
  }

  @Builder
  buildFeatureButtons() {
    Row() {
      Button('Refresh status')
        .onClick(() => {
          this.checkNetworkStatus()
          this.getSystemInfo()
        })
        .backgroundColor('#007AFF')
        .borderRadius(8)
        .layoutWeight(1)
        .margin({ right: 8 })
      
      Button('Settings')
        .onClick(() => {
          hilog.info(0x0000, 'testTag', 'Navigate to settings...')
        })
        .backgroundColor(Color.Transparent)
        .border({ width: 1, color: '#007AFF' })
        .borderRadius(8)
        .layoutWeight(1)
    }
    .width('100%')
  }

  @Builder
  buildActionArea() {
    Row() {
      Text('My footer')
        .fontSize(12)
        .fontColor('#999999')
    }
    .width('100%')
    .justifyContent(FlexAlign.Center)
  }

  private updateTime() {
    const now = new Date()
    this.currentTime = now.toLocaleTimeString('zh-CN', {
      hour12: false,
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit'
    })
  }

  private checkNetworkStatus() {
    this.isConnected = Math.random() > 0.3
  }

  private getSystemInfo() {
    this.systemInfo = {
      deviceModel: 'HarmonyOS Device',
      osVersion: 'HarmonyOS 6.0.0',
      apiLevel: 20
    }
  }
}

interface SystemInfo {
  deviceModel: string
  osVersion: string
  apiLevel: number
}
