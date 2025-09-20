@Entry
@Component
struct Index {
  @State count: number = 0

  build() {
    Column() {
      Text(`Count: ${this.count}`)
        .fontSize(30)
        .margin(20)
      
      Button('Increment')
        .onClick(() => {
          this.count++
        })
        .margin(10)
      
      Button('Decrement')
        .onClick(() => {
          this.count--
        })
        .margin(10)
    }
    .width('100%')
    .height('100%')
    .justifyContent(FlexAlign.Center)
  }
}
