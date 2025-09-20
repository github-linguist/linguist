// ArkTS class example with decorators
@Observed
class User {
  name: string
  age: number
  email: string

  constructor(name: string, age: number, email: string) {
    this.name = name
    this.age = age
    this.email = email
  }

  getInfo(): string {
    return `${this.name} (${this.age}) - ${this.email}`
  }
}

@Entry
@Component
struct UserProfile {
  @State user: User = new User('John Doe', 30, 'john@example.com')

  build() {
    Column() {
      Text(this.user.getInfo())
        .fontSize(20)
        .margin(20)
      
      Button('Update Age')
        .onClick(() => {
          this.user.age++
        })
        .margin(10)
    }
    .width('100%')
    .height('100%')
    .justifyContent(FlexAlign.Center)
  }
}
