func repeat<ItemType>(item: ItemType, times: Int) -> ItemType[] {
    var result = ItemType[]()
    for i in 0..times {
         result += item
    }
    return result
}
repeat("knock", 4)
