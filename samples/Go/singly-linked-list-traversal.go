start := &Ele{"tacos", nil}
end := start.Append("burritos")
end = end.Append("fajitas")
end = end.Append("enchilatas")
for iter := start; iter != nil; iter = iter.Next {
    fmt.Println(iter)
}
